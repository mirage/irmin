(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Printf

module Metadata = struct
  module X = struct
    type t = [`Normal | `Exec | `Link]
    let equal (a:t) (b:t) = (a = b)
    let compare = compare
    let to_hum = function
      | `Normal -> "Normal"
      | `Exec -> "Exec"
      | `Link -> "Link"
    let to_json t = `String (to_hum t)
    let of_json = function
      | `String "Normal" -> `Normal
      | `String "Exec" -> `Exec
      | `String "Link" -> `Link
      | j -> Ezjsonm.parse_error j "Not a valid Git file type"
    let of_hum s = of_json (`String s)
    let hash = function   (* Note: values also used for writing tag *)
      | `Normal -> 1
      | `Exec -> 2
      | `Link -> 3
    let read buf =
      match Ir_misc.untag buf with
      | 1 -> `Normal
      | 2 -> `Exec
      | 3 -> `Link
      | n -> Tc.Reader.error "Invalid Git file type %d" n
    let write t buf =
      Ir_misc.tag buf (hash t)
    let size_of _ = 1
  end
  include X
  let default = `Normal
  let merge = Irmin.Merge.default (module X)
end

let src = Logs.Src.create "irmin.git" ~doc:"Irmin Git-format store"
module Log = (val Logs.src_log src : Logs.LOG)

let (/) = Filename.concat

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

let write_string str b =
  let len = String.length str in
  Cstruct.blit_from_string str 0 b 0 len;
  Cstruct.shift b len

module Conf = struct

  let root = Irmin.Private.Conf.root

  let reference =
    let parse str = `Ok (Git.Reference.of_raw str) in
    let print ppf name = Fmt.string ppf (Git.Reference.to_raw name) in
    parse, print

  let head =
    Irmin.Private.Conf.key
      ~doc:"The main branch of the Git repository."
      "head" Irmin.Private.Conf.(some reference) None

  let bare =
    Irmin.Private.Conf.key
      ~doc:"Do not expand the filesystem on the disk."
      "bare" Irmin.Private.Conf.bool false

  let level =
    Irmin.Private.Conf.key
      ~doc:"The Zlib compression level."
      "level" Irmin.Private.Conf.(some int) None

end

let config ?(config=Irmin.Private.Conf.empty) ?root ?head ?bare ?level () =
  let module C = Irmin.Private.Conf in
  let config = C.add config Conf.root root in
  let config = match bare with
    | None   -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  let config = C.add config Conf.level level in
  config

module type LOCK = sig
  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module type CONTEXT = sig type t val v: unit -> t option Lwt.t end

module type VALUE_STORE = sig
  type t
  val read: t -> Git.Hash.t -> Git.Value.t option Lwt.t
  val mem: t -> Git.Hash.t -> bool Lwt.t
  val write: t -> Git.Value.t -> Git.Hash.t Lwt.t
  val contents: t -> (Git.Hash.t * Git.Value.t) list Lwt.t

  module Digest : Git.Hash.DIGEST
end

module Hash (G: VALUE_STORE) = struct
  module SHA_IO = Git.Hash.IO(G.Digest)
  type t = Git.Hash.t
  let hash = Git.Hash.hash
  let compare = Git.Hash.compare
  let equal = (=)
  let digest_size = 20 (* FIXME: expose Git.Hash.digest_size *)
  let to_json t = Ezjsonm.string (Git.Hash.to_hex t)
  let of_json j = SHA_IO.of_hex (Ezjsonm.get_string j)
  let size_of t = Tc.String.size_of (Git.Hash.to_raw t)
  let write t = Tc.String.write (Git.Hash.to_raw t)
  let read b = Git.Hash.of_raw (Tc.String.read b)
  let digest = G.Digest.cstruct
  let to_hum = Git.Hash.to_hex
  let of_hum = SHA_IO.of_hex
  let to_raw t = Cstruct.of_string (Git.Hash.to_raw t)
  let of_raw t = Git.Hash.of_raw (Cstruct.to_string t)
  let has_kind = function `SHA1 -> true | _ -> false
end

module Irmin_value_store
    (G: VALUE_STORE)
    (C: Irmin.Contents.S)
    (H: Irmin.Hash.S)   (* Why do we need both H and G.Digest? *)
= struct

  let () =
    if not (H.has_kind `SHA1) then
      failwith "The Git backend only support SHA1 hashes."

  module Tree_IO   = Git.Tree.IO(G.Digest)
  module Commit_IO = Git.Commit.IO(G.Digest)

  module type V = sig
    type t
    val type_eq: Git.Object_type.t -> bool
    val to_git: t -> Git.Value.t
    val of_git: Git.Value.t -> t option
  end

  module Git_hash = Hash(G)
  let git_of_key k = Git_hash.of_raw (H.to_raw k)
  let key_of_git k = H.of_raw (Git_hash.to_raw k)

  module AO (V: V) = struct

    type t = G.t
    type key = H.t
    type value = V.t

    let mem t key =
      let key = git_of_key key in
      G.mem t key >>= function
      | false    -> Lwt.return false
      | true     ->
        G.read t key >|= function
        | None   -> false
        | Some v -> V.type_eq (Git.Value.type_of v)

    let read t key =
      let key = git_of_key key in
      G.read t key >|= function
      | None   -> None
      | Some v -> V.of_git v

    let err_not_found n k =
      let str = sprintf "Irmin_git.%s: %s not found" n (H.to_hum k) in
      Lwt.fail (Invalid_argument str)

    let read_exn t key =
      read t key >>= function
      | None   -> err_not_found "read" key
      | Some v -> Lwt.return v

    let add t v =
      G.write t (V.to_git v) >|= fun k ->
      key_of_git k

    let iter t fn =
      G.contents t >>= fun contents ->
      Lwt_list.iter_s (fun (k, v) ->
          (* FIXME: ocaml-git needs to change to avoid reading the
             whole Git value on every iter. *)
          match V.of_git v with
          | None   -> Lwt.return_unit
          | Some v -> fn (key_of_git k) (fun () -> Lwt.return v)
        ) contents

  end

  module GitContents = struct
    type t = C.t
    let c: t Tc.t = (module C)
    let type_eq = function
      | Git.Object_type.Blob -> true
      | _ -> false
    let of_git = function
      | Git.Value.Blob b -> Some (Tc.read_string c (Git.Blob.to_raw b))
      | _                -> None
    let to_git b =
      Git.Value.Blob (Git.Blob.of_raw (Tc.write_string c b))
  end
  module XContents = struct
    include AO (GitContents)
    module Val = C
    module Key = H
  end
  module Contents = Irmin.Contents.Store(XContents)

  module XNode = struct
    module Key = H
    module Path = C.Path

    module Val = struct
      module Metadata = Metadata

      module S = C.Path.Step

      type t = Git.Tree.t
      type raw_contents = Contents.key
      type contents = Contents.key * Metadata.t
      type node = Key.t
      type step = Path.step

      let compare = Git.Tree.compare
      let equal = Git.Tree.equal
      let hash = Git.Tree.hash
      let read = Tree_IO.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Tree_IO.add buf t;
        Buffer.contents buf

      let write t b =
        write_string (to_string t) b

      let size_of t =
        (* XXX: eeerk: cause *a lot* of wwrite duplication!!  *)
        String.length (to_string t)

      let to_git perm (name, node) =
        { Git.Tree.perm; name = S.to_hum name; node }

      let iter_contents t fn =
        List.iter (function
          |  { Git.Tree.perm = #Metadata.t as perm; name; node } ->
              fn (S.of_hum name) (key_of_git node, perm)
          | _ -> ()
        ) t

      let iter_succ t fn =
        List.iter (fun { Git.Tree.perm; name; node } ->
            if perm = `Dir then fn (S.of_hum name) (key_of_git node)
          ) t

      let find t f s =
        let s = S.to_hum s in
        let rec aux = function
          | [] -> None
          | x::xs when x.Git.Tree.name <> s -> aux xs
          | x::xs ->
              match f x with
              | Some _ as r -> r
              | None -> aux xs in
        aux t

      type compare_result = LT | EQ | GT
      module Sort_key: sig
        type t
        val of_entry: Git.Tree.entry -> t
        val of_contents: string -> t
        val of_succ: string -> t
        val order: t -> t -> compare_result
        val compare: t -> t -> int
      end = struct
        type t = string

        let compare = String.compare

        let order a b =
          match compare a b with
            | 0 -> EQ
            | x when x > 0 -> GT
            | _ -> LT

        let of_contents n = n
        let of_succ n = n ^ "/"

        let of_entry = function
          | {Git.Tree.name = n; perm = `Dir; _} -> of_succ n
          | {Git.Tree.name = n; _} -> of_contents n
      end

      let compare_entries a b =
        Sort_key.(compare (of_entry a) (of_entry b))

      let with_succ t step succ =
        let step = S.to_hum step in
        let step_key = Sort_key.of_succ step in
        let contents_key = Sort_key.of_contents step in
        let return ~acc rest = match succ with
          | None   -> t
          | Some c ->
            let e = { Git.Tree.perm = `Dir; name = step; node = git_of_key c} in
            List.rev_append acc (e :: rest)
        in
        let rec aux acc = function
          | [] -> return ~acc []
          | { Git.Tree.node; _ } as h :: l ->
            let entry_key = Sort_key.of_entry h in
            (* Remove any contents entry with the same name. This will always
               come before the new succ entry. *)
            if Sort_key.order contents_key entry_key = EQ then
              aux acc l
            else match Sort_key.order step_key entry_key with
              | GT -> aux (h :: acc) l
              | LT -> return ~acc:acc (h::l)
              | EQ ->
                match succ with
                | None   -> List.rev_append acc l (* remove *)
                | Some c ->
                  if Git.Hash.equal (git_of_key c) node then t else return ~acc l
        in
        aux [] t

      let with_contents t step contents =
        let step = S.to_hum step in
        let step_key = Sort_key.of_contents step in
        let return ~acc rest = match contents with
          | None   -> t
          | Some (c, perm) ->
            let perm = (perm :> Git.Tree.perm) in
            let e =
              { Git.Tree.perm; name = step; node = git_of_key c}
            in
            List.rev_append acc (e :: rest)
        in
        (* After inserting a new contents entry, we need to continue searching to remove
           any succ with the same name. *)
        let without dir_key =
          List.filter (fun e -> Sort_key.order dir_key (Sort_key.of_entry e) <> EQ)
        in
        let rec aux acc entries =
          match entries with
          | [] -> return ~acc []
          | { Git.Tree.node; _ } as h :: l ->
            match Sort_key.order step_key (Sort_key.of_entry h) with
            | GT -> aux (h :: acc) l
            | LT -> return ~acc:acc (without (Sort_key.of_succ step) (h::l))
            | EQ ->
              match contents with
              | None   -> List.rev_append acc l (* remove *)
              | Some (c, _) ->
                if Git.Hash.equal (git_of_key c) node then t else return ~acc l
        in
        aux [] t

      let succ t s =
        find t (function
          | {Git.Tree.perm = `Dir; node; _} -> Some (key_of_git node)
          | _ -> None
        ) s

      let contents t s =
        find t (function
          | {Git.Tree.perm = #Metadata.t as perm; node; _} -> Some (key_of_git node, perm)
          | _ -> None
        ) s

      let empty = []

      let is_empty = function
        | [] -> true
        | _  -> false

      module N = Irmin.Private.Node.Make (H)(H)(C.Path)(Metadata)

      let alist t =
        let mk_n k = `Node (key_of_git k) in
        let mk_c k metadata = `Contents (key_of_git k, metadata) in
        List.map (function
            | { Git.Tree.perm = `Dir; name; node } -> (S.of_hum name, mk_n node)
            | { Git.Tree.perm = `Commit; _ } -> assert false
            | { Git.Tree.perm = #Metadata.t as perm; name; node; _ } -> (S.of_hum name, mk_c node perm)
          ) t

      let to_n t = N.create (alist t)

      let create alist =
        let alist = List.map (fun (l, x) ->
            match x with
            | `Contents (c, perm) -> to_git (perm :> Git.Tree.perm) (l, git_of_key c)
            | `Node n     -> to_git `Dir (l, git_of_key n)
          ) alist
        in
        List.fast_sort compare_entries alist

      let of_n n = create (N.alist n)
      let to_json t = N.to_json (to_n t)
      let of_json j = of_n (N.of_json j)

    end

    include AO (struct
        type t = Val.t
        let type_eq = function
          | Git.Object_type.Tree -> true
          | _ -> false
        let to_git t = Git.Value.Tree t
        let of_git = function
          | Git.Value.Tree t -> Some t
          | _ -> None
      end)
  end
  module Node = Irmin.Private.Node.Store(Contents)(XNode)

  module XCommit = struct
    module Val = struct
      type t = Git.Commit.t
      type commit = H.t
      type node = H.t

      let compare = Git.Commit.compare
      let equal = Git.Commit.equal
      let hash = Git.Commit.hash
      let read = Commit_IO.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Commit_IO.add buf t;
        Buffer.contents buf

      let write t b = write_string (to_string t) b
      let size_of t =
        (* XXX: yiiik, causes *a lot* of write duplciations *)
        String.length (to_string t)

      let commit_key_of_git k =
        H.of_raw (Git_hash.to_raw (Git.Hash.of_commit k))
      let git_of_commit_key k =
        Git.Hash.to_commit (Git_hash.of_raw (H.to_raw k))

      let node_key_of_git k =
        H.of_raw (Git_hash.to_raw (Git.Hash.of_tree k))
      let git_of_node_key k =
        Git.Hash.to_tree (Git_hash.of_raw (H.to_raw k))

      let task_of_git author message git =
        let id = author.Git.User.name in
        let date, _ = author.Git.User.date in
        let uid = Int64.of_int (Hashtbl.hash (author, message, git)) in
        Irmin.Task.create ~date ~owner:id ~uid message

      let of_git g =
        let { Git.Commit.tree; parents; author; message; _ } = g in
        let parents = List.map commit_key_of_git parents in
        let node = node_key_of_git tree in
        let task = task_of_git author message g in
        (task, node, parents)

      let name_email name =
        let name = String.trim name in
        try
          let i = String.rindex name ' ' in
          let email = String.sub name (i+1) (String.length name - i - 1) in
          if String.length email > 0
             && email.[0] = '<'
             && email.[String.length email - 1] = '>'
          then
            let email = String.sub email 1 (String.length email - 2) in
            let name = String.trim (String.sub name 0 i) in
            name, email
          else
            name, "irmin@openmirage.org"
        with Not_found ->
          name, "irmin@openmirage.org"

      let to_git task node parents =
        let tree = git_of_node_key node in
        let parents = List.map git_of_commit_key parents in
        let parents = List.fast_sort Git.Hash.Commit.compare parents in
        let author =
          let date = Irmin.Task.date task in
          let name, email = name_email (Irmin.Task.owner task) in
          Git.User.({ name; email;
                      date  = date, None;
                    }) in
        let message = String.concat "\n" (Irmin.Task.messages task) in
        { Git.Commit.tree; parents; author; committer = author; message }

      let create task ~node ~parents = to_git task node parents
      let xnode { Git.Commit.tree; _ } = node_key_of_git tree
      let node t = xnode t
      let parents { Git.Commit.parents; _ } = List.map commit_key_of_git parents
      let task g =
        let { Git.Commit.author; message; _ } = g in
        task_of_git author message g

      module C = Irmin.Private.Commit.Make(H)(H)

      let of_c c = to_git (C.task c) (C.node c) (C.parents c)

      let to_c t =
        let task, node, parents = of_git t in
        C.create task ~node ~parents

      let to_json t = C.to_json (to_c t)
      let of_json j = of_c (C.of_json j)

    end

    module Key = H

    include AO (struct
        type t = Val.t
        let type_eq = function
          | Git.Object_type.Commit -> true
          | _ -> false
        let of_git = function
          | Git.Value.Commit c -> Some c
          | _ -> None
        let to_git c = Git.Value.Commit c
      end)

  end
  module Commit = Irmin.Private.Commit.Store(Node)(XCommit)

end

module Irmin_ref_store
    (L: LOCK) (G: Git.Store.S) (R: Irmin.Ref.S) (H: Irmin.Hash.S) =
struct

  module Key = R
  module Val = H
  module Git_hash = Hash(G)

  module W = Irmin.Private.Watch.Make(Key)(Val)

  type t = {
    bare: bool;
    git_root: string;
    git_head: Git.Reference.head_contents;
    t: G.t;
    w: W.t;
  }

  type key = Key.t
  type value = Val.t
  type watch = W.watch * (unit -> unit Lwt.t)

  let tag_of_git r =
    let str = String.trim @@ Git.Reference.to_raw r in
    match string_chop_prefix ~prefix:("refs" / "heads" / "") str with
    | None   -> None
    | Some r -> Some (Key.of_hum r)

  let git_of_tag_string str =Git.Reference.of_raw ("refs" / "heads" / str)
  let git_of_tag r = git_of_tag_string (Key.to_hum r)
  let head_of_git k = Val.of_raw (Git_hash.to_raw k)
  let git_of_head k = Git_hash.of_raw (Val.to_raw k)

  let mem { t; _ } r =
    G.mem_reference t (git_of_tag r)

  let read { t; _ } r =
    G.read_reference t (git_of_tag r) >|= function
    | None   -> None
    | Some k -> Some (head_of_git k)

  let listen_dir t =
    if G.kind = `Disk then
      let dir = t.git_root / "refs" / "heads" in
      let key file = Some (Key.of_hum file) in
      W.listen_dir t.w dir ~key ~value:(read t)
    else
      Lwt.return (fun () -> Lwt.return_unit)

  let watch_key t key ?init f =
    listen_dir t >>= fun stop ->
    W.watch_key t.w key ?init f >|= fun w ->
    (w, stop)

  let watch t ?init f =
    listen_dir t >>= fun stop ->
    W.watch t.w ?init f >|= fun w ->
    (w, stop)

  let unwatch t (w, stop) =
    stop ();
    W.unwatch t.w w

  let create t ~head ~bare =
    let git_root = G.root t / ".git" in
    let write_head head =
      let head = Git.Reference.Ref head in
      begin
        if G.kind = `Disk then G.write_head t head
        else Lwt.return_unit
      end >|= fun () ->
      head
    in
    begin match head with
      | Some h -> write_head h
      | None   ->
        G.read_head t >>= function
        | Some head -> Lwt.return head
        | None      -> write_head (git_of_tag R.master)
    end >|= fun git_head ->
    let w = W.create () in
    { git_head; bare; t; w; git_root }

  let read_exn { t; _ } r =
    G.read_reference_exn t (git_of_tag r) >|= fun k ->
    head_of_git k

  let iter { t; _ } fn =
    G.references t >>= fun refs ->
    Lwt_list.iter_p (fun r ->
        let v () = G.read_reference_exn t r >|= head_of_git in
        match tag_of_git r with
        | None   -> Lwt.return_unit
        | Some r -> fn r v
      ) refs

  let write_index t gr gk =
    Log.debug (fun f -> f "write_index");
    if G.kind = `Disk then (
      let git_head = Git.Reference.Ref gr in
      Log.debug (fun f -> f "write_index/if bare=%b head=%a" t.bare Git.Reference.pp gr);
      if not t.bare && git_head = t.git_head then (
        Log.debug (fun f -> f "write cache (%a)" Git.Reference.pp gr);
        G.write_index t.t gk
      ) else
        Lwt.return_unit
    ) else
      Lwt.return_unit

  let lock_file t r = t.git_root / "lock" / Key.to_hum r

  let update t r k =
    Log.debug (fun f -> f "update %s" (Tc.show (module R) r));
    let gr = git_of_tag r in
    let gk = git_of_head k in
    let lock = lock_file t r in
    let write () = G.write_reference t.t gr gk in
    L.with_lock lock write >>= fun () ->
    W.notify t.w r (Some k) >>= fun () ->
    write_index t gr (Git.Hash.to_commit gk)

  let remove t r =
    Log.debug (fun f -> f "remove %s" (Tc.show (module R) r));
    let lock = lock_file t r in
    let remove () = G.remove_reference t.t (git_of_tag r) in
    L.with_lock lock remove >>= fun () ->
    W.notify t.w r None

  let compare_and_set t r ~test ~set =
    Log.debug (fun f -> f "compare_and_set");
    let gr = git_of_tag r in
    let lock = lock_file t r in
    L.with_lock lock (fun () ->
        read t r >>= fun v ->
        if Tc.O1.equal Val.equal v test then (
          let action () = match set with
            | None   -> G.remove_reference t.t gr
            | Some v -> G.write_reference t.t gr (git_of_head v)
          in
          action () >>= fun () ->
          Lwt.return true
        ) else
          Lwt.return false
      ) >>= fun updated ->
    (if updated then W.notify t.w r set else Lwt.return_unit) >>= fun () ->
    begin
      (* We do not protect [write_index] because it can took a log
         time and we don't want to hold the lock for too long. Would
         be safer to grab a lock, although the expanded filesystem
         is not critical for Irmin consistency (it's only a
         convenience for the user). *)
      if updated then match set with
        | None   -> Lwt.return_unit
        | Some v -> write_index t gr (Git.Hash.to_commit (git_of_head v))
      else
        Lwt.return_unit
    end >|= fun () ->
    updated

end

module Irmin_sync_store
    (Ctx: CONTEXT) (IO: Git.Sync.IO with type ctx = Ctx.t)
    (G: Git.Store.S)
    (R: Irmin.Ref.S) (H: Irmin.Hash.S) =
struct

  (* FIXME: should not need to pass G.Digest and G.Inflate... *)
  module Sync = Git.Sync.Make(IO)(G)
  module Git_hash = Hash(G)

  type t = Sync.t
  type commit_id = H.t
  type branch_id = R.t

  let git_of_tag_string str = Git.Reference.of_raw ("refs/heads" / str)
  let git_of_tag r = git_of_tag_string (R.to_hum r)

  let head_of_git key = H.of_raw (Git_hash.to_raw key)

  let o_head_of_git = function
    | None   -> Lwt.return `No_head
    | Some k -> Lwt.return (`Head (head_of_git k))

  let fetch t ?depth ~uri tag =
    Log.debug (fun f -> f "fetch %s" uri);
    let gri = Git.Gri.of_string uri in
    let deepen = depth in
    let result r =
      Log.debug (fun f -> f "fetch result: %a" Git.Sync.Result.pp_fetch r);
      let tag = git_of_tag tag in
      let key =
        let refs = Git.Sync.Result.references r in
        try Some (Git.Reference.Map.find tag refs)
        with Not_found -> None
      in
      o_head_of_git key
    in
    Ctx.v () >>= fun ctx ->
    Sync.fetch t ?ctx ?deepen gri >>=
    result

  let push t ?depth:_ ~uri tag =
    Log.debug (fun f -> f "push %s" uri);
    let branch = git_of_tag tag in
    let gri = Git.Gri.of_string uri in
    let result r =
      Log.debug (fun f -> f "push result: %a" Git.Sync.Result.pp_push r);
      match r.Git.Sync.Result.result with
      | `Ok      -> `Ok
      | `Error _ -> `Error
    in
    Ctx.v () >>= fun ctx ->
    Sync.push t ?ctx ~branch gri >|=
    result

end

module Make_ext
    (Ctx: CONTEXT) (IO: Git.Sync.IO with type ctx = Ctx.t)
    (L: LOCK) (G: Git.Store.S)
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S)
= struct
  module Git_store = struct
    include G
    let create config =
      let root = Irmin.Private.Conf.get config Conf.root in
      let level = Irmin.Private.Conf.get config Conf.level in
      G.create ?root ?level ()
  end

  module XRef = Irmin_ref_store(L)(G)(R)(H)

  type repo = {
    config: Irmin.config;
    g: Git_store.t;
    ref_store: XRef.t;
  }

  module XSync = struct
    include Irmin_sync_store(Ctx)(IO)(G)(R)(H)
    let create repo = Lwt.return repo.g
  end

  module P = struct
    include Irmin_value_store(Git_store)(C)(H)
    module Ref = XRef
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = XSync
    module Repo = struct
      type t = repo
      let ref_t t = t.ref_store
      let contents_t t = t.g
      let node_t t = contents_t t, t.g
      let commit_t t = node_t t, t.g

      let create config =
        let head = Irmin.Private.Conf.get config Conf.head in
        let bare = Irmin.Private.Conf.get config Conf.bare in
        Git_store.create config >>= fun g ->
        Ref.create ~head ~bare g >|= fun ref_store ->
        { g; ref_store; config }
    end
  end
  include Irmin.Make_ext(P)

  module Internals = struct
    let commit_of_id repo h =
      let h = Hash.to_raw h |> Cstruct.to_string |> Git.Hash.of_raw in
      Git_store.read repo.g h >|= function
      | Some Git.Value.Commit c -> Some c
      | _ -> None

  end

end

module NoL = struct
  let with_lock _ f = f ()
end

module Digest (H: Irmin.Hash.S): Git.Hash.DIGEST = struct
  (* FIXME: lots of allocations ... *)
  let cstruct buf =
    Git.Hash.of_raw (Cstruct.to_string (H.to_raw (H.digest buf)))
  let string str = cstruct (Cstruct.of_string str)
  let length = Cstruct.len @@ H.to_raw (H.digest (Cstruct.of_string ""))
end

module Memory_ext (Ctx: CONTEXT)
    (IO: Git.Sync.IO with type ctx = Ctx.t) (I: Git.Inflate.S)
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
  Make_ext (Ctx) (IO) (NoL) (Git.Memory.Make(Digest(H))(I)) (C) (R) (H)

module NoC (IO: Git.Sync.IO) = struct
  type t = IO.ctx
  let v () = Lwt.return None
end

module Make (IO: Git.Sync.IO) = Make_ext (NoC(IO))(IO)

module Memory (IO: Git.Sync.IO) (I: Git.Inflate.S)
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
  Make (IO) (NoL) (Git.Memory.Make(Digest(H))(I)) (C) (R) (H)

module FS (IO: Git.Sync.IO) (I: Git.Inflate.S) (L: LOCK) (FS: Git.FS.IO)
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
  Make (IO) (L) (Git.FS.Make(FS)(Digest(H))(I)) (C) (R) (H)

module AO (G: Git.Store.S) (K: Irmin.Hash.S) (V: Tc.S0) = struct
  module V = struct
    include V
    let merge _path ~old:_ _ _ = failwith "Irmin_git.AO.merge"
    module Path = Irmin.Path.String_list
  end
  module M = Irmin_value_store (G)(V)(K)
  include M.Contents
end

module RW (L: LOCK) (G: Git.Store.S) (K: Irmin.Ref.S) (V: Irmin.Hash.S) = struct
  module K = struct
    include K
    let master = K.of_hum "master"
  end
  include Irmin_ref_store (L)(G)(K)(V)
end

module type S = sig
  include Irmin.S
  module Internals: sig
    val commit_of_id: Repo.t -> commit_id -> Git.Commit.t option Lwt.t
  end
end

module type S_MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (R: Irmin.Ref.S) ->
  functor (H: Irmin.Hash.S) ->
    S with type key = C.Path.t
       and module Key = C.Path
       and type value = C.t
       and type branch_id = R.t
       and type commit_id = H.t
       and type Private.Node.Val.Metadata.t = Metadata.t

include Conf
