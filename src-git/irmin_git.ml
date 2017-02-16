(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Metadata = struct
  module X = struct
    type t = [`Normal | `Exec | `Link]
    let t = Irmin.Type.enum "metadata" [
        ("normal", `Normal);
        ("exec"  , `Exec);
        ("link"  , `Link);
      ]
  end
  include X
  let default = `Normal
  let merge = Irmin.Merge.default X.t
end

let src = Logs.Src.create "irmin.git" ~doc:"Irmin Git-format store"
module Log = (val Logs.src_log src : Logs.LOG)

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

module Conf = struct

  let root = Irmin.Private.Conf.root

  let reference =
    let parse str = Ok (Git.Reference.of_raw str) in
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

let config ?(config=Irmin.Private.Conf.empty) ?head ?bare ?level root =
  let module C = Irmin.Private.Conf in
  let config = C.add config Conf.root (Some root) in
  let config = match bare with
    | None   -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  let config = C.add config Conf.level level in
  config

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
  let digest_size = 20 (* FIXME: expose Git.Hash.digest_size *)
  let t = Irmin.Type.(like string) SHA_IO.of_hex Git.Hash.to_hex
  let digest = G.Digest.cstruct
  let to_raw t = Cstruct.of_string (Git.Hash.to_raw t)
  let of_raw t = Git.Hash.of_raw (Cstruct.to_string t)
  let has_kind = function `SHA1 -> true | _ -> false
  let pp ppf x = Fmt.string ppf (Git.Hash.to_hex x)
  let of_string str =
    try Ok (SHA_IO.of_hex str)
    with Git.Hash.Ambiguous s -> Error (`Msg ("ambiguous " ^ s))
end

module Irmin_value_store
    (G: VALUE_STORE)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (H: Irmin.Hash.S)   (* Why do we need both H and G.Digest? *)
= struct

  let () =
    if not (H.has_kind `SHA1) then
      failwith "The Git backend only support SHA1 hashes."

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

    let find t key =
      let key = git_of_key key in
      G.read t key >|= function
      | None   -> None
      | Some v -> V.of_git v

    let add t v =
      G.write t (V.to_git v) >|=
      key_of_git

  end

  module GitContents = struct
    type t = C.t
    let to_string = Fmt.to_to_string C.pp
    let type_eq = function Git.Object_type.Blob -> true | _ -> false

    let of_string str = match C.of_string str with
      | Ok x           -> Some x
      | Error (`Msg e) ->
        Log.err (fun l -> l "Git.Contents: cannot parse %S: %s" str e);
        None

    let of_git = function
      | Git.Value.Blob b -> of_string (Git.Blob.to_raw b)
      | _                -> None

    let to_git b = Git.Value.Blob (Git.Blob.of_raw (to_string b))
  end
  module XContents = struct
    include AO (GitContents)
    module Val = C
    module Key = H
  end
  module Contents = Irmin.Contents.Store(XContents)

  module XNode = struct
    module Key = H
    module Path = P

    module Val = struct
      module Metadata = Metadata

      type t = Git.Tree.t
      type metadata = Metadata.t
      type contents = Contents.key
      type node = Key.t
      type step = Path.step
      type value = [`Node of node | `Contents of contents * metadata ]
      let metadata_t = Metadata.t
      let contents_t = Contents.Key.t
      let node_t = Key.t
      let step_t = Path.step_t

      let value_t =
        let open Irmin.Type in
        let contents_t = pair contents_t metadata_t in
        variant "Tree.value" (fun node contents -> function
            | `Node n     -> node n
            | `Contents c -> contents c)
        |~ case1 "node"     node_t     (fun n -> `Node n)
        |~ case1 "contents" contents_t (fun c -> `Contents c)
        |> sealv

      let of_step = Fmt.to_to_string P.pp_step

      let to_step str = match P.step_of_string str with
        | Ok x           -> x
        | Error (`Msg e) -> failwith e

      let to_git perm (name, node) =
        { Git.Tree.perm; name = of_step name; node }

      let list t  =
        List.fold_left (fun acc { Git.Tree.perm; name; node } ->
            let name = to_step name in
            match perm with
            | `Dir             -> (name, `Node (key_of_git node)) :: acc
            | `Commit          -> acc (* FIXME *)
            | #Metadata.t as p -> (name, `Contents (key_of_git node, p)) :: acc
          ) [] t
        |> List.rev

      let find t s =
        let s = of_step s in
        let rec aux = function
          | [] -> None
          | x::xs when x.Git.Tree.name <> s -> aux xs
          | { Git.Tree.perm; node; _ } :: _ ->
            match perm with
            | `Dir             -> Some (`Node (key_of_git node))
            | `Commit          -> None (* FIXME *)
            | #Metadata.t as p -> Some (`Contents (key_of_git node, p))
        in
        aux t

      type compare_result = LT | EQ | GT

      module Sort_key: sig
        type t
        val of_entry: Git.Tree.entry -> t
        val of_contents: string -> t
        val of_node: string -> t
        val order: t -> t -> compare_result
        val compare: t -> t -> int
      end = struct

        type t =
          | Contents: string -> t
          | Node    : string -> t

        exception Result of int

        let str = function Contents s | Node s -> s

        let compare x y = match x, y with
          | Contents x, Contents y -> String.compare x y
          | _  ->
            let xs = str x and ys = str y in
            let lenx = String.length xs in
            let leny = String.length ys in
            let i = ref 0 in
            try
              while !i < lenx && !i < leny do
                match
                  Char.compare
                    (String.unsafe_get xs !i) (String.unsafe_get ys !i)
                with
                | 0 -> incr i
                | i -> raise (Result i)
              done;
              let get len s i =
                if i < len then String.unsafe_get (str s) i
                else if i = len then match s with
                  | Node _     -> '/'
                  | Contents _ -> '\000'
                else '\000'
              in
              match Char.compare (get lenx x !i) (get leny y !i) with
              | 0 -> Char.compare (get lenx x (!i + 1)) (get leny y (!i + 1))
              | i -> i
            with Result i ->
              i

        let order a b = match compare a b with
          | 0 -> EQ
          | x when x > 0 -> GT
          | _ -> LT

        let of_contents c = Contents c
        let of_node n = Node n

        let of_entry = function
          | {Git.Tree.name = n; perm = `Dir; _} -> of_node n
          | {Git.Tree.name = n; _} -> of_contents n
      end

      let compare_entries a b =
        Sort_key.(compare (of_entry a) (of_entry b))

      (* the order is always:

         [ ...; foo (content key); ...; foo/ (node key);  ... ]

         So always scan until the 'node' key.
      *)

      let remove t step =
        let step = of_step step in
        let node_key = Sort_key.of_node step in
        let contents_key = Sort_key.of_contents step in
        let return ~acc rest = List.rev_append acc rest in
        let rec aux acc = function
          | []     -> t
          | h :: l ->
            let entry_key = Sort_key.of_entry h in
            if Sort_key.order contents_key entry_key = EQ then
              return ~acc l
            else match Sort_key.order node_key entry_key with
              | GT -> aux (h :: acc) l
              | EQ -> return ~acc l
              | LT -> t
        in
        aux [] t

      let git_of_v = function `Contents (x, _) | `Node x -> git_of_key x

      let update t step v =
        let step = of_step step in
        let node_key = Sort_key.of_node step in
        let contents_key = Sort_key.of_contents step in
        let return ~acc rest =
          let perm, v = match v with
          | `Node n          -> `Dir, n
          | `Contents (c, m) -> (m :> Git.Tree.perm), c
          in
          let e = { Git.Tree.perm; name = step; node = git_of_key v} in
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
            else match Sort_key.order node_key entry_key with
              | GT -> aux (h :: acc) l
              | LT -> return ~acc (h::l)
              | EQ when Git.Hash.equal (git_of_v v) node -> t
              | EQ -> return ~acc l
        in
        aux [] t

      let empty = []

      let is_empty = function
        | [] -> true
        | _  -> false

      let v alist =
        let alist = List.map (fun (l, x) ->
            let v k = l, git_of_key k in
            match x with
            | `Node n             -> to_git `Dir (v n)
            | `Contents (c, perm) -> to_git (perm :> Git.Tree.perm) (v c)
          ) alist
        in
        List.fast_sort compare_entries alist

       let alist t =
         let mk_n k = `Node (key_of_git k) in
         let mk_c k metadata = `Contents (key_of_git k, metadata) in
         List.map (function
             | { Git.Tree.perm = `Dir; name; node } -> (to_step name, mk_n node)
             | { Git.Tree.perm = `Commit; _ } -> assert false
             | { Git.Tree.perm = #Metadata.t as perm; name; node; _ } ->
               (to_step name, mk_c node perm)
          ) t

       module N = Irmin.Private.Node.Make (H)(H)(P)(Metadata)
       let to_n t = N.v (alist t)
       let of_n n = v (N.list n)
       let t = Irmin.Type.like N.t of_n to_n
    end

    include AO (struct
        type t = Val.t
        let type_eq = function Git.Object_type.Tree -> true | _ -> false
        let to_git t = Git.Value.Tree t
        let of_git = function Git.Value.Tree t -> Some t | _ -> None
      end)
  end
  module Node = Irmin.Private.Node.Store(Contents)(P)(Metadata)(XNode)

  module XCommit = struct
    module Val = struct
      type t = Git.Commit.t
      type commit = H.t
      type node = H.t

      let commit_t = H.t
      let node_t = H.t

      let commit_key_of_git k =
        H.of_raw (Git_hash.to_raw (Git.Hash.of_commit k))
      let git_of_commit_key k =
        Git.Hash.to_commit (Git_hash.of_raw (H.to_raw k))

      let node_key_of_git k =
        H.of_raw (Git_hash.to_raw (Git.Hash.of_tree k))
      let git_of_node_key k =
        Git.Hash.to_tree (Git_hash.of_raw (H.to_raw k))

      let info_of_git author message =
        let id = author.Git.User.name in
        let date, _ = author.Git.User.date in (* FIXME: tz offset is ignored *)
        Irmin.Info.v ~date ~owner:id message

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

      let of_git g =
        let { Git.Commit.tree; parents; author; message; _ } = g in
        let parents = List.map commit_key_of_git parents in
        let node = node_key_of_git tree in
        let info = info_of_git author message in
        (info, node, parents)

      let to_git info node parents =
        let tree = git_of_node_key node in
        let parents = List.rev_map git_of_commit_key parents in
        let parents = List.fast_sort Git.Hash.Commit.compare parents in
        let author =
          let date = Irmin.Info.date info in
          let name, email = name_email (Irmin.Info.owner info) in
          Git.User.({ name; email;
                      date  = date, None;
                    }) in
        let message = Irmin.Info.message info in
        { Git.Commit.tree; parents; author; committer = author; message }

      let v ~info ~node ~parents = to_git info node parents
      let xnode { Git.Commit.tree; _ } = node_key_of_git tree
      let node t = xnode t
      let parents { Git.Commit.parents; _ } = List.map commit_key_of_git parents
      let info { Git.Commit.author; message; _ } = info_of_git author message

      module C = Irmin.Private.Commit.Make(H)(H)

      let of_c c = to_git (C.info c) (C.node c) (C.parents c)

      let to_c t =
        let info, node, parents = of_git t in
        C.v ~info ~node ~parents

      let t = Irmin.Type.like C.t of_c to_c
    end

    module Key = H

    include AO (struct
        type t = Val.t
        let type_eq = function Git.Object_type.Commit -> true | _ -> false
        let of_git = function Git.Value.Commit c -> Some c | _ -> None
        let to_git c = Git.Value.Commit c
      end)

  end
  module Commit = Irmin.Private.Commit.Store(Node)(XCommit)

end

module Irmin_branch_store
    (G: Git.Store.S) (B: Irmin.Branch.S) (H: Irmin.Hash.S) =
struct

  module Key = B
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

  let watches = Hashtbl.create 10

  type key = Key.t
  type value = Val.t
  type watch = W.watch * (unit -> unit Lwt.t)

  let branch_of_git r =
    let str = String.trim @@ Git.Reference.to_raw r in
    match string_chop_prefix ~prefix:("refs/heads/") str with
    | None   -> None
    | Some r -> match Key.of_string r with
      | Ok x           -> Some x
      | Error (`Msg e) ->
        Log.err (fun l -> l "invalid branch name: %s" e);
        None

  let git_of_branch_str str = Git.Reference.of_raw ("refs/heads/" ^ str)

  let git_of_branch r = git_of_branch_str (Fmt.to_to_string Key.pp r)
  let commit_of_git k = Val.of_raw (Git_hash.to_raw k)
  let git_of_commit k = Git_hash.of_raw (Val.to_raw k)

  let mem { t; _ } r = G.mem_reference t (git_of_branch r)

  let find { t; _ } r =
    G.read_reference t (git_of_branch r) >|= function
    | None   -> None
    | Some k -> Some (commit_of_git k)

  let listen_dir t =
    let (/) = Filename.concat in
    if G.kind = `Disk then
      let dir = t.git_root / "refs" / "heads" in
      let key file = match Key.of_string file with
        | Ok x           -> Some x
        | Error (`Msg e) ->
          Log.err (fun l -> l "listen: file %s: %s" file e);
          None
      in
      W.listen_dir t.w dir ~key ~value:(find t)
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
    stop () >>= fun () ->
    W.unwatch t.w w

  let v t ~head ~bare =
    let git_root = Filename.concat (G.root t) ".git" in
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
        | None      -> write_head (git_of_branch B.master)
    end >|= fun git_head ->
    let w =
      try Hashtbl.find watches (G.root t)
      with Not_found ->
        let w = W.v () in
        (* FIXME: we might want to use a weak table *)
        Hashtbl.add watches (G.root t) w;
        w
    in
    { git_head; bare; t; w; git_root }

  let list { t; _ } =
    G.references t >|= fun refs ->
    List.fold_left (fun acc r ->
        match branch_of_git r with
        | None   -> acc
        | Some r -> r :: acc
      ) [] refs

  let write_index t gr gk =
    if G.kind = `Disk then (
      Log.debug (fun f -> f "write_index");
      let git_head = Git.Reference.Ref gr in
      Log.debug (fun f -> f "write_index/if bare=%b head=%a" t.bare Git.Reference.pp gr);
      if not t.bare && git_head = t.git_head then (
        Log.debug (fun f -> f "write cache (%a)" Git.Reference.pp gr);
        G.write_index t.t gk
      ) else
        Lwt.return_unit
    ) else
      Lwt.return_unit

  let set t r k =
    Log.debug (fun f -> f "update %a" B.pp r);
    let gr = git_of_branch r in
    let gk = git_of_commit k in
    G.write_reference t.t gr gk >>= fun () ->
    W.notify t.w r (Some k) >>= fun () ->
    write_index t gr (Git.Hash.to_commit gk)

  let remove t r =
    Log.debug (fun f -> f "remove %a" B.pp r);
    G.remove_reference t.t (git_of_branch r) >>= fun () ->
    W.notify t.w r None

  let test_and_set t r ~test ~set =
    Log.debug (fun f -> f "test_and_set");
    let gr = git_of_branch r in
    let c = function None -> None | Some h -> Some (git_of_commit h) in
    G.test_and_set_reference t.t gr ~test:(c test) ~set:(c set) >>= fun b ->
    (if b then W.notify t.w r set else Lwt.return_unit) >>= fun () ->
    begin
      (* We do not protect [write_index] because it can take a log
         time and we don't want to hold the lock for too long. Would
         be safer to grab a lock, although the expanded filesystem
         is not critical for Irmin consistency (it's only a
         convenience for the user). *)
      if b then match set with
        | None   -> Lwt.return_unit
        | Some v -> write_index t gr (Git.Hash.to_commit (git_of_commit v))
      else
        Lwt.return_unit
    end >|= fun () ->
    b

end

module Irmin_sync_store
    (Ctx: CONTEXT) (IO: Git.Sync.IO with type ctx = Ctx.t)
    (G: Git.Store.S)
    (B: Irmin.Branch.S) (H: Irmin.Hash.S) =
struct

  (* FIXME: should not need to pass G.Digest and G.Inflate... *)
  module Sync = Git.Sync.Make(IO)(G)
  module Git_hash = Hash(G)

  type t = Sync.t
  type commit = H.t
  type branch = B.t

  let git_of_branch_str str = Git.Reference.of_raw ("refs/heads/" ^ str)
  let git_of_branch r = git_of_branch_str (Fmt.to_to_string B.pp r)
  let commit_of_git key = H.of_raw (Git_hash.to_raw key)

  let o_head_of_git = function
    | None   -> Lwt.return (Error `No_head)
    | Some k -> Lwt.return (Ok (commit_of_git k))

  let fetch t ?depth ~uri br =
    Log.debug (fun f -> f "fetch %s" uri);
    let gri = Git.Gri.of_string uri in
    let deepen = depth in
    let result r =
      Log.debug (fun f -> f "fetch result: %a" Git.Sync.Result.pp_fetch r);
      let gr = git_of_branch br in
      let key =
        let refs = Git.Sync.Result.references r in
        try Some (Git.Reference.Map.find gr refs)
        with Not_found -> None
      in
      o_head_of_git key
    in
    Ctx.v () >>= fun ctx ->
    Sync.fetch t ?ctx ?deepen gri >>=
    result

  let push t ?depth:_ ~uri br =
    Log.debug (fun f -> f "push %s" uri);
    let branch = git_of_branch br in
    let gri = Git.Gri.of_string uri in
    let result r =
      Log.debug (fun f -> f "push result: %a" Git.Sync.Result.pp_push r);
      match r.Git.Sync.Result.result with
      | `Ok      -> Ok ()
      | `Error e -> Error (`Msg e)
    in
    Ctx.v () >>= fun ctx ->
    Sync.push t ?ctx ~branch gri >|=
    result

end

module Make_ext
    (Ctx: CONTEXT)
    (IO: Git.Sync.IO with type ctx = Ctx.t)
    (G: Git.Store.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S)
= struct

  module XBranch = Irmin_branch_store(G)(B)(H)

  type repo = {
    config: Irmin.config;
    g: G.t;
    b: XBranch.t;
  }

  module XSync = struct
    include Irmin_sync_store(Ctx)(IO)(G)(B)(H)
    let v repo = Lwt.return repo.g
  end

  module P = struct
    include Irmin_value_store(G)(C)(P)(H)
    module Branch = XBranch
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = XSync
    module Repo = struct
      type t = repo
      let branch_t t = t.b
      let contents_t t = t.g
      let node_t t = contents_t t, t.g
      let commit_t t = node_t t, t.g

      type config = {
        root : string option;
        level: int option;
        head : Git.Reference.t option;
        bare : bool;
      }

      let config c =
        let root = Irmin.Private.Conf.get c Conf.root in
        let level = Irmin.Private.Conf.get c Conf.level in
        let head = Irmin.Private.Conf.get c Conf.head in
        let bare = Irmin.Private.Conf.get c Conf.bare in
        { root; level; head; bare }

      let v conf =
        let { root; level; head; bare } = config conf in
        G.create ?root ?level () >>= fun g ->
        XBranch.v ~head ~bare g >|= fun b ->
        { g; b; config = conf }

    end

  end
  include Irmin.Make_ext(P)

  module Git = struct

    include G

    let git_commit repo h =
      let h =
        Commit.hash h
        |> Commit.Hash.to_raw
        |> Cstruct.to_string
        |> Git.Hash.of_raw
      in
      G.read repo.g h >|= function
      | Some Git.Value.Commit c -> Some c
      | _ -> None

    let of_repo r = r.g

    let to_repo ?head ?(bare=true) g =
      XBranch.v ~head ~bare g >|= fun b ->
      { config = Irmin.Private.Conf.empty; g; b }

  end

end

module Digest (H: Irmin.Hash.S): Git.Hash.DIGEST = struct
  (* FIXME: lots of allocations ... *)
  let cstruct buf =
    Git.Hash.of_raw (Cstruct.to_string (H.to_raw (H.digest buf)))
  let string str = cstruct (Cstruct.of_string str)
  let length = Cstruct.len @@ H.to_raw (H.digest (Cstruct.of_string ""))
end

module NoC (IO: Git.Sync.IO) = struct
  type t = IO.ctx
  let v () = Lwt.return None
end

module Make (IO: Git.Sync.IO) = Make_ext (NoC(IO))(IO)

module FS (IO: Git.Sync.IO) (I: Git.Inflate.S) (FS: Git.FS.IO)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) =
  Make (IO) (Git.FS.Make(FS)(Digest(H))(I)) (C) (P) (B) (H)

module Memory (IO: Git.Sync.IO) (I: Git.Inflate.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) = struct
  module Git_mem = Git.Memory.Make(Digest(H))(I)
  include Make (IO) (Git_mem) (C) (P) (B) (H)
end

module Memory_ext (Ctx: CONTEXT)
    (IO: Git.Sync.IO with type ctx = Ctx.t) (I: Git.Inflate.S)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) = struct
  module Git_mem = Git.Memory.Make(Digest(H))(I)
  include Make_ext (Ctx) (IO) (Git_mem) (C) (P) (B) (H)
end

module AO (G: Git.Store.S) (K: Irmin.Hash.S) (V: Irmin.Contents.Conv) = struct
  module V = struct
    include V
    let merge = Irmin.Merge.default Irmin.Type.(option V.t)
  end
  module P = Irmin.Path.String_list
  module M = Irmin_value_store (G)(V)(P)(K)
  include M.Contents
end

module RW (G: Git.Store.S) (K: Irmin.Branch.S) (V: Irmin.Hash.S) =
struct
  module K = struct
    include K
    let master = match K.of_string "master" with
      | Ok x           -> x
      | Error (`Msg e) -> failwith e
  end
  include Irmin_branch_store (G)(K)(V)
end

module type S = sig
  include Irmin.S
  module Git: sig
    include Git.Store.S
    val git_commit: Repo.t -> commit -> Git.Commit.t option Lwt.t
    val of_repo: Repo.t -> t
    val to_repo: ?head:Git.Reference.t -> ?bare:bool -> t -> Repo.t Lwt.t
  end
end

module type S_MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
  functor (H: Irmin.Hash.S) ->
    S with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and type metadata = Metadata.t
       and type Commit.Hash.t = H.t
       and type Tree.Hash.t = H.t
       and type Contents.Hash.t = H.t

module type S_mem = sig
  include S
  module Git_mem: sig
    val clear: ?root:string -> unit -> unit
    val clear_all: unit -> unit
  end
end

module type S_MAKER_mem =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
  functor (H: Irmin.Hash.S) ->
    S_mem with type key = P.t
           and type step = P.step
           and module Key = P
           and type contents = C.t
           and type branch = B.t
           and type metadata = Metadata.t
           and type Commit.Hash.t = H.t
           and type Tree.Hash.t = H.t
           and type Contents.Hash.t = H.t

include Conf
