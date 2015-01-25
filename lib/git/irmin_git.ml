(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt

module Log = Log.Make(struct let section = "GIT" end)

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
  let print ppf name = Format.pp_print_string ppf (Git.Reference.to_raw name) in
  parse, print

  let head =
    Irmin.Private.Conf.key
      ~doc:"The main branch of the Git repository."
      "head" Irmin.Private.Conf.(some reference) None

  let bare =
    Irmin.Private.Conf.key
      ~doc:"Do not expand the filesystem on the disk."
      "bare" Irmin.Private.Conf.bool false

end

let config ?root ?head ?bare () =
  let module C = Irmin.Private.Conf in
  let config = C.empty in
  let config = C.add config Conf.root root in
  let config = match bare with
    | None   -> C.add config Conf.bare (C.default Conf.bare)
    | Some b -> C.add config Conf.bare b
  in
  let config = C.add config Conf.head head in
  config

module Make (IO: Git.Sync.IO) (G: Git.Store.S)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S)
= struct

  let () =
    if not (H.has_kind `SHA1) then
      failwith "The Git backend only support SHA1 hashes."

  module GK = struct
    type t = Git.SHA.t
    let hash = Git.SHA.hash
    let compare = Git.SHA.compare
    let equal = (=)
    let to_json t = Ezjsonm.string (Git.SHA.to_hex t)
    let of_json j = Git.SHA.of_hex (Ezjsonm.get_string j)
    let size_of _ = 20
    let write t = Tc.String.write (Git.SHA.to_raw t)
    let read b = Git.SHA.of_raw (Tc.String.read b)
    let digest = Git.SHA.of_cstruct
    let to_hum = Git.SHA.to_hex
    let of_hum = Git.SHA.of_hex
    let to_raw t = Cstruct.of_string (Git.SHA.to_raw t)
    let of_raw t = Git.SHA.of_raw (Cstruct.to_string t)
    let has_kind = function `SHA1 -> true | _ -> false
  end

  module type V = sig
    type t
    val type_eq: Git.Object_type.t -> bool
    val to_git: t -> Git.Value.t
    val of_git: Git.Value.t -> t option
  end

  module AO (K: Irmin.Hash.S) (V: V) = struct

    type t = {
      t: G.t;
      task: Irmin.task;
      config: Irmin.config;
    }

    type key = K.t

    type value = V.t

    let create config task =
      let root = Irmin.Private.Conf.get config Conf.root in
      G.create ?root () >>= fun t ->
      return (fun a -> { task = task a; config; t })

    let task t = t.task
    let git_of_key k = GK.of_raw (K.to_raw k)
    let key_of_git k = K.of_raw (GK.to_raw k)

    let mem { t; _ } key =
      let key = git_of_key key in
      G.mem t key >>= function
      | false    -> return false
      | true     ->
        G.read t key >>= function
        | None   -> return false
        | Some v -> return (V.type_eq (Git.Value.type_of v))

    let read { t; _ } key =
      let key = git_of_key key in
      G.read t key >>= function
      | None   -> return_none
      | Some v -> return (V.of_git v)

    let read_exn t key =
      read t key >>= function
      | None   -> fail Not_found
      | Some v -> return v

    let add { t; _ } v =
      G.write t (V.to_git v) >>= fun k ->
      return (key_of_git k)

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
    include AO (GK)(GitContents)
    module Val = C
    module Key = GK
  end

  module XNode = struct
    module Key = GK
    module Path = C.Path
    module Val = struct
      module S = C.Path.Step

      type t = Git.Tree.t
      type contents = GK.t
      type node = GK.t
      type step = Path.step

      let compare = Git.Tree.compare
      let equal = Git.Tree.equal
      let hash = Git.Tree.hash
      let read = Git.Tree.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Git.Tree.add buf t;
        Buffer.contents buf

      let write t b =
        write_string (to_string t) b

      let size_of t =
        (* XXX: eeerk: cause *a lot* of wwrite duplication!!  *)
        String.length (to_string t)

      let to_git perm (name, node) =
        { Git.Tree.perm; name = S.to_hum name; node }

      let iter_contents t fn =
        List.iter (fun { Git.Tree.perm; name; node } ->
            if perm <> `Dir then fn (S.of_hum name) node
          ) t

      let iter_succ t fn =
        List.iter (fun { Git.Tree.perm; name; node } ->
            if perm = `Dir then fn (S.of_hum name) node
          ) t

      let find t p s =
        let s = S.to_hum s in
        try
          List.find (fun { Git.Tree.perm; name; _ } -> p perm && name = s) t
          |> fun v -> Some v.Git.Tree.node
        with Not_found ->
          None

      let with_succ t step succ =
        let step = S.to_hum step in
        let rec aux acc = function
          | { Git.Tree.perm; name; node } as h :: l when perm = `Dir ->
            if name = step then match succ with
              | None   -> List.rev_append acc l
              | Some c ->
                if Git.SHA.equal c node then t
                else List.rev_append acc ({ h with Git.Tree.node = c } :: l)
            else aux (h :: acc) l
          | h::t -> aux (h :: acc) t
          | []   -> match succ with
            | None   -> t
            | Some c ->
              List.rev ({ Git.Tree.perm = `Dir; name = step; node = c} :: acc)
        in
        let new_t = aux [] t in
        if t == new_t then t else new_t

      let with_contents t step contents =
        let step = S.to_hum step in
        let rec aux acc = function
          | { Git.Tree.perm; name; node } as h :: l when perm <> `Dir ->
            if name = step then match contents with
              | None   -> List.rev_append acc l
              | Some c ->
                if Git.SHA.equal c node then t
                else List.rev_append acc ({ h with Git.Tree.node = c } :: l)
            else aux (h :: acc) l
          | h::t -> aux (h :: acc) t
          | []   -> match contents with
            | None   -> t
            | Some c ->
              List.rev ({ Git.Tree.perm = `Normal; name = step; node = c} :: acc)
        in
        let new_t = aux [] t in
        if t == new_t then t else new_t

      let succ t s = find t (function `Dir -> true | _ -> false) s
      let contents t s = find t (function `Dir -> false | _ -> true) s
      let empty = []

      let is_empty = function
        | [] -> true
        | _  -> false

      module N = Irmin.Private.Node.Make (GK)(GK)(C.Path)

      (* FIXME: handle executable files *)
      let alist t =
        List.map (function
            | { Git.Tree.perm = `Dir; name; node } -> (S.of_hum name, `Node node)
            | { Git.Tree.name; node; _ }           -> (S.of_hum name, `Contents node)
          ) t

     let to_n t =
        N.create (alist t)

     let create alist =
       List.map (fun (l, x) ->
           match x with
           | `Contents c -> to_git `Normal (l, c)
           | `Node n     -> to_git `Dir (l, n)
         ) alist

      let of_n n = create (N.alist n)
      let to_json t = N.to_json (to_n t)
      let of_json j = of_n (N.of_json j)

    end

    include AO (GK)(struct
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

  module XCommit = struct
    module Val = struct
      type t = Git.Commit.t
      type node = GK.t

      let compare = Git.Commit.compare
      let equal = Git.Commit.equal
      let hash = Git.Commit.hash
      let read = Git.Commit.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Git.Commit.add buf t;
        Buffer.contents buf

      let write t b = write_string (to_string t) b
      let size_of t =
        (* XXX: yiiik, causes *a lot* of write duplciations *)
        String.length (to_string t)

      let commit_key_of_git k = H.of_raw (GK.to_raw (Git.SHA.of_commit k))
      let node_key_of_git k = Git.SHA.of_tree k

      let task_of_git author message git =
        let id = author.Git.User.name in
        let date, _ = author.Git.User.date in
        let uid = Int64.of_int (Hashtbl.hash (author, message, git)) in
        Irmin.Task.create ~date ~owner:id ~uid message

      let of_git g =
        let { Git.Commit.tree; parents; author; message; _ } = g in
        let parents = List.map commit_key_of_git parents in
        let node = Some (node_key_of_git tree) in
        let task = task_of_git author message g in
        (task, node, parents)

      let to_git task node parents =
        let git_of_commit_key k = Git.SHA.to_commit (GK.of_raw (H.to_raw k)) in
        let git_of_node_key k = Git.SHA.to_tree k in
        let tree = match node with
          | None   ->
            failwith
              "Irmin.Git.Commit: a commit with an empty filesystem... \
               this is not supported by Git!"
          | Some n -> git_of_node_key n
        in
        let parents = List.map git_of_commit_key parents in
        let parents = List.sort Git.SHA.Commit.compare parents in
        let date = Irmin.Task.date task in
        let author =
          Git.User.({ name  = Irmin.Task.owner task;
                      email = "irmin@openmirage.org";
                      date  = date, None;
                    }) in
        let message = String.concat "\n" (Irmin.Task.messages task) in
        { Git.Commit.tree; parents; author; committer = author; message }

      let create task ?node ~parents = to_git task node parents
      let xnode { Git.Commit.tree; _ } = node_key_of_git tree
      let node t = Some (xnode t)
      let parents { Git.Commit.parents; _ } = List.map commit_key_of_git parents
      let task g =
        let { Git.Commit.author; message; _ } = g in
        task_of_git author message g

      module C = Irmin.Private.Commit.Make(H)(GK)

      let of_c c =
        to_git (C.task c) (C.node c) (C.parents c)

      let to_c t =
        let task, node, parents = of_git t in
        C.create task ?node ~parents

      let to_json t = C.to_json (to_c t)
      let of_json j = of_c (C.of_json j)

    end

    module Key = H

    include AO (H)(struct
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

  module XTag = struct

    module Key = T
    module Val = H

    module W = Irmin.Private.Watch.Make(Key)(Val)

    type t = {
      task: Irmin.task;
      config: Irmin.config;
      git_root: string;
      git_head: Git.Reference.head_contents;
      t: G.t;
      w: W.t;
    }

    type key = Key.t
    type value = Val.t

    let task t = t.task

    let tag_of_git r =
      let str = Git.Reference.to_raw r in
      match string_chop_prefix ~prefix:"refs/heads/" str with
      | None   -> None
      | Some r -> Some (Key.of_hum r)

    let git_of_tag_string str =
      Git.Reference.of_raw ("refs/heads" / str)

    let git_of_tag r =
      git_of_tag_string (Key.to_hum r)

    let mem { t; _ } r =
      G.mem_reference t (git_of_tag r)

    let head_of_git k =
      H.of_raw (GK.to_raw (Git.SHA.of_commit k))

    let read { t; _ } r =
      G.read_reference t (git_of_tag r) >>= function
      | None   -> return_none
      | Some k -> return (Some (head_of_git k))

    let ref_of_file ~git_root file =
      match string_chop_prefix ~prefix:(git_root / "refs/heads/") file with
      | None   -> None
      | Some r -> Some (T.of_hum r)

    let create config task =
      let root = Irmin.Private.Conf.get config Conf.root in
      let head = Irmin.Private.Conf.get config Conf.head in
      G.create ?root () >>= fun t ->
      let git_root = G.root t / ".git" in
      let write_head head =
        let head = Git.Reference.Ref head in
        begin
          if G.kind = `Disk then G.write_head t head
          else return_unit
        end >>= fun () ->
        return head
      in
      begin
        G.read_head t >>= function
        | Some (Git.Reference.Ref current_head) ->
          begin match head with
            | None   -> write_head (git_of_tag T.master)
            | Some h ->
              if h = current_head then return (Git.Reference.Ref h)
              else write_head h
          end
        | _ ->
          begin match head with
            | None   -> write_head (git_of_tag T.master)
            | Some h -> write_head h
          end
      end >>= fun git_head ->
      let w = W.create () in
      return (fun a -> { task = task a; git_head; config; t; w; git_root })

    let read_exn { t; _ } r =
      G.read_reference_exn t (git_of_tag r) >>= fun k ->
      return (head_of_git k)

    let iter { t; _ } fn =
      G.references t >>= fun refs ->
      Lwt_list.iter_p (fun r -> match tag_of_git r with
          | None   -> return_unit
          | Some r -> fn r
        ) refs

    let git_of_head k =
      Git.SHA.to_commit (GK.of_raw (H.to_raw k))

    let write_index t gr gk =
      Log.debug "write_index";
      if G.kind = `Disk then (
        let bare = Irmin.Private.Conf.get t.config Conf.bare in
        let git_head = Git.Reference.Ref gr in
        Log.debug "write_index/if bare=%b head=%s" bare (Git.Reference.pretty gr);
        if not bare && git_head = t.git_head then (
          Log.debug "write cache (%s)" (Git.Reference.pretty gr);
          G.write_index t.t gk
        ) else
          return_unit
      ) else
        return_unit

    let update t r k =
      Log.debug "update %s" (Tc.show (module T) r);
      let gr = git_of_tag r in
      let gk = git_of_head k in
      G.write_reference t.t gr gk >>= fun () ->
      W.notify t.w r (Some k);
      write_index t gr gk

    let remove t r =
      G.remove_reference t.t (git_of_tag r) >>= fun () ->
      W.notify t.w r None;
      return_unit

    let watch t (r:key): value option Lwt_stream.t =
      if G.kind = `Disk then
        W.listen_dir t.w (t.git_root / "refs/heads")
          ~key:(ref_of_file ~git_root:t.git_root)
          ~value:(read t);
      Irmin.Private.Watch.lwt_stream_lift (
        read t r >>= fun k ->
        return (W.watch t.w r k)
      )

  end

  module XSync = struct

    module Sync = Git.Sync.Make(IO)(G)

    type t = G.t
    type head = XCommit.key
    type tag = XTag.key

    let head_of_git key = H.of_raw (GK.to_raw (Git.SHA.of_commit key))

    let o_head_of_git = function
      | None   -> return_none
      | Some k -> return (Some (`Local (head_of_git k)))

    let create config =
      let root = Irmin.Private.Conf.get config Conf.root in
      G.create ?root ()

    let fetch t ?depth ~uri tag =
      Log.debug "fetch %s" uri;
      let gri = Git.Gri.of_string uri in
      let deepen = depth in
      let result r =
        Log.debug "fetch result: %s" (Git.Sync.Result.pretty_fetch r);
        let tag = XTag.git_of_tag tag in
        let key =
          let refs = r.Git.Sync.Result.references in
          try Some (Git.Reference.Map.find tag refs)
          with Not_found -> None
        in
        o_head_of_git key
      in
      Sync.fetch t ?deepen gri >>=
      result

    let push t ?depth:_ ~uri tag =
      Log.debug "push %s" uri;
      let branch = XTag.git_of_tag tag in
      let gri = Git.Gri.of_string uri in
      let result r =
        Log.debug "push result: %s" (Git.Sync.Result.pretty_push r);
        match r.Git.Sync.Result.result with
        | `Ok      -> return `Ok
        | `Error _ -> return `Error in
      Sync.push t ~branch gri >>=
      result

  end
  module P = struct
    module Contents = Irmin.Contents.Make(XContents)
    module Node = XNode
    module Commit = XCommit
    module Tag = XTag
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = XSync
  end
  include Irmin.Make_ext(P)
end

module Memory (IO: Git.Sync.IO) = Make (IO) (Git.Memory)
module FS (IO: Git.Sync.IO) (FS: Git.FS.IO) = Make (IO) (Git.FS.Make(FS))

module FakeIO = struct
  type ic = unit
  type oc = unit
  let with_connection _ ?init:_ _ = failwith "FakeIO"
  let read_all _ = failwith "FakeIO"
  let read_exactly _ _ = failwith "FakeIO"
  let write _ _ = failwith "FakeIO"
  let flush _ = failwith "FakeIO"
end

module AO (G: Git.Store.S) (K: Irmin.Hash.S) (V: Tc.S0) = struct
  module V = struct
    include V
    let merge _path ~old:_ _ _ = failwith "Irmin_git.AO.merge"
    module Path = Irmin.Path.String_list
  end
  module M = Make (FakeIO)(G)(V)(Irmin.Tag.String)(K)
  include M.AO(K)(M.GitContents)
end

module RW (G: Git.Store.S) (K: Irmin.Hum.S) (V: Irmin.Hash.S) = struct
  module K = struct
    include K
    let master = K.of_hum "master"
  end
  module M = Make (FakeIO)(G)(Irmin.Contents.String)(K)(V)
  include M.XTag
end

include Conf
