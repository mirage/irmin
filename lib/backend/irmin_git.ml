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

module I = Irmin
module IB = Irmin.Backend
module Log = Log.Make(struct let section = "GIT" end)

module StringMap = Map.Make(Tc.String)

let string_map_of_alist l =
  List.fold_left (fun map (k, v)  -> StringMap.add k v map) StringMap.empty l

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

let list_filter_map f l =
  List.fold_left (fun acc x -> match f x with
      | None -> acc
      | Some y -> y :: acc
    ) [] l

let lwt_stream_lift s =
  let (stream: 'a Lwt_stream.t option ref) = ref None in
  let rec get () =
    match !stream with
    | Some s -> Lwt_stream.get s
    | None   ->
      s >>= fun s ->
      stream := Some s;
      get ()
  in
  Lwt_stream.from get

let of_root, to_root, root = I.Config.univ Tc.string
let of_bare, to_bare, bare = I.Config.univ Tc.bool
let of_disk, to_disk, disk = I.Config.univ Tc.bool

let root_k = "git:root"
let bare_k = "git:bare"
let disk_k = "git:disk"

let key k f config =
  try f (List.assoc k (I.Config.to_dict config))
  with Not_found -> None

let root_key = key root_k to_root

let key_bool k f d config =
  match key k f config with
  | None   -> d
  | Some b -> b

let bare_key = key_bool bare_k to_bare true
let disk_key = key_bool disk_k to_disk false

module Make (G: Git.Store.S) (S: Git.Sync.S) (C: I.Contents.S) = struct

  module K = struct
    type t = Git.SHA.t
    let hash = Git.SHA.hash
    let compare = Git.SHA.compare
    let equal = (=)
    let to_sexp t = Sexplib.Type.Atom (Git.SHA.to_hex t)
    let to_json t = Ezjsonm.string (Git.SHA.to_hex t)
    let of_json j = Git.SHA.of_hex (Ezjsonm.get_string j)
    let size_of _ = 20
    let read buf = Git.SHA.of_raw (Mstruct.get_string buf 20)
    let write t buf =
      Cstruct.blit_from_string (Git.SHA.to_raw t) 0 buf 0 20;
      Cstruct.shift buf 20
    let digest = Git.SHA.of_cstruct
  end

  let k: K.t Tc.t = (module K)
  let c: C.t Tc.t = (module C)

  module type V = sig
    type t
    val type_eq: Git.Object_type.t -> bool
    val to_git: G.t -> t -> [`Value of Git.Value.t Lwt.t | `Key of Git.SHA.t]
    val of_git: Git.SHA.t -> Git.Value.t -> t option
  end

  module AO (V: V): I.AO with type key = K.t and type value = V.t = struct

    type t = {
      t: G.t;
      task: I.Task.t;
      config: I.Config.t;
    }

    type key = K.t

    type value = V.t

    let create config task =
      let root = root_key config in
      G.create ?root () >>= fun t ->
      return { task; config; t }

    let task t = t.task
    let config t = t.config

    let mem { t; _ } key =
      G.mem t key >>= function
      | false    -> return false
      | true     ->
        G.read t key >>= function
        | None   -> return false
        | Some v -> return (V.type_eq (Git.Value.type_of v))

    let read { t; _ } key =
      G.read t key >>= function
      | None   -> return_none
      | Some v -> return (V.of_git key v)

    let read_exn t key =
      read t key >>= function
      | None   -> fail Not_found
      | Some v -> return v

    let list { t; _ } k =
      return k

    let dump { t; _ } =
      G.list t >>= fun keys ->
      Lwt_list.fold_left_s (fun acc k ->
          G.read_exn t k >>= fun v ->
          match V.of_git k v with
          | None   -> return acc
          | Some v -> return ((k, v) :: acc)
        ) [] keys

    let add { t; _ } v =
      match V.to_git t v with
      | `Key k   -> return k
      | `Value v -> v >>=  G.write t

  end

  module Contents: IB.Contents.STORE = struct

    include AO (struct

        type t = C.t

        let type_eq = function
          | Git.Object_type.Blob
          | _ -> false

        let of_git k b =
          match b with
          | Git.Value.Blob b -> Some (Tc.read_string c (Git.Blob.to_raw b))
          | _                -> None

        let to_git _ b =
          let value = Git.Value.Blob (Git.Blob.of_raw (Tc.write_string c b)) in
          `Value (return value)

      end)

      module Val = C
      module Key = K
    end

  module Node: IB.Node.STORE = struct
    module Key = K
    module Step = Tc.String
    module StepMap = Map.Make(Tc.String)
    module Val = struct

      type t = Git.Tree.t

      let compare = Git.Tree.compare
      let equal = Git.Tree.equal
      let hash = Git.Tree.hash
      let to_sexp = Git.Tree.sexp_of_t
      let write t b =
        let buf = Buffer.create 1024 in
        Git.Tree.add buf t;
        let str = Buffer.contents buf in
        let len = String.length str in
        Cstruct.blit_from_string str 0 b 0 len;
        Cstruct.shift b len

  let read buf =
    Mstruct.get_string buf (Mstruct.length buf)




      module N = IB.Node.Make (K)(K)(Tc.String)

      (* Name of the file containing the node contents. *)
      let to_attribute name = name ^ "#"
      let of_attribute name =
        if name.[String.length name - 1] = '#' then
          String.sub name 0 (String.length name - 1)
        else
          name

      (* FIXME: handler executable files *)
      let to_n t =
        let contents, succ =
          List.partition (fun { Git.Tree.perm; _ } -> perm <> `Dir) t
        in
        let succ = string_map_of_alist
          (List.map (fun { Git.Tree.name; node; _ } -> (name, node)) succ)
        in
        let contents = string_map_of_alist
          (List.map (fun { Git.Tree.name; node; _ } ->
                if StringMap.mem name succ then (to_attribute name, node)
                else (name, node)
              ) contents)
        in
        N.create contents succ

      let of_n n =
        if N.is_leaf n then
          match N.contents n with
          | None   -> failwith "Node.Val.to_git"
          | Some k -> Leaf k
        else
          let mktree entries =
            let entries = StringMap.bindings entries in
            let file node = { Git.Tree.perm = `Normal; name; node } in
            let dir node  = { Git.Tree.perm = `Dir   ; name; node } in
            Node (
              (match N.contents n with
               | None   -> []
               | Some k -> file k)
              @
              List.map (fun (name, node) ->
                  (* XXX: handle exec files. *)

                    (fun () -> G.read t node)
                      (function Zlib.Error _ -> return_none | e -> fail e)
                    >>= function
                    | None   -> dir () (* on import, the children nodes migh not
                                          have been loaded properly yet. *)
                    | Some v ->
                      match Git.Value.type_of v with
                      | Git.Object_type.Blob -> file ()
                      | Git.Object_type.Tree -> dir ()
                      | _                    -> fail (Failure "Node.to_git")
                  ) entries >>= fun entries ->
                return entries
              ) in
          match Val.contents node with
          | None     -> mktree (Val.succ node)
          | Some key ->
            (* This is an extended node (ie. with child and contents).
               Store the node contents in a dummy `.contents` file. *)
            mktree (StringMap.add contents_child key (Val.succ node))

    end

    include AO (struct

        type t = Val.t

        let type_eq = function
          | Git.Object_type.Blob
          | Git.Object_type.Tree -> true
          | _ -> false



      end)
  end

  module Commit: IB.Commit.STORE = struct
    module Val = IB.Commit.Make(K)(K)
    module Key = K
    include AO(struct

        type t = Val.t

        let type_eq = function
          | Git.Object_type.Commit -> true
          | _ -> false

        let of_git k v =
          match v with
          | Git.Value.Commit { Git.Commit.tree; parents; author; message; _ } ->
            let commit_key_of_git k = Git.SHA.of_commit k in
            let node_key_of_git k = Git.SHA.of_tree k in
            let parents = List.map commit_key_of_git parents in
            let node = Some (node_key_of_git tree) in
            let id = author.Git.User.name in
            let date = match Stringext.split ~on:' ' author.Git.User.date with
              | [date;_] -> Int64.of_string date
              | _        -> 0L in
            let task = I.Task.create ~date ~owner:id "%s" message in
            Some (Val.create task ?node ~parents)
          | _ -> None

        let to_git _ c =
          let node = Val.node c in
          let parents = Val.parents c in
          let task = Val.task c in
          match node with
          | None      -> failwith "Commit.to_git: not supported"
          | Some node ->
            let git_of_commit_key k = Git.SHA.to_commit k in
            let git_of_node_key k = Git.SHA.to_tree k in
            let tree = git_of_node_key node in
            let parents = List.map git_of_commit_key parents in
            let date = Int64.to_string (I.Task.date task) ^ " +0000" in
            let author =
              Git.User.({ name  = I.Task.owner task;
                          email = "irmin@openmirage.org";
                          date;
                        }) in
            let message = String.concat "\n" (I.Task.messages task) in
            let commit = {
              Git.Commit.tree; parents;
              author; committer = author;
              message } in
            let value = Git.Value.Commit commit in
            `Value (return value)

      end)

  end

  module Tag: IB.Tag.STORE = struct

    module T = I.Tag.String
    module Key = T
    module Val = K

    let t: T.t Tc.t = (module T)

    module W = I.Watch.Make(T)(K)

    type t = {
      task: I.task;
      config: I.config;
      t: G.t;
      w: W.t;
    }

    let (/) = Filename.concat

    type key = Key.t
    type value = Val.t

    let task t = t.task
    let config t = t.config

    let ref_of_git r =
      let str = Git.Reference.to_raw r in
      match string_chop_prefix ~prefix:"refs/heads/" str with
      | None   -> None
      | Some r -> Some (Tc.read_string t r)

    let git_of_ref r =
      let str = Tc.write_string t r in
      Git.Reference.of_raw ("refs/heads" / str)

    let mem { t; _ } r =
      G.mem_reference t (git_of_ref r)

    let key_of_git k = Git.SHA.of_commit k

    let read { t; _ } r =
      G.read_reference t (git_of_ref r) >>= function
      | None   -> return_none
      | Some k -> return (Some (key_of_git k))

    let create config task =
      let root = root_key config in
      G.create ?root () >>= fun t ->
      let git_root = G.root t / ".git" in
      let ref_of_file file =
        match string_chop_prefix ~prefix:(git_root / "refs/heads/") file with
        | None   -> None
        | Some r -> Some r in
      let w = W.create () in
      let t = { task; config; t; w } in
      if disk_key config then
        W.listen_dir w (git_root / "refs/heads") ~key:ref_of_file ~value:(read t);
      return t

    let read_exn { t; _ } r =
      G.read_reference_exn t (git_of_ref r) >>= fun k ->
      return (key_of_git k)

    let list { t; _ } _ =
      Log.debugf "list";
      G.references t >>= fun refs ->
      return (list_filter_map ref_of_git refs)

    let dump { t; _ } =
      Log.debugf "dump";
      G.references t >>= fun refs ->
      Lwt_list.map_p (fun r ->
          match ref_of_git r with
          | None     -> return_none
          | Some ref ->
            G.read_reference_exn t r >>= fun k ->
            return (Some (ref, key_of_git k))
        ) refs >>= fun l ->
      list_filter_map (fun x -> x) l |> return

    let git_of_key k = Git.SHA.to_commit k

    let update t r k =
      let gr = git_of_ref r in
      let gk = git_of_key k in
      G.write_head t.t (Git.Reference.Ref gr) >>= fun () ->
      G.write_reference t.t gr gk >>= fun () ->
      W.notify t.w r (Some k);
      if disk_key t.config && not (bare_key t.config) then
        G.write_cache t.t gk
      else
        return_unit

    let remove t r =
      G.remove_reference t.t (git_of_ref r) >>= fun () ->
      W.notify t.w r None;
      return_unit

    let watch t (r:key): value option Lwt_stream.t =
      lwt_stream_lift (
        read t r >>= fun k ->
        return (W.watch t.w r k)
      )

  end

  module Remote = functor (S: I.BC) -> struct

    type key = K.t

    let key_of_git key = Git.SHA.of_commit key

    let o_key_of_git = function
      | None   -> return_none
      | Some k -> return (Some (key_of_git k))

    let fetch t ?depth uri =
      Log.debugf "fetch %s" uri;
      let gri = Git.Gri.of_string uri in
      let deepen = depth in
      let result r =
        Log.debugf "fetch result: %s" (Git.Sync.Result.pretty_fetch r);
        let key = match r.Git.Sync.Result.head with
          | Some _ as h -> h
          | None        ->
            let max () =
              match Git.Reference.Map.to_alist r.Git.Sync.Result.references with
              | []          -> None
              | (_, k) :: _ -> Some k in
            match S.tag t with
            | None        -> max ()
            | Some branch ->
              let branch = Git.Reference.of_raw ("refs/heads/" ^ S.Branch.to_raw branch) in
              try Some (Git.Reference.Map.find branch
                          r.Git.Sync.Result.references)
              with Not_found -> max ()
        in
        o_key_of_git key in
      Config.Sync.fetch (S.contents_t t) ?deepen gri >>=
      result

    let push t ?depth uri =
      Log.debugf "push %s" uri;
      match S.branch t with
      | None        -> return_none
      | Some branch ->
        let branch = Git.Reference.of_raw (S.Branch.to_raw branch) in
        let gri = Git.Gri.of_string uri in
        let result { Git.Sync.Result.result } = match result with
          | `Ok      -> S.head t
          | `Error _ -> return_none in
        Config.Sync.push (S.contents_t t) ~branch gri >>=
        result

  end

end

module Memory = Make(struct
    let root = None
    module Store = Git.Memory
    module Sync = struct
      type t = Store.t
      include NoSync
    end
    let bare = true
    let disk = false
  end)

module Memory' (C: sig val root: string end) = Make(struct
    let root = Some C.root
    module Store = Git.Memory
    module Sync = struct
      type t = Store.t
      include NoSync
    end
    let bare = true
    let disk = false
  end)
