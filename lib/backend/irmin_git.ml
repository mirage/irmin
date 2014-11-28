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

module type S = sig
  include Irmin.S with type step = string and type tag = string
  val create: ?root:string -> ?bare:bool -> Irmin.Task.t -> t Lwt.t
  val of_tag: ?root:string -> ?bare:bool -> Irmin.Task.t -> tag -> t Lwt.t
  val of_head: ?root:string -> ?bare:bool -> Irmin.Task.t -> head -> t Lwt.t
end

module StringMap = Map.Make(Tc.String)

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

let write_string str b =
  let len = String.length str in
  Cstruct.blit_from_string str 0 b 0 len;
  Cstruct.shift b len

let key k f config =
  try f (List.assoc k (IB.Config.to_dict config))
  with Not_found -> None

let key_bool k f d config =
  match key k f config with
  | None   -> d
  | Some b -> b

(* ~root *)
let of_root, to_root, _root = IB.Config.univ Tc.string
let root_k = "git:root"
let root_key = key root_k to_root

(* ~disk *)
let of_disk, to_disk, _disk = IB.Config.univ Tc.bool
let disk_k = "git:disk"
let disk_key = key_bool disk_k to_disk false

(* ~bare *)
let of_bare, to_bare, _bare = IB.Config.univ Tc.bool
let bare_k = "git:bare"
let bare_key = key_bool bare_k to_bare true

module Make (G: Git.Store.S) (IO: Git.Sync.IO) (C: I.Contents.S) = struct

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

  module type V = sig
    type t
    val type_eq: Git.Object_type.t -> bool
    val to_git: t -> Git.Value.t
    val of_git: Git.Value.t -> t option
  end

  module AO (V: V) = struct

    type t = {
      t: G.t;
      task: I.task;
      config: I.config;
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
      | Some v -> return (V.of_git v)

    let read_exn t key =
      read t key >>= function
      | None   -> fail Not_found
      | Some v -> return v

    let list _ k =
      return k

    let dump { t; _ } =
      G.list t >>= fun keys ->
      Lwt_list.fold_left_s (fun acc k ->
          G.read_exn t k >>= fun v ->
          match V.of_git v with
          | None   -> return acc
          | Some v -> return ((k, v) :: acc)
        ) [] keys

    let add { t; _ } v =
      G.write t (V.to_git v)

  end

  module XContents = struct
    include AO (struct
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
      end)
      module Val = C
      module Key = K
    end

  module XNode = struct
    module Key = K
    module Step = Tc.String
    module Val = struct

      type t = Git.Tree.t
      type contents = K.t
      type node = K.t
      type step = string

      let compare = Git.Tree.compare
      let equal = Git.Tree.equal
      let hash = Git.Tree.hash
      let to_sexp = Git.Tree.sexp_of_t
      let read = Git.Tree.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Git.Tree.add buf t;
        Buffer.contents buf

      let write t b =
        write_string (to_string t) b

      let size_of _t =
        failwith "Git.Tree.size_of"
        (* XXX: eeerk: might cause wwrite duplication!!  *)
        (* String.length (to_string t) *)

      let of_git { Git.Tree.name; node; _ } = (name, node)
      let to_git perm (name, node) = { Git.Tree.perm; name; node }

      let all_contents t =
        List.filter (fun { Git.Tree.perm; _ } -> perm <> `Dir) t
        |> List.map of_git

      let all_succ t =
        List.filter (fun { Git.Tree.perm; _ } -> perm <> `Dir) t
        |> List.map of_git

      let find t p s =
        try
          List.find (fun { Git.Tree.perm; name; _ } -> p perm && name = s) t
          |> fun v -> Some v.Git.Tree.node
        with Not_found ->
          None

      let remove t p s =
        List.filter
          (fun { Git.Tree.perm; name; _ } -> not (p perm && name = s))
          t

      let with_succ t name node =
        let t = remove t (function `Dir -> true | _ -> false) name in
        match node with
        | None      -> t
        | Some node -> to_git `Dir (name, node) :: t

      let with_contents t name node =
        let t = remove t (function `Dir -> false | _ -> true) name in
        match node with
        | None      -> t
        | Some node -> to_git `Normal (name, node) :: t

      let succ t s = find t (function `Dir -> true | _ -> false) s
      let contents t s = find t (function `Dir -> false | _ -> true) s
      let steps t = List.map (fun { Git.Tree.name; _ } -> name) t
      let empty = []
      let create ~contents ~succ =
        List.map (to_git `Normal) contents @ List.map (to_git `Dir) succ

      let is_empty = function
        | [] -> true
        | _  -> false

      let edges t =
        List.map (function
            | { Git.Tree.perm = `Dir; node; _ } -> `Node node
            | { Git.Tree.node; _ } -> `Contents node
          ) t

      module N = IB.Node.Make (K)(K)(Tc.String)

      (* FIXME: handle executable files *)
      let to_n t =
        let succ = all_succ t in
        let contents = all_contents t in
        N.create ~contents ~succ

      let of_n n = create ~contents:(N.all_contents n) ~succ:(N.all_succ n)

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

  module XCommit = struct
    module Val = struct
      type t = Git.Commit.t
      type node = K.t

      let to_sexp = Git.Commit.sexp_of_t
      let compare = Git.Commit.compare
      let equal = Git.Commit.equal
      let hash = Git.Commit.hash
      let read = Git.Commit.input

      let to_string t =
        let buf = Buffer.create 1024 in
        Git.Commit.add buf t;
        Buffer.contents buf

      let write t b = write_string (to_string t) b
      let size_of _t =
        failwith "Git.Commit.size_of"
        (* XXX: yiiik *)
        (* String.length (to_string t) *)

      let commit_key_of_git k = Git.SHA.of_commit k
      let node_key_of_git k = Git.SHA.of_tree k

      let task_of_git author message =
        let id = author.Git.User.name in
        let date = match Stringext.split ~on:' ' author.Git.User.date with
          | [date;_] -> Int64.of_string date
          | _        -> 0L in
        I.Task.create ~date ~owner:id "%s" message

      let of_git { Git.Commit.tree; parents; author; message; _ } =
        let parents = List.map commit_key_of_git parents in
        let node = Some (node_key_of_git tree) in
        let task = task_of_git author message in
        (task, node, parents)

      let to_git task node parents =
        let git_of_commit_key k = Git.SHA.to_commit k in
        let git_of_node_key k = Git.SHA.to_tree k in
        let tree = match node with
          | None   ->
            failwith
              "Irmin.Git.Commit: a commit with an empty filesystem... \
               this is not supported by Git!"
          | Some n -> git_of_node_key n
        in
        let parents = List.map git_of_commit_key parents in
        let date = Int64.to_string (I.Task.date task) ^ " +0000" in
        let author =
          Git.User.({ name  = I.Task.owner task;
                      email = "irmin@openmirage.org";
                      date;
                    }) in
        let message = String.concat "\n" (I.Task.messages task) in
        { Git.Commit.tree; parents; author; committer = author; message }

      let create task ?node ~parents = to_git task node parents
      let xnode { Git.Commit.tree; _ } = node_key_of_git tree
      let node t = Some (xnode t)
      let parents { Git.Commit.parents; _ } = List.map commit_key_of_git parents
      let task { Git.Commit.author; message; _ } = task_of_git author message

      let edges t =
        let node = xnode t in
        let parents = parents t in
        `Node node :: List.map (fun k -> `Commit k) parents

      module C = IB.Commit.Make(K)(K)

      let of_c c =
        to_git (C.task c) (C.node c) (C.parents c)

      let to_c t =
        let task, node, parents = of_git t in
        C.create task ?node ~parents

      let to_json t = C.to_json (to_c t)
      let of_json j = of_c (C.of_json j)

    end

    module Key = K

    include AO(struct
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

    module T = I.Tag.String
    module Key = T
    module Val = K

    let t: T.t Tc.t = (module T)

    module W = IB.Watch.Make(T)(K)

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

  module XSync = struct

    module Sync = Git.Sync.Make(IO)(G)
    module S = IB.BC (XContents)(XNode)(XCommit)(XTag)

    type t = G.t
    type head = S.head
    type tag = S.tag

    let key_of_git key = Git.SHA.of_commit key

    let o_key_of_git = function
      | None   -> return_none
      | Some k -> return (Some (`Local (key_of_git k)))

    let create config =
      let root = root_key config in
      G.create ?root ()

    let fetch t ?depth ~uri tag =
      Log.debugf "fetch %s" uri;
      let gri = Git.Gri.of_string uri in
      let deepen = depth in
      let result r =
        Log.debugf "fetch result: %s" (Git.Sync.Result.pretty_fetch r);
        let tag = XTag.git_of_ref tag in
        let key =
          let refs = r.Git.Sync.Result.references in
          try Some (Git.Reference.Map.find tag refs)
          with Not_found -> None
        in
        o_key_of_git key
      in
      Sync.fetch t ?deepen gri >>=
      result

    let push t ?depth:_ ~uri tag =
      Log.debugf "push %s" uri;
      let branch = XTag.git_of_ref tag in
      let gri = Git.Gri.of_string uri in
      let result r =
        Log.debugf "push result: %s" (Git.Sync.Result.pretty_push r);
        match r.Git.Sync.Result.result with
        | `Ok      -> return `Ok
        | `Error _ -> return `Error in
      Sync.push t ~branch gri >>=
      result

  end

  include IB.Make(XContents)(XNode)(XCommit)(XTag)(XSync)

  let mk root bare =
    let root = match root with
      | None   -> []
      | Some r -> [ root_k, of_root r ]
    in
    let disk = match (* FIXME: G.backend_type *) `Disk with
      | `Memory -> [ disk_k, of_disk false ]
      | `Disk   -> [ disk_k, of_disk true ]
    in
    let bare = match bare with
      | None   -> [ bare_k, of_bare true ]
      | Some b -> [ bare_k, of_bare b ]
    in
    IB.Config.of_dict (root @ disk @ bare)

  let create ?root ?bare task = create (mk root bare) task
  let of_tag ?root ?bare task tag = of_tag (mk root bare) task tag
  let of_head ?root ?bare task head = of_head (mk root bare) task head

end

module Memory (IO: Git.Sync.IO) (C: I.Contents.S) = Make (Git.Memory)(IO)(C)
