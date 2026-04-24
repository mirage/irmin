(* Lwt compatibility layer for Irmin 4.

   Every wrapped operation threads its call through [Lwt_eio.run_eio] so
   the direct-style Irmin 4 implementation executes on the Eio
   scheduler while the caller remains in the Lwt monad. *)

let run_eio f = Lwt_eio.run_eio f

let run f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Lwt_eio.Promise.await_lwt (f ())

let run_with_env env f =
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Lwt_eio.Promise.await_lwt (f ())

module Make (S : Irmin.Generic_key.S) = struct
  type repo = S.repo
  type t = S.t
  type step = S.step
  type path = S.path
  type metadata = S.metadata
  type contents = S.contents
  type node = S.node
  type tree = S.tree
  type commit = S.commit
  type branch = S.branch
  type slice = S.slice
  type info = S.info
  type hash = S.hash
  type contents_key = S.contents_key
  type node_key = S.node_key
  type commit_key = S.commit_key
  type lca_error = S.lca_error
  type ff_error = S.ff_error
  type write_error = S.write_error
  type kinded_key = [ `Contents of contents_key | `Node of node_key ]

  (* Re-exports of the type-level modules of [S]. These are pure,
     forwarded as-is. *)
  module Schema = S.Schema
  module Info = S.Info
  module Hash = S.Hash
  module Path = S.Path
  module Metadata = S.Metadata
  module Backend = S.Backend
  module Contents = S.Contents
  module History = S.History
  module Status = S.Status

  module Repo = struct
    type nonrec t = repo

    let v config = run_eio (fun () -> S.Repo.v config)
    let close r = run_eio (fun () -> S.Repo.close r)
    let heads r = run_eio (fun () -> S.Repo.heads r)
    let branches r = run_eio (fun () -> S.Repo.branches r)
    let config r = S.Repo.config r

    let export ?full ?depth ?min ?max r =
      run_eio (fun () -> S.Repo.export ?full ?depth ?min ?max r)
  end

  let main r = run_eio (fun () -> S.main r)
  let master r = run_eio (fun () -> S.main r)
  let of_branch r b = run_eio (fun () -> S.of_branch r b)
  let of_commit c = run_eio (fun () -> S.of_commit c)
  let empty r = run_eio (fun () -> S.empty r)

  (* Pure accessors — no I/O, no wrapping needed. *)
  let repo = S.repo
  let tree = S.tree
  let status = S.status
  let find t p = run_eio (fun () -> S.find t p)
  let find_all t p = run_eio (fun () -> S.find_all t p)
  let mem t p = run_eio (fun () -> S.mem t p)
  let mem_tree t p = run_eio (fun () -> S.mem_tree t p)
  let get t p = run_eio (fun () -> S.get t p)
  let get_all t p = run_eio (fun () -> S.get_all t p)
  let find_tree t p = run_eio (fun () -> S.find_tree t p)
  let get_tree t p = run_eio (fun () -> S.get_tree t p)
  let hash t p = run_eio (fun () -> S.hash t p)
  let kind t p = run_eio (fun () -> S.kind t p)
  let list t p = run_eio (fun () -> S.list t p)
  let key t p = run_eio (fun () -> S.key t p)

  let set ?clear ?retries ?allow_empty ?parents ~info t p v =
    run_eio (fun () -> S.set ?clear ?retries ?allow_empty ?parents ~info t p v)

  let set_exn ?clear ?retries ?allow_empty ?parents ~info t p v =
    run_eio (fun () ->
        S.set_exn ?clear ?retries ?allow_empty ?parents ~info t p v)

  let set_tree ?clear ?retries ?allow_empty ?parents ~info t p tr =
    run_eio (fun () ->
        S.set_tree ?clear ?retries ?allow_empty ?parents ~info t p tr)

  let set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p tr =
    run_eio (fun () ->
        S.set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p tr)

  let remove ?clear ?retries ?allow_empty ?parents ~info t p =
    run_eio (fun () -> S.remove ?clear ?retries ?allow_empty ?parents ~info t p)

  let remove_exn ?clear ?retries ?allow_empty ?parents ~info t p =
    run_eio (fun () ->
        S.remove_exn ?clear ?retries ?allow_empty ?parents ~info t p)

  (* Irmin.Type.t descriptors derived by [@@deriving irmin] on [S]. *)
  let step_t = S.step_t
  let path_t = S.path_t
  let metadata_t = S.metadata_t
  let contents_t = S.contents_t
  let node_t = S.node_t
  let tree_t = S.tree_t
  let hash_t = S.hash_t
  let branch_t = S.branch_t
  let slice_t = S.slice_t
  let info_t = S.info_t
  let lca_error_t = S.lca_error_t
  let ff_error_t = S.ff_error_t
  let contents_key_t = S.contents_key_t
  let node_key_t = S.node_key_t
  let commit_key_t = S.commit_key_t
  let write_error_t = S.write_error_t
  let commit_t = S.commit_t

  let test_and_set ?clear ?retries ?allow_empty ?parents ~info t p ~test ~set =
    run_eio (fun () ->
        S.test_and_set ?clear ?retries ?allow_empty ?parents ~info t p ~test
          ~set)

  let test_and_set_exn ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run_eio (fun () ->
        S.test_and_set_exn ?clear ?retries ?allow_empty ?parents ~info t p ~test
          ~set)

  let test_and_set_tree ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run_eio (fun () ->
        S.test_and_set_tree ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_and_set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run_eio (fun () ->
        S.test_and_set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_set_and_get ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run_eio (fun () ->
        S.test_set_and_get ?clear ?retries ?allow_empty ?parents ~info t p ~test
          ~set)

  let test_set_and_get_exn ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run_eio (fun () ->
        S.test_set_and_get_exn ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_set_and_get_tree ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run_eio (fun () ->
        S.test_set_and_get_tree ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_set_and_get_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run_eio (fun () ->
        S.test_set_and_get_tree_exn ?clear ?retries ?allow_empty ?parents ~info
          t p ~test ~set)

  let merge ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run_eio (fun () ->
        S.merge ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run_eio (fun () ->
        S.merge_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_tree ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run_eio (fun () ->
        S.merge_tree ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_tree_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run_eio (fun () ->
        S.merge_tree_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let with_tree ?clear ?retries ?allow_empty ?parents ?strategy ~info t p f =
    run_eio (fun () ->
        S.with_tree ?clear ?retries ?allow_empty ?parents ?strategy ~info t p f)

  let with_tree_exn ?clear ?retries ?allow_empty ?parents ?strategy ~info t p f
      =
    run_eio (fun () ->
        S.with_tree_exn ?clear ?retries ?allow_empty ?parents ?strategy ~info t
          p f)

  let clone ~src ~dst = run_eio (fun () -> S.clone ~src ~dst)

  type 'a merge =
    info:S.Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Irmin.Merge.conflict) result Lwt.t

  let merge_into ~into ~info ?max_depth ?n t =
    run_eio (fun () -> S.merge_into ~into ~info ?max_depth ?n t)

  let merge_with_branch t ~info ?max_depth ?n b =
    run_eio (fun () -> S.merge_with_branch t ~info ?max_depth ?n b)

  let merge_with_commit t ~info ?max_depth ?n c =
    run_eio (fun () -> S.merge_with_commit t ~info ?max_depth ?n c)

  let lcas ?max_depth ?n t1 t2 = run_eio (fun () -> S.lcas ?max_depth ?n t1 t2)

  let lcas_with_branch t ?max_depth ?n b =
    run_eio (fun () -> S.lcas_with_branch t ?max_depth ?n b)

  let lcas_with_commit t ?max_depth ?n c =
    run_eio (fun () -> S.lcas_with_commit t ?max_depth ?n c)

  let history ?depth ?min ?max t =
    run_eio (fun () -> S.history ?depth ?min ?max t)

  let last_modified ?depth ?n t p =
    run_eio (fun () -> S.last_modified ?depth ?n t p)

  (* Backend converters. These are pure. *)
  let of_backend_node = S.of_backend_node
  let to_backend_node = S.to_backend_node
  let to_backend_portable_node = S.to_backend_portable_node
  let to_backend_commit = S.to_backend_commit
  let of_backend_commit = S.of_backend_commit

  (* Extend the top-level [Irmin.remote] the same way [S] does, so the
     identifiers in [Irmin_lwt.Make(S).E] and [S.E] refer to remotes carrying
     the same [endpoint] type. *)
  type Irmin.remote += E of Backend.Remote.endpoint

  (* Saves. These do I/O. *)
  let save_contents c v = run_eio (fun () -> S.save_contents c v)
  let save_tree ?clear r c n t = run_eio (fun () -> S.save_tree ?clear r c n t)

  module Tree = struct
    type nonrec t = tree
    type metadata = S.metadata
    type node = S.node
    type step = S.step
    type kinded_hash = S.Tree.kinded_hash
    type kinded_key = S.Tree.kinded_key
    type elt = S.Tree.elt

    (* Pure constructors and inspectors. *)
    let empty = S.Tree.empty
    let singleton = S.Tree.singleton
    let of_contents = S.Tree.of_contents
    let of_node = S.Tree.of_node
    let v = S.Tree.v
    let pruned = S.Tree.pruned
    let is_empty = S.Tree.is_empty
    let destruct = S.Tree.destruct
    let hash = S.Tree.hash
    let kinded_hash = S.Tree.kinded_hash
    let key = S.Tree.key
    let shallow = S.Tree.shallow
    let clear = S.Tree.clear
    let of_concrete = S.Tree.of_concrete
    let pp = S.Tree.pp

    (* I/O-performing ops, wrapped. *)
    let kind t p = run_eio (fun () -> S.Tree.kind t p)
    let diff x y = run_eio (fun () -> S.Tree.diff x y)
    let mem t p = run_eio (fun () -> S.Tree.mem t p)
    let find_all t p = run_eio (fun () -> S.Tree.find_all t p)
    let length t ?cache p = run_eio (fun () -> S.Tree.length t ?cache p)
    let find t p = run_eio (fun () -> S.Tree.find t p)
    let get_all t p = run_eio (fun () -> S.Tree.get_all t p)
    let get t p = run_eio (fun () -> S.Tree.get t p)

    let list t ?offset ?length ?cache p =
      run_eio (fun () -> S.Tree.list t ?offset ?length ?cache p)

    let seq t ?offset ?length ?cache p =
      run_eio (fun () -> S.Tree.seq t ?offset ?length ?cache p)

    let add t p ?metadata c = run_eio (fun () -> S.Tree.add t p ?metadata c)

    let update t p ?metadata f =
      run_eio (fun () -> S.Tree.update t p ?metadata f)

    let remove t p = run_eio (fun () -> S.Tree.remove t p)
    let mem_tree t p = run_eio (fun () -> S.Tree.mem_tree t p)
    let find_tree t p = run_eio (fun () -> S.Tree.find_tree t p)
    let get_tree t p = run_eio (fun () -> S.Tree.get_tree t p)
    let add_tree t p sub = run_eio (fun () -> S.Tree.add_tree t p sub)
    let update_tree t p f = run_eio (fun () -> S.Tree.update_tree t p f)
    let stats ?force t = run_eio (fun () -> S.Tree.stats ?force t)
    let to_concrete t = run_eio (fun () -> S.Tree.to_concrete t)
    let find_key r t = run_eio (fun () -> S.Tree.find_key r t)
    let of_key r k = run_eio (fun () -> S.Tree.of_key r k)
    let of_hash r h = run_eio (fun () -> S.Tree.of_hash r h)

    (* [fold] accepts Lwt-returning folders as Irmin 3 did; each folder is
       bridged to direct style via [Lwt_eio.Promise.await_lwt] before being
       handed to the underlying Irmin 4 [S.Tree.fold]. *)
    type marks = S.Tree.marks

    let empty_marks = S.Tree.empty_marks

    type 'a force_lwt = [ `True | `False of path -> 'a -> 'a Lwt.t ]
    type uniq = [ `False | `True | `Marks of marks ]
    type ('a, 'b) folder_lwt = path -> 'b -> 'a -> 'a Lwt.t
    type depth = S.Tree.depth

    let lift_folder = function
      | None -> None
      | Some (f : _ folder_lwt) ->
          Some (fun path b acc -> Lwt_eio.Promise.await_lwt (f path b acc))

    let lift_force = function
      | None -> None
      | Some `True -> Some `True
      | Some (`False f) ->
          Some (`False (fun path acc -> Lwt_eio.Promise.await_lwt (f path acc)))

    let fold ?order ?force ?cache ?uniq ?pre ?post ?depth ?contents ?node ?tree
        t acc =
      let force = lift_force force in
      let pre = lift_folder pre in
      let post = lift_folder post in
      let contents = lift_folder contents in
      let node = lift_folder node in
      let tree = lift_folder tree in
      run_eio (fun () ->
          S.Tree.fold ?order ?force ?cache ?uniq ?pre ?post ?depth ?contents
            ?node ?tree t acc)
  end

  module Commit = struct
    type nonrec t = commit
    type commit_key = S.commit_key

    (* Pure accessors. *)
    let tree = S.Commit.tree
    let parents = S.Commit.parents
    let info = S.Commit.info
    let hash = S.Commit.hash
    let key = S.Commit.key
    let pp = S.Commit.pp
    let pp_hash = S.Commit.pp_hash

    (* I/O-performing. *)
    let v ?clear r ~info ~parents tree =
      run_eio (fun () -> S.Commit.v ?clear r ~info ~parents tree)

    let of_key r k = run_eio (fun () -> S.Commit.of_key r k)
    let of_hash r h = run_eio (fun () -> S.Commit.of_hash r h)
  end

  module Branch = struct
    type nonrec t = branch

    let mem r b = run_eio (fun () -> S.Branch.mem r b)
    let find r b = run_eio (fun () -> S.Branch.find r b)
    let get r b = run_eio (fun () -> S.Branch.get r b)
    let set r b c = run_eio (fun () -> S.Branch.set r b c)
    let remove r b = run_eio (fun () -> S.Branch.remove r b)
    let list r = run_eio (fun () -> S.Branch.list r)
    let pp = S.Branch.pp

    let watch r b ?init lwt_cb =
      let cb diff = Lwt_eio.Promise.await_lwt (lwt_cb diff) in
      run_eio (fun () -> S.Branch.watch r b ?init cb)

    let watch_all r ?init lwt_cb =
      let cb br diff = Lwt_eio.Promise.await_lwt (lwt_cb br diff) in
      run_eio (fun () -> S.Branch.watch_all r ?init cb)
  end

  module Head = struct
    let list r = run_eio (fun () -> S.Head.list r)
    let find t = run_eio (fun () -> S.Head.find t)
    let get t = run_eio (fun () -> S.Head.get t)
    let set t c = run_eio (fun () -> S.Head.set t c)

    let fast_forward t ?max_depth ?n c =
      run_eio (fun () -> S.Head.fast_forward t ?max_depth ?n c)

    let test_and_set t ~test ~set =
      run_eio (fun () -> S.Head.test_and_set t ~test ~set)

    let merge ~into ~info ?max_depth ?n c =
      run_eio (fun () -> S.Head.merge ~into ~info ?max_depth ?n c)
  end

  type watch = S.watch
  (** Top-level watches. *)

  let watch t ?init lwt_cb =
    let cb diff = Lwt_eio.Promise.await_lwt (lwt_cb diff) in
    run_eio (fun () -> S.watch t ?init cb)

  let watch_key t path ?init lwt_cb =
    let cb diff = Lwt_eio.Promise.await_lwt (lwt_cb diff) in
    run_eio (fun () -> S.watch_key t path ?init cb)

  let unwatch w = run_eio (fun () -> S.unwatch w)
end

(* Lwt wrappers for [irmin-pack-unix]-specific operations.

   [Pack.Make] takes an [Irmin_pack_io.S] (the full pack-unix store
   signature) and returns a module that:
   - [include]s [Make (S)] — every generic-key Lwt-wrapped operation is
     available;
   - additionally exposes Lwt-wrapped versions of the pack-unix
     extensions: integrity check, GC, snapshots, split/reload/flush,
     [create_one_commit_store]. *)
module Pack = struct
  module Make (S : Irmin_pack_io.S) = struct
    include Make (S)

    let integrity_check ?ppf ?heads ~auto_repair r =
      run_eio (fun () -> S.integrity_check ?ppf ?heads ~auto_repair r)

    let integrity_check_inodes ?heads r =
      run_eio (fun () -> S.integrity_check_inodes ?heads r)

    let traverse_pack_file kind conf =
      run_eio (fun () -> S.traverse_pack_file kind conf)

    let test_traverse_pack_file kind conf =
      run_eio (fun () -> S.test_traverse_pack_file kind conf)

    let split r = run_eio (fun () -> S.split r)
    let is_split_allowed r = S.is_split_allowed r
    let add_volume r = run_eio (fun () -> S.add_volume r)
    let reload r = run_eio (fun () -> S.reload r)
    let flush r = run_eio (fun () -> S.flush r)

    let create_one_commit_store ~domain_mgr r ck path =
      run_eio (fun () -> S.create_one_commit_store ~domain_mgr r ck path)

    module Gc = struct
      type process_state = S.Gc.process_state
      type msg = S.Gc.msg

      let start_exn ~domain_mgr ?unlink r c =
        run_eio (fun () -> S.Gc.start_exn ~domain_mgr ?unlink r c)

      let finalise_exn ?wait r = run_eio (fun () -> S.Gc.finalise_exn ?wait r)

      let run ~domain_mgr ?finished r c =
        let finished =
          Option.map
            (fun lwt_f result -> Lwt_eio.Promise.await_lwt (lwt_f result))
            finished
        in
        run_eio (fun () -> S.Gc.run ~domain_mgr ?finished r c)

      let wait r = run_eio (fun () -> S.Gc.wait r)
      let cancel r = run_eio (fun () -> S.Gc.cancel r)
      let is_finished r = S.Gc.is_finished r
      let behaviour r = S.Gc.behaviour r
      let is_allowed r = S.Gc.is_allowed r
      let latest_gc_target r = S.Gc.latest_gc_target r
    end

    module Snapshot = struct
      include S.Snapshot

      let export ?on_disk r f ~root_key =
        run_eio (fun () -> S.Snapshot.export ?on_disk r f ~root_key)
    end
  end
end
