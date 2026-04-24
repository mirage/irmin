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
  type path = S.path
  type contents = S.contents
  type tree = S.tree
  type commit = S.commit
  type branch = S.branch
  type info = S.info
  type hash = S.hash
  type write_error = S.write_error

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
  let get t p = run_eio (fun () -> S.get t p)
  let find_tree t p = run_eio (fun () -> S.find_tree t p)
  let get_tree t p = run_eio (fun () -> S.get_tree t p)
  let hash t p = run_eio (fun () -> S.hash t p)

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

  let merge_into ~into ~info t = run_eio (fun () -> S.merge_into ~into ~info t)

  let last_modified ?depth ?n t p =
    run_eio (fun () -> S.last_modified ?depth ?n t p)

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
  end
end
