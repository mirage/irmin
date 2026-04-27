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

module type S = sig
  (** {1 Schema} *)

  module Schema : Irmin.Schema.S

  (** {1 Types} *)

  type repo
  type t
  type step = Schema.Path.step
  type path = Schema.Path.t
  type metadata = Schema.Metadata.t
  type contents = Schema.Contents.t
  type node
  type tree
  type commit
  type branch = Schema.Branch.t
  type slice
  type info = Schema.Info.t
  type hash = Schema.Hash.t
  type contents_key
  type node_key
  type commit_key
  type lca_error = [ `Max_depth_reached | `Too_many_lcas ]
  type ff_error = [ `No_change | `Rejected | lca_error ]

  type write_error =
    [ Irmin.Merge.conflict
    | `Too_many_retries of int
    | `Test_was of tree option ]

  type kinded_key = [ `Contents of contents_key | `Node of node_key ]
  type watch

  (** {1 Type-level submodules} *)

  module Info : Irmin.Info.S with type t = info
  module Hash : Irmin.Hash.S with type t = hash
  module Path : Irmin.Path.S with type t = path and type step = step
  module Metadata : Irmin.Metadata.S with type t = metadata

  module Backend :
    Irmin.Backend.S
      with module Schema = Schema
      with type Slice.t = slice
       and type Repo.t = repo
       and type Contents.key = contents_key
       and type Node.key = node_key
       and type Commit.key = commit_key

  module Contents : sig
    include Irmin.Contents.S with type t = contents

    val hash : contents -> hash
    val of_key : repo -> contents_key -> contents option Lwt.t
    val of_hash : repo -> hash -> contents option Lwt.t
  end

  module History : Graph.Sig.P with type V.t = commit

  module Status : sig
    type t = [ `Empty | `Branch of branch | `Commit of commit ]

    val t : repo -> t Irmin.Type.t
    val pp : t Fmt.t
  end

  type Irmin.remote +=
    | E of Backend.Remote.endpoint
          (** Extends [Irmin.remote] with the endpoint type of [Backend]. *)

  (** {1 Repositories} *)

  module Repo : sig
    type nonrec t = repo

    type elt =
      [ `Commit of commit_key
      | `Node of node_key
      | `Contents of contents_key
      | `Branch of branch ]

    val elt_t : elt Irmin.Type.t
    val v : Irmin.Backend.Conf.t -> t Lwt.t
    val close : t -> unit Lwt.t
    val heads : t -> commit list Lwt.t
    val branches : t -> branch list Lwt.t
    val config : t -> Irmin.Backend.Conf.t

    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:[ `Head | `Max of commit list ] ->
      t ->
      slice Lwt.t

    val import : t -> slice -> (unit, [ `Msg of string ]) result Lwt.t
    val default_pred_commit : t -> commit_key -> elt list
    val default_pred_node : t -> node_key -> elt list
    val default_pred_contents : t -> contents_key -> elt list

    val iter :
      ?cache_size:int ->
      min:elt list ->
      max:elt list ->
      ?edge:(elt -> elt -> unit Lwt.t) ->
      ?branch:(branch -> unit Lwt.t) ->
      ?commit:(commit_key -> unit Lwt.t) ->
      ?node:(node_key -> unit Lwt.t) ->
      ?contents:(contents_key -> unit Lwt.t) ->
      ?skip_branch:(branch -> bool Lwt.t) ->
      ?skip_commit:(commit_key -> bool Lwt.t) ->
      ?skip_node:(node_key -> bool Lwt.t) ->
      ?skip_contents:(contents_key -> bool Lwt.t) ->
      ?pred_branch:(t -> branch -> elt list Lwt.t) ->
      ?pred_commit:(t -> commit_key -> elt list Lwt.t) ->
      ?pred_node:(t -> node_key -> elt list Lwt.t) ->
      ?pred_contents:(t -> contents_key -> elt list Lwt.t) ->
      ?rev:bool ->
      t ->
      unit Lwt.t

    val breadth_first_traversal :
      ?cache_size:int ->
      max:elt list ->
      ?branch:(branch -> unit Lwt.t) ->
      ?commit:(commit_key -> unit Lwt.t) ->
      ?node:(node_key -> unit Lwt.t) ->
      ?contents:(contents_key -> unit Lwt.t) ->
      ?pred_branch:(t -> branch -> elt list Lwt.t) ->
      ?pred_commit:(t -> commit_key -> elt list Lwt.t) ->
      ?pred_node:(t -> node_key -> elt list Lwt.t) ->
      ?pred_contents:(t -> contents_key -> elt list Lwt.t) ->
      t ->
      unit Lwt.t
  end

  (** {1 Stores} *)

  val main : repo -> t Lwt.t

  val master : repo -> t Lwt.t
  [@@ocaml.deprecated "Use `main` instead."]
  (** Deprecated alias kept for Irmin 3 compatibility. Use {!main}. *)

  val of_branch : repo -> branch -> t Lwt.t
  val of_commit : commit -> t Lwt.t
  val empty : repo -> t Lwt.t
  val repo : t -> repo
  val tree : t -> tree
  val status : t -> [ `Empty | `Branch of branch | `Commit of commit ]

  (** {2 Reads} *)

  val find : t -> path -> contents option Lwt.t
  val find_all : t -> path -> (contents * metadata) option Lwt.t
  val mem : t -> path -> bool Lwt.t
  val mem_tree : t -> path -> bool Lwt.t
  val get : t -> path -> contents Lwt.t
  val get_all : t -> path -> (contents * metadata) Lwt.t
  val find_tree : t -> path -> tree option Lwt.t
  val get_tree : t -> path -> tree Lwt.t
  val hash : t -> path -> hash option Lwt.t
  val kind : t -> path -> [ `Contents | `Node ] option Lwt.t
  val list : t -> path -> (step * tree) list Lwt.t
  val key : t -> path -> kinded_key option Lwt.t

  (** {2 Writes} *)

  val set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    contents ->
    (unit, write_error) result Lwt.t

  val set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    contents ->
    unit Lwt.t

  val set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    tree ->
    (unit, write_error) result Lwt.t

  val set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    tree ->
    unit Lwt.t

  val remove :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    (unit, write_error) result Lwt.t

  val remove_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    unit Lwt.t

  val test_and_set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    (unit, write_error) result Lwt.t

  val test_and_set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    unit Lwt.t

  val test_and_set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result Lwt.t

  val test_and_set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    unit Lwt.t

  val test_set_and_get :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    (commit option, write_error) result Lwt.t

  val test_set_and_get_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    commit option Lwt.t

  val test_set_and_get_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    (commit option, write_error) result Lwt.t

  val test_set_and_get_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    commit option Lwt.t

  val merge :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    path ->
    contents option ->
    (unit, write_error) result Lwt.t

  val merge_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    path ->
    contents option ->
    unit Lwt.t

  val merge_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    path ->
    tree option ->
    (unit, write_error) result Lwt.t

  val merge_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    path ->
    tree option ->
    unit Lwt.t

  val with_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    path ->
    (tree option -> tree option) ->
    (unit, write_error) result Lwt.t

  val with_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    path ->
    (tree option -> tree option) ->
    unit Lwt.t

  val clone : src:t -> dst:branch -> t Lwt.t

  (** {2 Merges and ancestors} *)

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Irmin.Merge.conflict) result Lwt.t
  (** Lwt-wrapped abbreviation for merge-into-something functions. *)

  val merge_into : into:t -> t merge
  val merge_with_branch : t -> branch merge
  val merge_with_commit : t -> commit merge

  val lcas :
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result Lwt.t

  val lcas_with_branch :
    t ->
    ?max_depth:int ->
    ?n:int ->
    branch ->
    (commit list, lca_error) result Lwt.t

  val lcas_with_commit :
    t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (commit list, lca_error) result Lwt.t

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t Lwt.t

  val last_modified : ?depth:int -> ?n:int -> t -> path -> commit list Lwt.t

  (** {2 Backend converters} *)

  val of_backend_node : repo -> Backend.Node.value -> node
  val to_backend_node : node -> Backend.Node.value
  val to_backend_portable_node : node -> Backend.Node_portable.t
  val to_backend_commit : commit -> Backend.Commit.value

  val of_backend_commit :
    repo -> Backend.Commit.Key.t -> Backend.Commit.value -> commit

  val save_contents :
    [> Irmin.Perms.write ] Backend.Contents.t -> contents -> contents_key Lwt.t

  val save_tree :
    ?clear:bool ->
    repo ->
    [> Irmin.Perms.write ] Backend.Contents.t ->
    [> Irmin.Perms.read_write ] Backend.Node.t ->
    tree ->
    kinded_key Lwt.t

  (** {1 Trees} *)

  module Tree : sig
    type nonrec t = tree
    type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]

    type kinded_key =
      [ `Contents of contents_key * metadata | `Node of node_key ]

    type elt = [ `Node of node | `Contents of contents * metadata ]
    type marks

    type depth =
      [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]

    type stats

    val stats_t : stats Irmin.Type.t

    type concrete =
      [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

    type 'a force_lwt = [ `True | `False of path -> 'a -> 'a Lwt.t ]
    type uniq = [ `False | `True | `Marks of marks ]
    type ('a, 'b) folder_lwt = path -> 'b -> 'a -> 'a Lwt.t

    type error =
      [ `Dangling_hash of hash | `Pruned_hash of hash | `Portable_value ]

    type 'a or_error = ('a, error) result

    (** Operations on lazy tree contents. *)
    module Contents : sig
      type nonrec t

      val hash : ?cache:bool -> t -> hash
      val key : t -> contents_key option
      val force : t -> contents or_error Lwt.t
      val force_exn : t -> contents Lwt.t
      val clear : t -> unit
    end

    val empty : unit -> t
    val singleton : path -> ?metadata:metadata -> contents -> t
    val of_contents : ?metadata:metadata -> contents -> t
    val of_node : node -> t
    val v : elt -> t
    val pruned : kinded_hash -> t
    val is_empty : t -> bool
    val destruct : t -> [ `Node of node | `Contents of Contents.t * metadata ]
    val hash : ?cache:bool -> t -> hash
    val kinded_hash : ?cache:bool -> t -> kinded_hash
    val key : t -> kinded_key option
    val shallow : Repo.t -> kinded_key -> t
    val clear : ?depth:int -> t -> unit
    val pp : t Irmin.Type.pp
    val kind : t -> path -> [ `Contents | `Node ] option Lwt.t
    val diff : t -> t -> (path * (contents * metadata) Irmin.Diff.t) list Lwt.t
    val mem : t -> path -> bool Lwt.t
    val find_all : t -> path -> (contents * metadata) option Lwt.t
    val length : t -> ?cache:bool -> path -> int Lwt.t
    val find : t -> path -> contents option Lwt.t
    val get_all : t -> path -> (contents * metadata) Lwt.t
    val get : t -> path -> contents Lwt.t

    val list :
      t ->
      ?offset:int ->
      ?length:int ->
      ?cache:bool ->
      path ->
      (step * t) list Lwt.t

    val seq :
      t ->
      ?offset:int ->
      ?length:int ->
      ?cache:bool ->
      path ->
      (step * t) Seq.t Lwt.t

    val add : t -> path -> ?metadata:metadata -> contents -> t Lwt.t

    val update :
      t ->
      path ->
      ?metadata:metadata ->
      (contents option -> contents option) ->
      t Lwt.t

    val remove : t -> path -> t Lwt.t
    val mem_tree : t -> path -> bool Lwt.t
    val find_tree : t -> path -> t option Lwt.t
    val get_tree : t -> path -> t Lwt.t
    val add_tree : t -> path -> t -> t Lwt.t
    val update_tree : t -> path -> (t option -> t option) -> t Lwt.t
    val of_concrete : concrete -> t
    val stats : ?force:bool -> t -> stats Lwt.t
    val to_concrete : t -> concrete Lwt.t
    val find_key : Repo.t -> t -> kinded_key option Lwt.t
    val of_key : Repo.t -> kinded_key -> t option Lwt.t
    val of_hash : Repo.t -> kinded_hash -> t option Lwt.t

    (** {2 Fold} *)

    val empty_marks : unit -> marks

    val fold :
      ?order:[ `Sorted | `Undefined | `Random of Random.State.t ] ->
      ?force:'a force_lwt ->
      ?cache:bool ->
      ?uniq:uniq ->
      ?pre:('a, step list) folder_lwt ->
      ?post:('a, step list) folder_lwt ->
      ?depth:depth ->
      ?contents:('a, contents) folder_lwt ->
      ?node:('a, node) folder_lwt ->
      ?tree:('a, t) folder_lwt ->
      t ->
      'a ->
      'a Lwt.t
  end

  (** {1 Commits} *)

  module Commit : sig
    type nonrec t = commit
    type nonrec commit_key = commit_key

    val tree : t -> tree
    val parents : t -> commit_key list
    val info : t -> info
    val hash : t -> hash
    val key : t -> commit_key
    val pp : t Fmt.t
    val pp_hash : t Fmt.t

    val v :
      ?clear:bool ->
      Repo.t ->
      info:info ->
      parents:commit_key list ->
      tree ->
      t Lwt.t

    val of_key : Repo.t -> commit_key -> t option Lwt.t
    val of_hash : Repo.t -> hash -> t option Lwt.t
  end

  (** {1 Branches} *)

  module Branch : sig
    type nonrec t = branch

    val mem : Repo.t -> t -> bool Lwt.t
    val find : Repo.t -> t -> commit option Lwt.t
    val get : Repo.t -> t -> commit Lwt.t
    val set : Repo.t -> t -> commit -> unit Lwt.t
    val remove : Repo.t -> t -> unit Lwt.t
    val list : Repo.t -> t list Lwt.t
    val pp : t Fmt.t

    val watch :
      Repo.t ->
      t ->
      ?init:commit ->
      (commit Irmin.Diff.t -> unit Lwt.t) ->
      watch Lwt.t

    val watch_all :
      Repo.t ->
      ?init:(t * commit) list ->
      (t -> commit Irmin.Diff.t -> unit Lwt.t) ->
      watch Lwt.t
  end

  (** {1 Heads} *)

  module Head : sig
    val list : Repo.t -> commit list Lwt.t
    val find : t -> commit option Lwt.t
    val get : t -> commit Lwt.t
    val set : t -> commit -> unit Lwt.t

    val fast_forward :
      t ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      ( unit,
        [ `No_change | `Rejected | `Max_depth_reached | `Too_many_lcas ] )
      result
      Lwt.t

    val test_and_set :
      t -> test:commit option -> set:commit option -> bool Lwt.t

    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Irmin.Merge.conflict) result Lwt.t
  end

  (** {1 Watches} *)

  val watch :
    t -> ?init:commit -> (commit Irmin.Diff.t -> unit Lwt.t) -> watch Lwt.t

  val watch_key :
    t ->
    path ->
    ?init:commit ->
    ((commit * tree) Irmin.Diff.t -> unit Lwt.t) ->
    watch Lwt.t

  val unwatch : watch -> unit Lwt.t

  (** {1 Type descriptors} *)

  val step_t : step Irmin.Type.t
  val path_t : path Irmin.Type.t
  val metadata_t : metadata Irmin.Type.t
  val contents_t : contents Irmin.Type.t
  val node_t : node Irmin.Type.t
  val tree_t : tree Irmin.Type.t
  val hash_t : hash Irmin.Type.t
  val branch_t : branch Irmin.Type.t
  val slice_t : slice Irmin.Type.t
  val info_t : info Irmin.Type.t
  val lca_error_t : lca_error Irmin.Type.t
  val ff_error_t : ff_error Irmin.Type.t
  val contents_key_t : contents_key Irmin.Type.t
  val node_key_t : node_key Irmin.Type.t
  val commit_key_t : commit_key Irmin.Type.t
  val write_error_t : write_error Irmin.Type.t
  val commit_t : repo -> commit Irmin.Type.t
end

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
  module History = S.History
  module Status = S.Status

  module Contents = struct
    include (S.Contents : Irmin.Contents.S with type t = S.contents)

    let hash = S.Contents.hash
    let of_key r k = run_eio (fun () -> S.Contents.of_key r k)
    let of_hash r h = run_eio (fun () -> S.Contents.of_hash r h)
  end

  module Repo = struct
    type nonrec t = repo
    type elt = S.Repo.elt

    let elt_t = S.Repo.elt_t
    let v config = run_eio (fun () -> S.Repo.v config)
    let close r = run_eio (fun () -> S.Repo.close r)
    let heads r = run_eio (fun () -> S.Repo.heads r)
    let branches r = run_eio (fun () -> S.Repo.branches r)
    let config r = S.Repo.config r

    let export ?full ?depth ?min ?max r =
      run_eio (fun () -> S.Repo.export ?full ?depth ?min ?max r)

    let import t s = run_eio (fun () -> S.Repo.import t s)

    (* Pure: no lazy loading. *)
    let default_pred_commit = S.Repo.default_pred_commit
    let default_pred_node = S.Repo.default_pred_node
    let default_pred_contents = S.Repo.default_pred_contents

    (* Helpers to bridge the Lwt-returning callbacks of [iter] and
       [breadth_first_traversal] to the direct-style callbacks that the
       underlying Irmin 4 function expects. *)
    let lift_cb1 = function
      | None -> None
      | Some f -> Some (fun x -> Lwt_eio.Promise.await_lwt (f x))

    let lift_cb2 = function
      | None -> None
      | Some f -> Some (fun x y -> Lwt_eio.Promise.await_lwt (f x y))

    let iter ?cache_size ~min ~max ?edge ?branch ?commit ?node ?contents
        ?skip_branch ?skip_commit ?skip_node ?skip_contents ?pred_branch
        ?pred_commit ?pred_node ?pred_contents ?rev t =
      let edge = lift_cb2 edge in
      let branch = lift_cb1 branch in
      let commit = lift_cb1 commit in
      let node = lift_cb1 node in
      let contents = lift_cb1 contents in
      let skip_branch = lift_cb1 skip_branch in
      let skip_commit = lift_cb1 skip_commit in
      let skip_node = lift_cb1 skip_node in
      let skip_contents = lift_cb1 skip_contents in
      let pred_branch = lift_cb2 pred_branch in
      let pred_commit = lift_cb2 pred_commit in
      let pred_node = lift_cb2 pred_node in
      let pred_contents = lift_cb2 pred_contents in
      run_eio (fun () ->
          S.Repo.iter ?cache_size ~min ~max ?edge ?branch ?commit ?node
            ?contents ?skip_branch ?skip_commit ?skip_node ?skip_contents
            ?pred_branch ?pred_commit ?pred_node ?pred_contents ?rev t)

    let breadth_first_traversal ?cache_size ~max ?branch ?commit ?node ?contents
        ?pred_branch ?pred_commit ?pred_node ?pred_contents t =
      let branch = lift_cb1 branch in
      let commit = lift_cb1 commit in
      let node = lift_cb1 node in
      let contents = lift_cb1 contents in
      let pred_branch = lift_cb2 pred_branch in
      let pred_commit = lift_cb2 pred_commit in
      let pred_node = lift_cb2 pred_node in
      let pred_contents = lift_cb2 pred_contents in
      run_eio (fun () ->
          S.Repo.breadth_first_traversal ?cache_size ~max ?branch ?commit ?node
            ?contents ?pred_branch ?pred_commit ?pred_node ?pred_contents t)
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

    (* Polymorphic variants: declared transparently here. They are
       structurally identical to the upstream [S.Tree.X] versions, so
       values flow through without coercion thanks to polymorphic-variant
       subtyping. *)
    type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]

    type kinded_key =
      [ `Contents of contents_key * metadata | `Node of node_key ]

    type elt = [ `Node of node | `Contents of contents * metadata ]

    type concrete =
      [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

    (* [stats] is a record. We keep it as an alias of [S.Tree.stats] so
       it remains nominally compatible. Field access is exposed through
       [stats_t] / [Irmin.Type] introspection. *)
    type stats = S.Tree.stats

    let stats_t = S.Tree.stats_t

    type error = S.Tree.error
    type 'a or_error = ('a, error) result

    module Contents = struct
      include (
        S.Tree.Contents :
          module type of struct
            include S.Tree.Contents
          end
          with type t = S.Tree.Contents.t)

      let force c = run_eio (fun () -> S.Tree.Contents.force c)
      let force_exn c = run_eio (fun () -> S.Tree.Contents.force_exn c)
    end

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
