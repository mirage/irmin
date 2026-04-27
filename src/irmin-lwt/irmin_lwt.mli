(** Lwt compatibility layer for Irmin 4.

    This package lets Irmin 3 (Lwt-based) consumers continue to use a monadic
    [Lwt.t] API while the backend is Irmin 4 (direct-style Eio). It is a
    transitional shim: new code should use [Irmin] directly.

    See [doc/migration-from-irmin-3.md] for a migration walkthrough. *)

val run : (unit -> 'a Lwt.t) -> 'a
(** [run f] sets up the Eio runtime and the [lwt_eio] bridge, runs [f ()] to
    completion, and returns its result. This is the drop-in replacement for
    [Lwt_main.run] in Irmin 3 client code.

    Intended for top-level [let () = Irmin_lwt.run main] style usage in Irmin
    3-era programs being migrated to Irmin 4. *)

val run_with_env : < clock : _ Eio.Time.clock ; .. > -> (unit -> 'a Lwt.t) -> 'a
(** [run_with_env env f] is like {!run} but reuses an existing Eio environment
    instead of calling [Eio_main.run] internally. Useful when the client is
    already inside an Eio event loop. *)

(** The Lwt-flavoured counterpart of [Irmin.Generic_key.S].

    Every I/O-triggering operation of [Irmin.Generic_key.S] is replaced by a
    version returning ['_ Lwt.t]; type-level submodules (Schema, Info, Hash,
    Path, Metadata, Backend, Contents, History, Status) are kept so downstream
    consumers can write [Irmin_lwt.S with module Schema = …] the same way they
    would write [Irmin.Generic_key.S with module Schema = …].

    See {!Make} for the functor that produces a module conforming to [S] from an
    arbitrary [Irmin.Generic_key.S]. *)
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
    (** Tree statistics. The record fields ([nodes], [leafs], [skips], [depth],
        [width]) cannot be exposed through the functor boundary, but the
        [Irmin.Type.t] descriptor [stats_t] gives field access via [Irmin.Type]
        introspection. *)

    val stats_t : stats Irmin.Type.t

    type concrete =
      [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

    type 'a force_lwt = [ `True | `False of path -> 'a -> 'a Lwt.t ]
    type uniq = [ `False | `True | `Marks of marks ]
    type ('a, 'b) folder_lwt = path -> 'b -> 'a -> 'a Lwt.t

    type error =
      [ `Dangling_hash of hash | `Pruned_hash of hash | `Portable_value ]
    (** Errors that can be raised when forcing a lazy tree value. *)

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

    (** {2 Merge} *)

    val merge : t Irmin.Merge.t

    (** {2 Performance counters and inspection} *)

    type counters

    val counters : unit -> counters
    val dump_counters : unit Fmt.t
    val reset_counters : unit -> unit

    val inspect :
      t ->
      [ `Contents
      | `Node of [ `Map | `Key | `Value | `Portable_dirty | `Pruned ] ]
    (** [inspect t] is similar to {!val-kind}, with extra state information
        returned for nodes. Pure: no I/O. *)
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

module Make (S : Irmin.Generic_key.S) :
  S
    with module Schema = S.Schema
     and type repo = S.repo
     and type t = S.t
     and type node = S.node
     and type tree = S.tree
     and type commit = S.commit
     and type slice = S.slice
     and type contents_key = S.contents_key
     and type node_key = S.node_key
     and type commit_key = S.commit_key
     and type lca_error = S.lca_error
     and type ff_error = S.ff_error
     and type write_error = S.write_error
     and type watch = S.watch
     and module Info = S.Info
     and module Hash = S.Hash
     and module Path = S.Path
     and module Metadata = S.Metadata
     and module Backend = S.Backend
     and module History = S.History
     and type Repo.elt = S.Repo.elt
     and type Tree.kinded_hash = S.Tree.kinded_hash
     and type Tree.kinded_key = S.Tree.kinded_key
     and type Tree.elt = S.Tree.elt
     and type Tree.marks = S.Tree.marks
     and type Tree.depth = S.Tree.depth
     and type Tree.stats = S.Tree.stats
     and type Tree.concrete = S.Tree.concrete

(** Lwt wrappers for [irmin-pack-unix]-specific operations.

    [Pack.Make] takes an [Irmin_pack_io.S] (the full pack-unix store signature)
    and returns a module that includes the result of the generic [Make] functor
    plus Lwt-wrapped versions of the pack-unix extensions: integrity check, GC,
    snapshots, split/reload/flush, [create_one_commit_store]. *)
module Pack : sig
  module Make (S : Irmin_pack_io.S) : sig
    include module type of Make (S)

    val integrity_check :
      ?ppf:Format.formatter ->
      ?heads:commit list ->
      auto_repair:bool ->
      repo ->
      ( [> `Fixed of int | `No_error ],
        [> `Cannot_fix of string | `Corrupted of int ] )
      result
      Lwt.t

    val integrity_check_inodes :
      ?heads:commit list ->
      repo ->
      ([> `No_error ], [> `Cannot_fix of string ]) result Lwt.t

    val traverse_pack_file :
      [ `Reconstruct_index of [ `In_place | `Output of string ]
      | `Check_index
      | `Check_and_fix_index ] ->
      Irmin.config ->
      unit Lwt.t

    val test_traverse_pack_file :
      [ `Reconstruct_index of [ `In_place | `Output of string ]
      | `Check_index
      | `Check_and_fix_index ] ->
      Irmin.config ->
      unit Lwt.t

    val split : repo -> unit Lwt.t
    val is_split_allowed : repo -> bool
    val add_volume : repo -> unit Lwt.t
    val reload : repo -> unit Lwt.t
    val flush : repo -> unit Lwt.t

    val create_one_commit_store :
      domain_mgr:_ Eio.Domain_manager.t ->
      repo ->
      commit_key ->
      Eio.Fs.dir_ty Eio.Path.t ->
      unit Lwt.t

    module Gc : sig
      type process_state = S.Gc.process_state
      type msg = S.Gc.msg

      val start_exn :
        domain_mgr:_ Eio.Domain_manager.t ->
        ?unlink:bool ->
        repo ->
        commit_key ->
        bool Lwt.t

      val finalise_exn : ?wait:bool -> repo -> process_state Lwt.t

      val run :
        domain_mgr:_ Eio.Domain_manager.t ->
        ?finished:
          ((Irmin_pack_io.Stats.Latest_gc.stats, msg) result -> unit Lwt.t) ->
        repo ->
        commit_key ->
        (bool, msg) result Lwt.t

      val wait :
        repo -> (Irmin_pack_io.Stats.Latest_gc.stats option, msg) result Lwt.t

      val cancel : repo -> bool Lwt.t
      val is_finished : repo -> bool
      val behaviour : repo -> [ `Archive | `Delete ]
      val is_allowed : repo -> bool
      val latest_gc_target : repo -> commit_key option
    end

    module Snapshot : sig
      include module type of S.Snapshot

      val export :
        ?on_disk:[ `Path of Eio.Fs.dir_ty Eio.Path.t ] ->
        repo ->
        (t -> unit) ->
        root_key:Tree.kinded_key ->
        int Lwt.t
    end
  end
end
