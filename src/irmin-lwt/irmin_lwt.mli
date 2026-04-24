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

module Make (S : Irmin.Generic_key.S) : sig
  (** [Make(S)] wraps every I/O-performing operation of [S] so that it returns
      an ['a Lwt.t] value. The wrappers thread each call through
      [Lwt_eio.run_eio], which runs the direct-style body on the current Eio
      scheduler. The caller must therefore be running inside an Eio event loop
      with an active [lwt_eio] bridge — see [Irmin_lwt.run] for a convenience
      entry point that sets both up. *)

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

  (** Type-level modules of [S], forwarded as-is. They carry no I/O and do not
      need Lwt wrapping.

      The [module type of struct include S.X end] idiom is used instead of the
      shorter [module type of S.X] because the latter produces fresh abstract
      types: [Backend.Contents.t] would not be definitionally equal to
      [S.Backend.Contents.t], which breaks downstream functors (e.g. Tezos'
      [Tezos_context_helpers.Context.Make_tree]) that receive values of both
      types. *)
  module Schema : module type of struct
    include S.Schema
  end

  module Info : module type of struct
    include S.Info
  end

  module Hash : module type of struct
    include S.Hash
  end

  module Path : module type of struct
    include S.Path
  end

  module Metadata : module type of struct
    include S.Metadata
  end

  module Backend : module type of struct
    include S.Backend
  end

  module Contents : module type of struct
    include S.Contents
  end

  module History : module type of struct
    include S.History
  end

  module Status : module type of struct
    include S.Status
  end

  module Repo : sig
    type nonrec t = repo

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
      S.slice Lwt.t
  end

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
  val find : t -> path -> contents option Lwt.t
  val find_all : t -> path -> (contents * S.metadata) option Lwt.t
  val mem : t -> path -> bool Lwt.t
  val get : t -> path -> contents Lwt.t
  val find_tree : t -> path -> tree option Lwt.t
  val get_tree : t -> path -> tree Lwt.t
  val hash : t -> path -> hash option Lwt.t

  val set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    contents ->
    (unit, write_error) result Lwt.t

  val set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    contents ->
    unit Lwt.t

  val set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    tree ->
    (unit, write_error) result Lwt.t

  val set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    tree ->
    unit Lwt.t

  val remove :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    (unit, write_error) result Lwt.t

  val remove_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
    t ->
    path ->
    unit Lwt.t

  val commit_t : repo -> commit Irmin.Type.t

  val test_and_set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
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
    info:S.Info.f ->
    t ->
    path ->
    (tree option -> tree option) ->
    unit Lwt.t

  val clone : src:t -> dst:branch -> t Lwt.t

  val merge_into :
    into:t -> info:S.Info.f -> t -> (unit, Irmin.Merge.conflict) result Lwt.t

  val merge_with_branch :
    t ->
    info:S.Info.f ->
    ?max_depth:int ->
    ?n:int ->
    branch ->
    (unit, Irmin.Merge.conflict) result Lwt.t

  val merge_with_commit :
    t ->
    info:S.Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (unit, Irmin.Merge.conflict) result Lwt.t

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

  (** {2 Backend converters}

      These translate between frontend and backend representations. They are
      pure — no I/O, no scheduler round-trip. *)

  val of_backend_node : repo -> S.Backend.Node.value -> node
  val to_backend_node : node -> S.Backend.Node.value
  val to_backend_portable_node : node -> S.Backend.Node_portable.t
  val to_backend_commit : commit -> S.Backend.Commit.value

  val of_backend_commit :
    repo -> S.Backend.Commit.Key.t -> S.Backend.Commit.value -> commit

  (** {2 Saving raw contents and trees}

      Lwt-wrapped because they persist to the backend store. *)

  val save_contents :
    [> Irmin.Perms.write ] S.Backend.Contents.t ->
    contents ->
    contents_key Lwt.t

  val save_tree :
    ?clear:bool ->
    repo ->
    [> Irmin.Perms.write ] S.Backend.Contents.t ->
    [> Irmin.Perms.read_write ] S.Backend.Node.t ->
    tree ->
    [ `Contents of contents_key | `Node of node_key ] Lwt.t

  (** Lwt-wrapped tree operations. Pure constructors and inspectors (e.g.
      {!empty}, {!is_empty}, {!hash}) are forwarded as-is; operations that might
      trigger lazy loading from the backend are threaded through
      [Lwt_eio.run_eio]. *)
  module Tree : sig
    type nonrec t = tree
    type metadata = S.metadata
    type node = S.node
    type step = S.step
    type kinded_hash = S.Tree.kinded_hash
    type kinded_key = S.Tree.kinded_key
    type elt = S.Tree.elt

    val empty : unit -> t
    val singleton : path -> ?metadata:metadata -> contents -> t
    val of_contents : ?metadata:metadata -> contents -> t
    val of_node : node -> t
    val v : elt -> t
    val pruned : kinded_hash -> t
    val is_empty : t -> bool

    val destruct :
      t -> [ `Node of node | `Contents of S.Tree.Contents.t * metadata ]

    val hash : ?cache:bool -> t -> hash
    val kinded_hash : ?cache:bool -> t -> kinded_hash
    val key : t -> kinded_key option
    val shallow : Repo.t -> kinded_key -> t
    val clear : ?depth:int -> t -> unit
    val of_concrete : S.Tree.concrete -> t
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
    val stats : ?force:bool -> t -> S.Tree.stats Lwt.t
    val to_concrete : t -> S.Tree.concrete Lwt.t
    val find_key : Repo.t -> t -> kinded_key option Lwt.t
    val of_key : Repo.t -> kinded_key -> t option Lwt.t
    val of_hash : Repo.t -> kinded_hash -> t option Lwt.t

    (** {2 Fold} *)

    type marks = S.Tree.marks

    val empty_marks : unit -> marks

    type 'a force_lwt = [ `True | `False of path -> 'a -> 'a Lwt.t ]
    (** Like {!S.Tree.force} but the [`False] callback returns an Lwt promise.
    *)

    type uniq = [ `False | `True | `Marks of marks ]
    type ('a, 'b) folder_lwt = path -> 'b -> 'a -> 'a Lwt.t
    type depth = S.Tree.depth

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
    (** [fold] is the Lwt-wrapped counterpart of [S.Tree.fold]. Every callback
        ([pre], [post], [contents], [node], [tree], and the [`False] branch of
        [force]) is expected to return an [Lwt.t] promise; the wrapper awaits
        each promise on the lwt_eio bridge before resuming the underlying
        traversal. *)
  end

  (** Lwt-wrapped commit operations. Pure accessors ([tree], [parents], [info],
      [hash], [key], [pp]) are forwarded as-is; constructors and lookups that
      might load from the backend are wrapped. *)
  module Commit : sig
    type nonrec t = commit
    type commit_key = S.commit_key

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

  type watch = S.watch
  (** Top-level watch type, used by {!watch}, {!watch_key} and the watch
      operations on {!module-Branch}. *)

  (** Lwt-wrapped branch operations. *)
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

  (** Lwt-wrapped head operations. *)
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
      info:S.Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Irmin.Merge.conflict) result Lwt.t
  end

  val watch :
    t -> ?init:commit -> (commit Irmin.Diff.t -> unit Lwt.t) -> watch Lwt.t

  val watch_key :
    t ->
    path ->
    ?init:commit ->
    ((commit * tree) Irmin.Diff.t -> unit Lwt.t) ->
    watch Lwt.t

  val unwatch : watch -> unit Lwt.t
end

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
