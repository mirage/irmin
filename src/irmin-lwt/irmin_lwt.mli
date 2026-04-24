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
  type path = S.path
  type contents = S.contents
  type tree = S.tree
  type commit = S.commit
  type branch = S.branch
  type info = S.info
  type hash = S.hash
  type write_error = S.write_error

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

  val merge_into :
    into:t -> info:S.Info.f -> t -> (unit, Irmin.Merge.conflict) result Lwt.t

  val last_modified : ?depth:int -> ?n:int -> t -> path -> commit list Lwt.t

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
  end
end
