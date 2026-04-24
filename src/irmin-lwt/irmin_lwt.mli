(** Lwt compatibility layer for Irmin 4.

    This package lets Irmin 3 (Lwt-based) consumers continue to use a monadic
    [Lwt.t] API while the backend is Irmin 4 (direct-style Eio). It is a
    transitional shim: new code should use [Irmin] directly.

    See [doc/migration-from-irmin-3.md] for a migration walkthrough. *)

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
end
