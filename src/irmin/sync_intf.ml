module type S = sig
  (** {1 Remote synchronization} *)

  type t
  (** The type for store handles. *)

  type commit
  (** The type for store heads. *)

  type branch
  (** The type for branch IDs. *)

  type endpoint
  (** The type for sync endpoints. *)

  val fetch :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (commit option, [ `Msg of string ]) result Lwt.t
  (** [fetch t uri] fetches the contents of the remote store located at [uri]
      into the local store [t]. Return the head of the remote branch with the
      same name, which is now in the local store. [No_head] means no such branch
      exists. *)

  val push :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
  (** [push t uri] pushes the contents of the local store [t] into the remote
      store located at [uri]. *)
end

module type Sync = sig
  module type S = S

  (** Provides stub implementations of the {!S} that always returns [Error] when
      push/pull operations are attempted. *)
  module None (H : Type.S) (R : Type.S) : sig
    include
      S with type commit = H.t and type branch = R.t and type endpoint = unit

    val v : 'a -> t Lwt.t
    (** Create a remote store handle. *)
  end
end
