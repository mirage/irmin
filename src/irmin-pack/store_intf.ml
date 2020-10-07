(** [Irmin-pack]-specific extensions to the [Store] module type. *)
module type S = sig
  type repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
  (** Checks the integrity of the repository. if [auto_repair] is [true], will
      also try to fix the issues. [ppf] is a formatter for progressive
      reporting. [`Fixed] and [`Corrupted] report the number of fixed/corrupted
      entries. *)

  val sync : repo -> unit
  (** [sync t] syncs a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)

  val clear : repo -> unit Lwt.t
  (** [clear t] removes all the data persisted in [t]. This operations provides
      snapshot isolation guarantees for read-only instances: read-only instance
      will continue to see all the data until they explicitely call {!sync}. *)

  val migrate : Irmin.config -> unit
  (** [migrate conf] upgrades the repository with configuration [conf] to use
      the latest storage format.

      {b Note:} performing concurrent store operations during the migration, or
      attempting to use pre-migration instances of the repository after the
      migration is complete, will result in undefined behaviour. *)

  val flush : repo -> unit
  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)
end

module type Store = sig
  module type S = S

  module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) :
    S.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val migrate : Irmin.config -> unit

  exception Unsupported_version of IO.version

  module Checks (Index : Pack_index.S) : sig
    val integrity_check :
      ?ppf:Format.formatter ->
      auto_repair:bool ->
      check:
        (kind:[ `Contents | `Node | `Commit ] ->
        offset:int64 ->
        length:int ->
        Index.key ->
        (unit, [ `Absent_value | `Wrong_hash ]) result) ->
      Index.t ->
      ( [> `Fixed of int | `No_error ],
        [> `Cannot_fix of string | `Corrupted of int ] )
      result
  end
end
