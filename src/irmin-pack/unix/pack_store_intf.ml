open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of {!Indexable.S}
    that uses an append-only file of variable-length data blocks.

    Certain values in the data file are indexed by hash via a {!Pack_index.S}
    implementation, but not all of them need be. *)
module type S = sig
  include Irmin_pack.Indexable.S
  (* sig
   *   type -'a t
   *   type key
   *   type value
   *   val mem : [> Irmin__Import.read ] t -> key -> bool Lwt.t
   *   val find : [> Irmin__Import.read ] t -> key -> value option Lwt.t
   *   val close : 'a t -> unit Lwt.t
   *   type hash
   *   val index : [> Irmin__Import.read ] t -> hash -> key option Lwt.t
   *   val batch :
   *     Irmin__Import.read t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
   *   module Key :
   *     sig
   *       type t = key
   *       val t : t Irmin__Type.t
   *       type hash = hash
   *       val to_hash : t -> hash/2
   *     end
   *   val add : 'a t -> value -> key Lwt.t
   *   val unsafe_add : 'a t -> hash -> value -> key Lwt.t
   *   val index_direct : 'a t -> hash -> key option
   *   val unsafe_append :
   *     ensure_unique:bool -> overcommit:bool -> 'a t -> hash -> value -> key
   *   val unsafe_mem : 'a t -> key -> bool
   *   val unsafe_find : check_integrity:bool -> 'a t -> key -> value option
   * end *)

  type file_manager

  val v : config:Irmin.Backend.Conf.t -> fm:file_manager -> read t Lwt.t

  val cast : read t -> read_write t

  (* val sync : 'a t -> unit
   * (\** Syncs a readonly instance with the files on disk. The same file instance
   *     is shared between several pack instances. *\)
   *
   * val flush : ?index:bool -> ?index_merge:bool -> 'a t -> unit
   * val offset : 'a t -> int63 *)

  (** @inline *)
  include Irmin_pack.S.Checkable with type 'a t := 'a t and type hash := hash
  (* sig
   *   type 'a t
   *   type hash
   *   val integrity_check :
   *     offset:Irmin_pack__Import.int63 ->
   *     length:int ->
   *     hash -> 'a t -> (unit, [ `Absent_value | `Wrong_hash ]) result
   * end *)

  module Entry_prefix : sig
    type t = {
      hash : hash;
      kind : Pack_value.Kind.t;
      size_of_value_and_length_header : int option;
          (** Remaining bytes in the entry after reading the hash and the kind
              (i.e. the length of the length header + the value of the length
              header), if the entry has a length header (otherwise [None]).

              NOTE: the length stored in the index and in direct pack keys is
              the {!total_entry_length} (including the hash and the kind). *)
    }

    val total_entry_length : t -> int option
  end

  val read_and_decode_entry_prefix :
    off:int63 -> io_read:(off:int63 -> bytes -> int) -> Entry_prefix.t
  (** Read the entry prefix at offset [off]. *)

  val index_direct_with_kind : 'a t -> hash -> (key * Pack_value.Kind.t) option
  (** Returns the key and the kind of an object indexed by hash. *)
end

module type Sigs = sig
  module type S = S

  module Make
      (Fm : File_manager.S)
      (Hash : Irmin.Hash.S with type t = Fm.Index.key)
      (Val : Pack_value.Persistent
               with type hash := Hash.t
                and type key := Hash.t Pack_key.t) :
    S
      with type key = Hash.t Pack_key.t
       and type hash = Hash.t
       and type value = Val.t
       and type file_manager = Fm.t
end
