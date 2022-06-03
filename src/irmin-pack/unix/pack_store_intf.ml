open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of {!Indexable.S}
    that uses an append-only file of variable-length data blocks.

    Certain values in the data file are indexed by hash via a {!Pack_index.S}
    implementation, but not all of them need be. *)
module type S = sig
  include Irmin_pack.Indexable.S

  type index

  val v :
    readonly:bool ->
    lru_size:int ->
    index:index ->
    indexing_strategy:Irmin_pack.Indexing_strategy.t ->
    dict:Pack_dict.t ->
    io:Io_legacy.Unix.t ->
    read t Lwt.t

  val sync : 'a t -> unit
  (** Syncs a readonly instance with the files on disk. The same file instance
      is shared between several pack instances. *)

  val flush : ?index:bool -> ?index_merge:bool -> 'a t -> unit
  val offset : 'a t -> int63

  (** @inline *)
  include Irmin_pack.S.Checkable with type 'a t := 'a t and type hash := hash

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
      (Index : Pack_index.S)
      (Hash : Irmin.Hash.S with type t = Index.key)
      (V : Pack_value.Persistent
             with type hash := Hash.t
              and type key := Hash.t Pack_key.t) :
    S
      with type key = Hash.t Pack_key.t
       and type hash = Hash.t
       and type value = V.t
       and type index := Index.t
end
