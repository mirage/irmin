open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of
    {!Content_addressable.S} that uses an append-only file of variable-length
    data blocks. The data file is indexed by hash via {!Pack_index.S}
    implementation. *)
module type S = sig
  include Indexable.S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    read t Lwt.t

  val sync : 'a t -> unit
  (** Syncs a readonly instance with the files on disk. The same file instance
      is shared between several pack instances. *)

  val flush : ?index:bool -> ?index_merge:bool -> 'a t -> unit
  val offset : 'a t -> int63

  val clear_caches : 'a t -> unit
  (** [clear_cache t] clears all the in-memory caches of [t]. Persistent data
      are not removed. *)

  (** @inline *)
  include S.Checkable with type 'a t := 'a t and type hash := hash

  val debug_block : 'a t -> IO.Unix.t
  (** For debug use: reveal underlying IO *)
end

module type Maker = sig
  type hash
  type index

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)

  module Make
      (V : Pack_value.Persistent
             with type hash := hash
              and type key := hash Pack_key.t) :
    S
      with type key = hash Pack_key.t
       and type hash = hash
       and type value = V.t
       and type index := index
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  val selected_version : Version.t

  module Maker
      (Index : Pack_index.S)
      (Hash : Irmin.Hash.S with type t = Index.key) :
    Maker with type hash = Hash.t and type index := Index.t
end
