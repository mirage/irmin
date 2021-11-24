open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of {!Indexable.S}
    that uses an append-only file of variable-length data blocks.

    Certain values in the data file are indexed by hash via a {!Pack_index.S}
    implementation, but not all of them need be. *)
module type S = sig
  include Indexable.S

  type index
  type indexing_strategy

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    indexing_strategy:indexing_strategy ->
    string ->
    read t Lwt.t

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  (** syncs a readonly instance with the files on disk. The same file instance
      is shared between several pack instances. Therefore only the first pack
      instance that checks a generation change, can see it.
      [on_generation_change] is a callback for all pack instances to react to a
      generation change. *)

  val flush : ?index:bool -> ?index_merge:bool -> 'a t -> unit
  val version : _ t -> Version.t
  val offset : 'a t -> int63

  val clear_caches : 'a t -> unit
  (** [clear_cache t] clears all the in-memory caches of [t]. Persistent data
      are not removed. *)

  (** @inline *)
  include S.Checkable with type 'a t := 'a t and type hash := hash
end

module type Maker = sig
  type key
  type hash
  type index
  type indexing_strategy

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)

  module Make (V : Pack_value.Persistent with type hash := hash) :
    S
      with type key = key
       and type hash = hash
       and type value = V.t
       and type index := index
       and type indexing_strategy := indexing_strategy
end

module type Sigs = sig
  module Indexing_strategy : sig
    type t = Pack_value.Kind.t -> bool

    val always : t
    val minimal : t
  end

  module type S = S with type indexing_strategy := Indexing_strategy.t
  module type Maker = Maker with type indexing_strategy := Indexing_strategy.t

  module Maker
      (_ : Version.S)
      (Index : Pack_index.S)
      (Hash : Irmin.Hash.S with type t = Index.key) :
    Maker
      with type key = Hash.t Pack_key.t
       and type hash = Hash.t
       and type index := Index.t
end
