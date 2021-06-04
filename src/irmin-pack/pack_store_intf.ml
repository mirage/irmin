open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of
    {!Content_addressable.S} that uses an append-only file of variable-length
    data blocks. The data file is indexed by hash via {!Pack_index.S}
    implementation. *)
module type S = sig
  include Content_addressable.S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
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
  include S.Checkable with type 'a t := 'a t and type key := key
end

module type Maker = sig
  type key
  type index

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)

  module Make (V : Pack_value.S with type hash := key) :
    S with type key = key and type value = V.t and type index = index
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  module Maker
      (V : Version.S)
      (Index : Pack_index.S)
      (K : Irmin.Hash.S with type t = Index.key) :
    Maker with type key = K.t and type index = Index.t
end
