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
end

module type Maker = sig
  type hash
  type index
  type indexing_strategy

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
       and type indexing_strategy := indexing_strategy
end

module type Sigs = sig
  module Indexing_strategy : sig
    type t = value_length:int -> Pack_value.Kind.t -> bool
    (** The type of configurations for [irmin-pack]'s indexing strategy, which
        dictates whether or not newly-appended pack entries should also be added
        to the index. Strategies are parameterised over:

        - the length of the binary encoding of the {i object} inside the pack
          entry (i.e. not accounting for the encoded hash and kind character);
        - the kind of the pack object having been added.

        Indexing more than the {!minimal} strategy only impacts performance and
        not correctness: more indexing results in a larger index and a smaller
        pack file. *)

    val always : t
    (** The strategy that indexes all objects. *)

    val minimal : t
    (** The strategy that indexes as few objects as possible while still
        maintaing store integrity. *)
  end

  module type S = S with type indexing_strategy := Indexing_strategy.t
  module type Maker = Maker with type indexing_strategy := Indexing_strategy.t

  val selected_version : Version.t

  module Maker
      (Index : Pack_index.S)
      (Hash : Irmin.Hash.S with type t = Index.key) :
    Maker with type hash = Hash.t and type index := Index.t
end
