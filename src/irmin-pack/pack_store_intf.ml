open! Import

module Pack_store_IO : Irmin_pack_layers.IO.S = struct
  include IO.Unix

  type t = { base:IO.Unix.t; mutable read_logger: out_channel option }
           
  (* now we need to lift all the funs to work with this new type; OO has an advantage here
     in that classes can be easily extended with additional fields, whereas here we have
     to lift the existing functions *)

  (* default value *)
  let read_logger = None

  let v ~version ~fresh ~readonly path = { base=v ~version ~fresh ~readonly path; read_logger}

  let truncate t = truncate t.base

  let readonly t = readonly t.base

  let flush t = flush t.base

  let close t = close t.base
      
  let offset t = offset t.base

  let read t ~off buf = 
    let len = read t.base ~off buf in
    let _maybe_log = 
      match t.read_logger with 
      | None -> ()
      | Some oc -> 
        Irmin_pack_layers.Util.Out_channel_extra.(
          output_int_ne oc (Int63.to_int off);
          output_int_ne oc len;
          ())
    in
    len

  let append t s = append t.base s

  let version t = version t.base

  let set_version t = set_version t.base

  let name t = name t.base

  let force_offset t = force_offset t.base

  let set_read_logger t opt = t.read_logger <- opt

end

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

(*
  (** [set_read_logger t (Some oc)] logs (off,len) data from the underlying [IO.t]
      instance to the out channel [oc]; this is needed by layered when computing
      reachability information; [set_read_logger t None] unsets the logger. NOTE probably
      this should only be used by the layered implementation. *)
  val set_read_logger: 'a t -> out_channel option -> unit
*)

  (** Layers operates at the IO level; we expose this here in order to provide access to
      the [Pack_store_IO.t] instance, and hence to GC functions etc. *)
  val get_io: 'a t -> Pack_store_IO.t

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

    val default : t
    (** [default] is the indexing strategy used by [irmin-pack] instances that
        do not explicitly set an indexing strategy in {!Irmin_pack.config}.
        Currently set to {!always}. *)
  end

  module type S = S with type indexing_strategy := Indexing_strategy.t
  module type Maker = Maker with type indexing_strategy := Indexing_strategy.t

  val selected_version : Version.t

  module Maker
      (Index : Pack_index.S)
      (Hash : Irmin.Hash.S with type t = Index.key) :
    Maker with type hash = Hash.t and type index := Index.t
end
