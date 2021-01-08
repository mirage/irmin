include Irmin_pack.Private.Sigs

module type STORE = sig
  include Irmin_layers.S
  include Irmin_pack.Store.S with type repo := repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    (( [> `Fixed of int | `No_error ],
       [> `Cannot_fix of string | `Corrupted of int ] )
     result
    * Irmin_layers.Layer_id.t)
    list
end

module type LAYERED_GENERAL = sig
  type 'a t

  include CLOSEABLE with type 'a t := 'a t

  val update_flip : flip:bool -> _ t -> unit
  val flip_upper : _ t -> unit
end

module type LAYERED = sig
  type t

  include LAYERED_GENERAL with type _ t := t
end

module type LAYERED_ATOMIC_WRITE_STORE = sig
  include ATOMIC_WRITE_STORE
  module U : ATOMIC_WRITE_STORE
  module L : ATOMIC_WRITE_STORE

  val v :
    U.t ->
    U.t ->
    L.t option ->
    flip:bool ->
    freeze_in_progress:(unit -> bool) ->
    t

  val copy :
    mem_commit_lower:(value -> bool Lwt.t) ->
    mem_commit_upper:(value -> bool Lwt.t) ->
    t ->
    unit Lwt.t

  include LAYERED with type t := t

  val flush_next_lower : t -> unit
  val clear_previous_upper : ?keep_generation:unit -> t -> unit Lwt.t
  val copy_newies_to_next_upper : t -> unit Lwt.t
end

module type LAYERED_PACK = sig
  open Irmin_pack.Pack
  include S
  module U : S with type value = value
  module L : S

  val v :
    [ `Read ] U.t ->
    [ `Read ] U.t ->
    [ `Read ] L.t option ->
    flip:bool ->
    freeze_in_progress:(unit -> bool) ->
    [ `Read ] t

  val layer_id : [ `Read ] t -> key -> Irmin_layers.Layer_id.t Lwt.t

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  val copy : 'l layer_type * 'l -> [ `Read ] t -> string -> key -> unit

  val copy_from_lower :
    [ `Read ] t ->
    dst:'a U.t ->
    ?aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t
  val mem_next : [> `Read ] t -> key -> bool Lwt.t
  val current_upper : 'a t -> [ `Read ] U.t
  val next_upper : 'a t -> [ `Read ] U.t
  val lower : 'a t -> [ `Read ] L.t
  val clear_previous_upper : ?keep_generation:unit -> 'a t -> unit Lwt.t

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  include LAYERED_GENERAL with type 'a t := 'a t

  val clear_caches_next_upper : 'a t -> unit

  val unsafe_append :
    ensure_unique:bool -> overcommit:bool -> 'a t -> key -> value -> unit

  val flush_next_lower : 'a t -> unit

  val integrity_check :
    offset:int64 ->
    length:int ->
    layer:Irmin_layers.Layer_id.t ->
    key ->
    _ t ->
    (unit, integrity_error) result

  val consume_newies : 'a t -> key list

  val check :
    'a t ->
    ?none:(unit -> unit Lwt.t) ->
    ?some:(value -> unit Lwt.t) ->
    key ->
    unit Lwt.t
end

module type LAYERED_PACK_MAKER = sig
  open Irmin_pack.Pack

  type key
  type index

  module Make (V : ELT with type hash := key) :
    LAYERED_PACK
      with type key = key
       and type value = V.t
       and type index = index
       and type U.index = index
       and type L.index = index
       and type U.key = key
       and type L.key = key
       and type U.value = V.t
       and type L.value = V.t
end
