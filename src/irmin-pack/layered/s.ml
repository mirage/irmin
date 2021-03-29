open! Import
open Store_properties
include Irmin_pack.Private.Sigs

module type Store = sig
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

module type Layered_general = sig
  type 'a t

  include Closeable with type 'a t := 'a t

  val update_flip : flip:bool -> _ t -> unit
  val flip_upper : _ t -> unit
end

module type Layered = sig
  type t

  include Layered_general with type _ t := t
end

module type Layered_atomic_write_store = sig
  include Atomic_write_store
  module U : Atomic_write_store
  module L : Atomic_write_store

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

  include Layered with type t := t

  val flush_next_lower : t -> unit
  val clear_previous_upper : ?keep_generation:unit -> t -> unit Lwt.t
  val copy_newies_to_next_upper : t -> unit Lwt.t
end

module type Layered_pack = sig
  open Irmin_pack.Pack
  include S
  module U : S with type value = value
  module L : S

  val v :
    read U.t ->
    read U.t ->
    read L.t option ->
    flip:bool ->
    freeze_in_progress:(unit -> bool) ->
    read t

  val layer_id : read t -> key -> Irmin_layers.Layer_id.t Lwt.t

  type 'a layer_type =
    | Upper : read U.t layer_type
    | Lower : read L.t layer_type

  val copy : 'l layer_type * 'l -> read t -> string -> key -> unit

  val copy_from_lower :
    read t ->
    dst:'a U.t ->
    ?aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t
  val mem_next : [> read ] t -> key -> bool Lwt.t
  val current_upper : 'a t -> read U.t
  val next_upper : 'a t -> read U.t
  val lower : 'a t -> read L.t
  val clear_previous_upper : ?keep_generation:unit -> 'a t -> unit Lwt.t

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  include Layered_general with type 'a t := 'a t

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

module type Layered_pack_maker = sig
  open Irmin_pack.Pack

  type key
  type index

  module Make (V : ELT with type hash := key) :
    Layered_pack
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
