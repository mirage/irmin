(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
module Sigs = S

module type ELT = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val magic : t -> char

  val encode_bin :
    dict:(string -> int option) ->
    offset:(hash -> int64 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> hash) -> string -> int -> t
end

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  val add : 'a t -> value -> key Lwt.t
  (** Overwrite [add] to work with a read-only database handler. *)

  val unsafe_add : 'a t -> key -> value -> unit Lwt.t
  (** Overwrite [unsafe_add] to work with a read-only database handler. *)

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val unsafe_append :
    ensure_unique:bool -> overcommit:bool -> 'a t -> key -> value -> unit

  val unsafe_mem : 'a t -> key -> bool

  val unsafe_find : check_integrity:bool -> 'a t -> key -> value option

  val flush : ?index:bool -> ?index_merge:bool -> 'a t -> unit

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  (** syncs a readonly instance with the files on disk. The same file instance
      is shared between several pack instances. Therefore only the first pack
      instance that checks a generation change, can see it.
      [on_generation_change] is a callback for all pack instances to react to a
      generation change. *)

  val version : 'a t -> IO.version

  val generation : 'a t -> int64

  val offset : 'a t -> int64

  val clear : 'a t -> unit Lwt.t
  (** [clear t] removes all the data from [t]. *)

  val clear_caches : 'a t -> unit
  (** [clear_cache t] clears all the in-memory caches of [t]. Persistent data
      are not removed. *)

  include Sigs.CHECKABLE with type 'a t := 'a t and type key := key

  include Sigs.CLOSEABLE with type 'a t := 'a t

  val clear_keep_generation : 'a t -> unit Lwt.t
end

module type MAKER = sig
  type key

  type index

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)
  module Make (V : ELT with type hash := key) :
    S with type key = key and type value = V.t and type index = index
end

module type LAYERED = sig
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

  include Sigs.LAYERED_GENERAL with type 'a t := 'a t

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
    (unit, Sigs.integrity_error) result

  val consume_newies : 'a t -> key list

  val check :
    'a t ->
    ?none:(unit -> unit Lwt.t) ->
    ?some:(value -> unit Lwt.t) ->
    key ->
    unit Lwt.t
end

module type LAYERED_MAKER = sig
  type key

  type index

  module Make (V : ELT with type hash := key) :
    LAYERED
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

module type Pack = sig
  module type ELT = ELT

  module type S = S

  module type MAKER = MAKER

  module type LAYERED = LAYERED

  module type LAYERED_MAKER = LAYERED_MAKER

  module File (Index : Pack_index.S) (K : Irmin.Hash.S with type t = Index.key) :
    MAKER with type key = K.t and type index = Index.t
end
