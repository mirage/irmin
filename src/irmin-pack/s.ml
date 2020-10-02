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

module type ATOMIC_WRITE_STORE = sig
  include Irmin.ATOMIC_WRITE_STORE

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t

  val flush : t -> unit
end

module type LAYERED_CONTENT_ADDRESSABLE_STORE = sig
  include Pack.S

  module U : Pack.S with type value = value

  module L : Pack.S

  val v :
    [ `Read ] U.t ->
    [ `Read ] U.t ->
    [ `Read ] L.t option ->
    flip:bool ->
    freeze_lock:Lwt_mutex.t ->
    add_lock:Lwt_mutex.t ->
    [ `Read ] t

  val layer_id : [ `Read ] t -> key -> [ `Upper0 | `Upper1 | `Lower ] Lwt.t

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  val copy :
    'l layer_type * 'l ->
    [ `Read ] t ->
    ?aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t

  val check_and_copy :
    'l layer_type * 'l ->
    [ `Read ] t ->
    ?aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t

  val mem_next : [> `Read ] t -> key -> bool Lwt.t

  val flip_upper : 'a t -> unit

  val current_upper : 'a t -> [ `Read ] U.t

  val next_upper : 'a t -> [ `Read ] U.t

  val lower : 'a t -> [ `Read ] L.t

  val clear_previous_upper : 'a t -> unit Lwt.t

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  val update_flip : flip:bool -> 'a t -> unit

  val copy_newies_to_next_upper : 'a t -> unit Lwt.t

  val copy_last_newies_to_next_upper : 'a t -> unit Lwt.t

  val clear_caches_next_upper : 'a t -> unit

  val unsafe_append : 'a t -> key -> value -> unit Lwt.t

  val unsafe_mem : 'a t -> key -> bool Lwt.t

  val flush_next_lower : 'a t -> unit
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
    freeze_lock:Lwt_mutex.t ->
    add_lock:Lwt_mutex.t ->
    t

  val copy :
    mem_commit_lower:(value -> bool Lwt.t) ->
    mem_commit_upper:(value -> bool Lwt.t) ->
    t ->
    unit Lwt.t

  val flip_upper : t -> unit

  val clear_previous_upper : t -> unit Lwt.t

  val update_flip : flip:bool -> t -> unit

  val copy_newies_to_next_upper : t -> unit Lwt.t

  val copy_last_newies_to_next_upper : t -> unit Lwt.t

  val flush_next_lower : t -> unit
end

module type LAYERED_MAKER = sig
  type key

  type index

  module Make (V : Pack.ELT with type hash := key) :
    LAYERED_CONTENT_ADDRESSABLE_STORE
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

module type LAYERED_INODE = sig
  include Inode.S

  module U : Pack.S

  module L : Pack.S

  val v :
    [ `Read ] U.t ->
    [ `Read ] U.t ->
    [ `Read ] L.t option ->
    flip:bool ->
    freeze_lock:Lwt_mutex.t ->
    add_lock:Lwt_mutex.t ->
    [ `Read ] t

  val layer_id : [ `Read ] t -> key -> [ `Upper0 | `Upper1 | `Lower ] Lwt.t

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  val copy : 'l layer_type * 'l -> [ `Read ] t -> key -> unit Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t

  val mem_next : [> `Read ] t -> key -> bool Lwt.t

  val flip_upper : 'a t -> unit

  val next_upper : 'a t -> [ `Read ] U.t

  val lower : 'a t -> [ `Read ] L.t

  val clear_previous_upper : 'a t -> unit Lwt.t

  val copy_newies_to_next_upper : 'a t -> unit Lwt.t

  val copy_last_newies_to_next_upper : 'a t -> unit Lwt.t

  val update_flip : flip:bool -> 'a t -> unit

  val clear_caches_next_upper : 'a t -> unit

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool
end

module type STORE = sig
  type t

  val close : t -> unit Lwt.t

  val update_flip : flip:bool -> t -> unit

  val flip_upper : t -> unit

  val copy_newies_to_next_upper : t -> unit Lwt.t

  val copy_last_newies_to_next_upper : t -> unit Lwt.t
end
