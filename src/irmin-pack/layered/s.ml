(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open! Import
open Store_properties

module type Store = sig
  include Irmin_layers.S
  include Irmin_pack.Specifics with type repo := repo and type commit := commit

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

module type Maker = functor
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  (H : Irmin.Hash.S)
  ->
  Store
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

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

module type Atomic_write = sig
  open Irmin_pack.Atomic_write
  include S
  module U : Persistent
  module L : Persistent

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

module type Content_addressable = sig
  open Irmin_pack.Pack_store
  include S
  module U : S with type value = value and type index := index
  module L : S with type index := index

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
    offset:int63 ->
    length:int ->
    layer:Irmin_layers.Layer_id.t ->
    key ->
    _ t ->
    (unit, Irmin_pack.Checks.integrity_error) result

  val consume_newies : 'a t -> key list

  val check :
    'a t ->
    ?none:(unit -> unit Lwt.t) ->
    ?some:(value -> unit Lwt.t) ->
    key ->
    unit Lwt.t
end

module type Content_addressable_maker = sig
  type key
  type index

  module Make (V : Irmin_pack.Pack_value.S with type hash := key) :
    Content_addressable
      with type key = key
       and type value = V.t
       and type index = index
       and type U.key = key
       and type L.key = key
       and type U.value = V.t
       and type L.value = V.t
end
