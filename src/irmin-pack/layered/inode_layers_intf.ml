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

module type S = sig
  include Irmin_pack.Inode.Persistent
  module U : Irmin_pack.Pack_store.S with type index := index
  module L : Irmin_pack.Pack_store.S with type index := index

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

  val copy : 'l layer_type * 'l -> read t -> key -> unit
  val mem_lower : 'a t -> key -> bool Lwt.t
  val mem_next : [> read ] t -> key -> bool Lwt.t
  val next_upper : 'a t -> read U.t
  val current_upper : 'a t -> read U.t
  val lower : 'a t -> read L.t

  include S.Layered_general with type 'a t := 'a t

  val clear_caches_next_upper : 'a t -> unit

  val sync :
    ?on_generation_change:(unit -> unit) ->
    ?on_generation_change_next_upper:(unit -> unit) ->
    'a t ->
    bool

  val integrity_check :
    offset:int63 ->
    length:int ->
    layer:Irmin_layers.Layer_id.t ->
    key ->
    'a t ->
    (unit, Irmin_pack.Checks.integrity_error) result

  val flush : ?index:bool -> 'a t -> unit
  val copy_from_lower : dst:'a U.t -> read t -> key -> unit Lwt.t
  val consume_newies : 'a t -> key list

  val check :
    'a t ->
    ?none:(unit -> unit Lwt.t) ->
    ?some:(U.value -> unit Lwt.t) ->
    key ->
    unit Lwt.t
end

module Index = Irmin_pack.Index

module type Sigs = sig
  module type S = S

  module Make
      (_ : Irmin_pack.Conf.S)
      (H : Irmin.Hash.S)
      (_ : S.Content_addressable_maker
             with type key = H.t
              and type index = Index.Make(H).t)
      (Node : Irmin.Private.Node.S with type hash = H.t) :
    S
      with type key = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type index = Index.Make(H).t
end
