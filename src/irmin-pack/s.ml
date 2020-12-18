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

type integrity_error = [ `Wrong_hash | `Absent_value ]

module type CLOSEABLE = sig
  type 'a t

  val close : _ t -> unit Lwt.t
end

module type CHECKABLE = sig
  type 'a t

  type key

  val integrity_check :
    offset:int64 -> length:int -> key -> _ t -> (unit, integrity_error) result
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

module type ATOMIC_WRITE_STORE = sig
  include Irmin.ATOMIC_WRITE_STORE

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t

  val flush : t -> unit

  val clear_keep_generation : t -> unit Lwt.t
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
