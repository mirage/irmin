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

type version = [ `V1 | `V2 ]

val pp_version : version Fmt.t

type path = string

module type S = sig
  type t

  exception RO_Not_Allowed

  val v : version:version -> fresh:bool -> readonly:bool -> path -> t

  val name : t -> string

  val clear : t -> unit

  val append : t -> string -> unit

  val set : t -> off:int64 -> string -> unit

  val read : t -> off:int64 -> bytes -> int

  val offset : t -> int64

  val force_offset : t -> int64

  val generation : t -> int64

  val force_generation : t -> int64

  val readonly : t -> bool

  val version : t -> version

  val force_version : t -> version

  val flush : t -> unit

  val close : t -> unit

  val upgrade :
    src:t ->
    dst:path * version ->
    progress:(written:int64 -> unit) ->
    (unit, [> `Msg of string ]) result
  (** @raise Invalid_arg if the migration path is not supported. *)
end

module Unix : S

module Cache : sig
  type ('a, 'v) t = {
    v : 'a -> ?fresh:bool -> ?readonly:bool -> string -> 'v;
    invalidate : readonly:bool -> string -> unit;
  }

  val memoize :
    v:('a -> fresh:bool -> readonly:bool -> string -> 'v) ->
    clear:('v -> unit) ->
    valid:('v -> bool) ->
    string ->
    ('a, 'v) t
end
