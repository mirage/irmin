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

type headers = { offset : int63; generation : int63 }

module type S = sig
  type t
  type path := string

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> path -> t
  val name : t -> string
  val clear : ?keep_generation:unit -> t -> unit
  val append : t -> string -> unit
  val set : t -> off:int63 -> string -> unit
  val read : t -> off:int63 -> bytes -> int
  val read_buffer : t -> off:int63 -> buf:bytes -> len:int -> int
  val offset : t -> int63
  val generation : t -> int63
  val force_headers : t -> headers
  val readonly : t -> bool
  val version : t -> Version.t
  val flush : t -> unit
  val close : t -> unit
  val exists : string -> bool
  val size : t -> int

  val truncate : t -> unit
  (** Sets the length of the underlying IO to be 0, without actually purging the
      associated data. Not supported for stores beyond [`V1], which should use
      {!clear} instead. *)

  val migrate :
    progress:(int63 -> unit) ->
    t ->
    Version.t ->
    (unit, [> `Msg of string ]) result
  (** @raise Invalid_arg if the migration path is not supported. *)
end

module type Sigs = sig
  type nonrec headers = headers

  module type S = S

  module Unix : S

  module Cache : sig
    type ('a, 'v) t = {
      v : 'a -> ?fresh:bool -> ?readonly:bool -> string -> 'v;
    }

    val memoize :
      v:('a -> fresh:bool -> readonly:bool -> string -> 'v) ->
      clear:('v -> unit) ->
      valid:('v -> bool) ->
      (root:string -> string) ->
      ('a, 'v) t
  end
end
