(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
  type t
  type path := string

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> path -> t
  val name : t -> string
  val append : t -> string -> unit
  val set : t -> off:int63 -> string -> unit
  val read : t -> off:int63 -> bytes -> int
  val read_buffer : t -> off:int63 -> buf:bytes -> len:int -> int
  val offset : t -> int63
  val force_offset : t -> int63
  val readonly : t -> bool
  val flush : t -> unit
  val close : t -> unit
  val exists : string -> bool
  val size : t -> int
  val mkdir : string -> unit

  (* {2 Versioning} *)

  val version : t -> Version.t
  val set_version : t -> Version.t -> unit
end

module type Sigs = sig
  module type S = S

  module Unix : S
end
