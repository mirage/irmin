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
  module Io : Io_intf.S

  type t

  val find : t -> int -> string option
  val index : t -> string -> int option

  val create_rw :
    sw:Eio.Switch.t ->
    overwrite:bool ->
    path:string ->
    (t, [> Io.create_error ]) result

  val open_rw :
    sw:Eio.Switch.t ->
    size:int63 ->
    dead_header_size:int ->
    string ->
    (t, [> Io.open_error | Io.read_error | `Inconsistent_store ]) result

  val open_ro :
    sw:Eio.Switch.t ->
    size:int63 ->
    dead_header_size:int ->
    string ->
    (t, [> Io.open_error | Io.read_error | `Inconsistent_store ]) result

  val refresh_end_poff :
    t -> int63 -> (unit, [> Io.read_error | `Rw_not_allowed ]) result

  val empty_buffer : t -> bool
  val end_poff : t -> int63
  val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result
  val fsync : t -> (unit, [> Io.write_error ]) result
  val flush : t -> (unit, [> Io.write_error ]) result
end

module type Sigs = sig
  module type S = S

  module Make (Io : Io_intf.S) : S with module Io = Io
end
