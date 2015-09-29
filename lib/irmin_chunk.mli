 (*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

(** Managing Chuncks.

     All functions for manipulate chuncks representation, a chunck is
     represented as above :

      --------------------------
     | uint8_t type             |
     ---------------------------
     | uint16_t chunck_length   |
     ---------------------------
     | byte data[data_length]   |
     ---------------------------

     Where type define if the chunck contain data or indirection, size
     represent the data length to consider, and data field is the
     payload.
*)


val config: ?conf:Irmin.config -> ?size:int -> unit -> Irmin.config
(** Configuration values. *)

module CHUNCK_AO (S: Irmin.AO_MAKER_RAW): Irmin.AO_MAKER_RAW
