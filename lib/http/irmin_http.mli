(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** JSON CRUD interface. *)

val config:
  ?config:Irmin.config -> ?content_type:[`Raw|`Json] -> Uri.t ->
  Irmin.config

val uri: Uri.t option Irmin.Private.Conf.key
val content_type: string option Irmin.Private.Conf.key

module AO (C: Cohttp_lwt.Client): Irmin.AO_MAKER
module RW (C: Cohttp_lwt.Client): Irmin.RW_MAKER

module Make (C: Cohttp_lwt.Client) (M: Irmin.Metadata.S): Irmin.S_MAKER
module Low (C: Cohttp_lwt.Client) (M: Irmin.Metadata.S): Irmin.S_MAKER
