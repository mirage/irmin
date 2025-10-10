(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Append_only : Irmin.Append_only.Maker
module Atomic_write : Irmin.Atomic_write.Maker
module Maker : Irmin.Maker
module KV : Irmin.KV_maker with type info = Irmin.Info.default

(** {1 Configuration} *)

module Conf = Conf

val config :
  sw:Eio.Switch.t ->
  fs:[> Eio.Fs.dir_ty ] Eio.Path.t ->
  ?fresh:bool ->
  ?sync:bool ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Irmin.config

(** {1 Configuration} *)

(* TODO: add stats for benchmarking ? *)
