(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

val config :
  ?conf:Irmin.config ->
  ?lower_root:string ->
  ?upper_root1:string ->
  ?upper_root0:string ->
  ?with_lower:bool ->
  ?blocking_copy_size:int ->
  unit ->
  Irmin.config
(** Configuration options for layered stores.

    @param conf is an irmin-pack configuration.
    @param lower_root is the root of the lower store, "lower" is the default.
    @param upper_root1 is the root of one of the upper stores, "upper1" is the
    default.
    @param upper_root0 is the root of one of the upper stores, "upper0" is the
    default.
    @param with_lower if true (the default) use a lower layer during freezes.
    @param blocking_copy_size specifies the maximum size (in bytes) that can be
    copied in the blocking portion of the freeze. *)

module type S = sig
  include S.Store
  (** @inline *)
end

module type Maker = S.Maker

module Maker (_ : Irmin_pack.Conf.S) : Maker

module Maker_ext
    (_ : Irmin_pack.Conf.S)
    (_ : Irmin.Private.Node.Maker)
    (_ : Irmin.Private.Commit.Maker) : Maker

module Checks = Checks
