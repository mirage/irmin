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

module Maker_ext
    (Config : Irmin_pack.Config.S)
    (N : Irmin.Private.Node.S)
    (CT : Irmin.Private.Commit.S with type hash = N.hash) : sig
  module Make
      (Metadata : Irmin.Metadata.S with type t = N.metadata)
      (Contents : Irmin.Contents.S)
      (Path : Irmin.Path.S with type step = N.step)
      (Branch : Irmin.Branch.S)
      (Hash : Irmin.Hash.S with type t = N.hash) :
    S
      with type key = Path.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type Key.step = Path.step
end

module Maker (Config : Irmin_pack.Config.S) : sig
  module Make
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) :
    S
      with type key = P.t
       and type step = P.step
       and type metadata = M.t
       and type contents = C.t
       and type branch = B.t
       and type hash = H.t
end

module Checks = Checks
