(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Values. *)

module type S = sig
  include Tc.S0
  val merge: t Ir_merge.t
end

module String: S with type t = string
module Json: S with type t = Ezjsonm.t
module Cstruct: S with type t = Cstruct.t

module type STORE = sig
  include Ir_ao.STORE
  module Key: Ir_hash.S with type t = key
  module Val: S with type t = value
end

module type STORE_EXT = sig
  include STORE
  val merge: t -> key Ir_merge.t
end

module Make_ext (Contents: STORE):
  STORE_EXT with type t = Contents.t
             and type key = Contents.key
             and type value = Contents.value
