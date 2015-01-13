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
  module Path: Ir_path.S
  val merge: Path.t -> t option Ir_merge.t
end

module String: S with type t = string and module Path = Ir_path.String_list
module Json: S with type t = Ezjsonm.t and module Path = Ir_path.String_list
module Cstruct: S with type t = Cstruct.t and module Path = Ir_path.String_list

module type STORE = sig
  include Ir_ao.STORE
  module Path: Ir_path.S
  val merge: Path.t -> t -> key option Ir_merge.t
  module Key: Ir_hash.S with type t = key
  module Val: S with type t = value and module Path = Path
end

module Make
    (S: sig
       include Ir_ao.STORE
       module Key: Ir_hash.S with type t = key
       module Val: S with type t = value
     end):
  STORE with type t = S.t
         and type key = S.key
         and type value = S.value
         and module Path =  S.Val.Path
