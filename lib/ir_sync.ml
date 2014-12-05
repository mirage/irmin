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

open Lwt

module type S = sig
  type t
  type head
  type tag
  val create: Ir_conf.t -> t Lwt.t
  val fetch: t -> ?depth:int -> uri:string -> tag -> [`Local of head] option Lwt.t
  val push : t -> ?depth:int -> uri:string -> tag -> [`Ok | `Error] Lwt.t
end

module None (H: Tc.S0) (T: Tc.S0) = struct
  type t = unit
  type head = H.t
  type tag = T.t
  let create _ = return_unit
  let fetch () ?depth:_ ~uri:_ _tag = return_none
  let push () ?depth:_ ~uri:_ _tag = return `Error
end
