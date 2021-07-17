(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

include Key_intf

module Of_hash (Hash : Hash.S) = struct
  type t = Hash.t [@@deriving irmin]
  type hash = Hash.t

  let to_hash x = x [@@inline]
  let of_hash x = x [@@inline]
end

(* module Make (H : Hash.S) = struct
 *   module Hash = H
 * 
 *   type hash = H.t
 *   type 'a t = { hash : H.t; mutable value : 'a option } [@@deriving irmin]
 * 
 *   let pp_hash = Type.pp H.t
 * 
 *   let pp pp_v ppf t =
 *     match t.value with
 *     | None -> Fmt.pf ppf "[%a]" pp_hash t.hash
 *     | Some id -> Fmt.pf ppf "[%a:%a]" pp_hash t.hash pp_v id
 * 
 *   let t : type a. a Type.t -> a t Type.t =
 *    fun v ->
 *     let pre_hash =
 *       let f = Type.(unstage (pre_hash H.t)) in
 *       Type.stage (fun t -> f t.hash)
 *     in
 *     Type.like (t v) ~pre_hash ~pp:(pp (Type.pp v))
 * 
 *   let hash t = t.hash
 *   let value t = t.value
 *   let v hash = { hash; value = None }
 *   let of_value v hash = { hash; value = Some v }
 *   let clear t = t.value <- None
 *   let set t m = t.value <- Some m
 * end *)
