(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type (_, _) eq = Refl : ('a, 'a) eq

type _ equality = ..

module type Inst = sig
  type t

  type _ equality += Eq : t equality
end

type 'a t = (module Inst with type t = 'a)

let make : type a. unit -> a t =
 fun () ->
  let module Inst = struct
    type t = a

    type _ equality += Eq : t equality
  end in
  (module Inst)

let eq : type a b. a t -> b t -> (a, b) eq option =
 fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None

let cast : type a b. a t -> b t -> a -> b option =
 fun awit bwit a -> match eq awit bwit with Some Refl -> Some a | None -> None
