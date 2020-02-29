(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
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

open Brands
module Subtyping = Subtyping

type ('s, 't, 'a, 'b, 'm, 'feat) t = {
  f : 'w. ('feat, 'w) Dictionary.t -> ('a, 'b, 'w) app2 -> ('s, 't, 'w) app2;
}

type ('s, 't, 'a, 'b, 'm, 'feat) optic = ('s, 't, 'a, 'b, 'm, 'feat) t

let compose t1 t2 =
  { f = (fun dictionary mapping -> t1.f dictionary (t2.f dictionary mapping)) }

let ( >> ) = compose

(* let optional_dict = { Dictionary.Create.Optional.f = (fun ) *)

(* let get_option t = 
 * 
 * let map
 * 
 * let update *)
(* let get *)

include Optic_types.Aliases

module Optic_list = struct
  type ('list, 'm, 'k) t =
    | ( :: ) :
        ('s, 't, 'a, 'b, 'm, 'k) optic * ('list, 'm, 'k) t
        -> (('s, 't, 'a, 'b, 'm, 'k) optic * 'list, 'm, 'k) t
    | [] : (unit, 'm, 'k) t
end
