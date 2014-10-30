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

open Ir_misc.OP
open Lwt

module Log = Log.Make(struct let section = "AO" end)

module type S = sig
  include Ir_ro.S
  val add: t -> value -> key Lwt.t
end

module type BINARY = S with type key = Cstruct.t and type value = Cstruct.t

module type MAKER = functor (K: Ir_uid.S) -> functor (V: Tc.I0) ->
  S with type key = K.t and type value = V.t

module Binary (S: BINARY)  (K: Ir_uid.S) (V: Tc.I0) = struct

  include Ir_ro.Binary(S)(K)(V)

  let of_raw = Tc.read_cstruct (module K)

  let add (t:t) (value:value) =
    Log.debugf "add";
    S.add t (Tc.write_cstruct (module V) value) >>= fun key ->
    let key = of_raw key in
    Log.debugf "added: %a" force (show (module K) key);
    return key

end
