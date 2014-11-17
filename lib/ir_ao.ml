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

module type STORE = sig
  include Ir_ro.STORE
  val add: t -> origin -> value -> key Lwt.t
end

module type BINARY = STORE
  with type key = Cstruct.t
   and type value = Cstruct.t
   and type origin = Cstruct.t

module type JSON = STORE
  with type key = Ezjsonm.t
   and type value = Ezjsonm.t
   and type origin = Ezjsonm.t

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (V: Tc.I0) ->
  functor (O: Tc.I0) ->
    STORE with type key = K.t and type value = V.t and type origin = O.t

module Binary (S: BINARY) (K: Ir_uid.S) (V: Tc.I0) (O: Tc.I0) = struct

  include Ir_ro.Binary(S)(K)(V)(O)

  let add t origin value =
    Log.debugf "add";
    let value = Tc.write_cstruct (module V) value in
    let origin = Tc.write_cstruct (module O) origin in
    S.add t origin value >>= fun key ->
    let key = Tc.read_cstruct (module K) key in
    Log.debugf "added: %a" force (show (module K) key);
    return key

end

module Json (S: JSON)  (K: Ir_uid.S) (V: Tc.I0) (O: Tc.I0) = struct

  include Ir_ro.Json(S)(K)(V)(O)

  let add t origin value =
    Log.debugf "add";
    let origin = Tc.to_json (module O) origin in
    let value = Tc.to_json (module V) value in
    S.add t origin value >>= fun key ->
    let key = Tc.of_json (module K) key in
    Log.debugf "added: %a" force (show (module K) key);
    return key

end
