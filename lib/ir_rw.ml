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
module Log = Log.Make(struct let section = "RW" end)

module type STORE = sig
  include Ir_ro.STORE
  val update: t -> origin -> key -> value -> unit Lwt.t
  val remove: t -> origin -> key -> unit Lwt.t
  val watch: t -> origin -> key -> value Lwt_stream.t
end

module type MAKER =
  functor (K: Tc.I0) ->
  functor (V: Tc.I0) ->
  functor (O: Tc.I0) ->
  STORE with type key = K.t and type value = V.t and type origin = O.t

module type BINARY = STORE
  with type key = Cstruct.t
   and type value = Cstruct.t
   and type origin = Cstruct.t

module type JSON = STORE
  with type key = Ezjsonm.t
   and type value = Ezjsonm.t
   and type origin = Ezjsonm.t

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0) = struct

  include Ir_ro.Binary(S)(K)(V)(O)

  let k_to_raw = Tc.write_cstruct (module K)
  let o_to_raw = Tc.write_cstruct (module O)
  let v_to_raw = Tc.write_cstruct (module V)
  let v_of_raw = Tc.read_cstruct (module V)

  let update t origin key value =
    Log.debugf "update %a" force (show (module K) key);
    S.update t (o_to_raw origin) (k_to_raw key) (v_to_raw value)

  let remove t origin key =
    Log.debugf "remove %a" force (show (module K) key);
    S.remove t (o_to_raw origin) (k_to_raw key)

  let watch t origin key =
    Log.debugf "watch %a" force (show (module K) key);
    Lwt_stream.map v_of_raw (S.watch t (o_to_raw origin) (k_to_raw key))

end

module Json (S: JSON) (K: Tc.I0) (V: Tc.I0) (O: Tc.I0) = struct

  include Ir_ro.Json(S)(K)(V)(O)

  let k_to_json = Tc.to_json (module K)
  let o_to_json = Tc.to_json (module O)
  let v_to_json = Tc.to_json (module V)
  let v_of_json = Tc.of_json (module V)

  let update t origin key value =
    Log.debugf "update %a" force (show (module K) key);
    S.update t (o_to_json origin) (k_to_json key) (v_to_json value)

  let remove t origin key =
    Log.debugf "remove %a" force (show (module K) key);
    S.remove t (o_to_json origin) (k_to_json key)

  let watch t origin key =
    Log.debugf "watch %a" force (show (module K) key);
    Lwt_stream.map v_of_json (S.watch t (o_to_json origin) (k_to_json key))

end
