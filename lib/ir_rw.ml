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

module type S = sig
  include Ir_ro.S
  val update: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
  val watch: t -> key -> value Lwt_stream.t
end

module type BINARY = S with type key = Cstruct.t and type value = Cstruct.t

module type MAKER = functor (K: Tc.I0) -> functor (V: Tc.I0) ->
  S with type key = K.t and type value = V.t

module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0) = struct

  include Ir_ro.Binary(S)(K)(V)

  let to_raw = Tc.write_cstruct (module K)

  let update t key value =
    Log.debugf "update %a" force (show (module K) key);
    S.update t (to_raw key) (Tc.write_cstruct (module V) value)

  let remove t key =
    Log.debugf "remove %a" force (show (module K) key);
    S.remove t (to_raw key)

  let watch t key =
    Log.debugf "watch %a" force (show (module K) key);
    Lwt_stream.map (Tc.read_cstruct (module V)) (S.watch t (to_raw key))

end
