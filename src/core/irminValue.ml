(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let debug fmt =
  IrminLog.debug "VALUE" fmt

exception Conflict

module type S = sig
  include IrminBase.S
  val of_bytes: string -> t
  val merge: old:t -> t -> t -> t
end

module Simple  = struct

  let debug fmt = IrminLog.debug "VALUE" fmt

  include IrminBase.String

  let name = "value"

  let create s = s

  let of_bytes s = s

  let merge ~old t1 t2 =
    if compare t1 t2 = 0 then t1
    else if compare old t1 = 0 then t2
    else if compare old t2 = 0 then t1
    else raise Conflict

  (* |-----|---------| *)
  (* | 'S' | PAYLOAD | *)
  (* |-----|---------| *)

  let sizeof t =
    1 + sizeof t

  let header = "V"

  let set buf t =
    IrminBuffer.set_string buf header;
    set buf t

  let get buf =
    let h = IrminBuffer.get_string buf 1 in
    if header = h then get buf
    else None

end

module type STORE = sig
  include IrminStore.A
  include S with type t := value
end

module type MAKER = functor (K: IrminKey.BINARY) -> functor (V: S) ->
  STORE with type key = K.t
         and type value = V.t
(** Value store creator. *)

module Make (S: IrminStore.A_MAKER) (K: IrminKey.BINARY) (V: S) =
struct

  include S(K)(V)

  include (V: S with type t := value)

end
