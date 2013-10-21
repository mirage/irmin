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

module STORE: sig
  type t
  type key
  type value
  val write: t -> key -> value -> unit Lwt.t
  val read: t -> key -> value option Lwt.t
  val list: t -> key list Lwt.t
end

module type RAW = S with type value := IrminBuffer.t

module Make (R: RAW) (B: IrminBase.S) = struct

  open Lwt

  let read t k =
    Store.read t k >>= function
    | None   -> None
    | Some b -> Some (Value.get b)

  let write t k v =
    let ba = IrminBuffer.create_ba (Value.sizeof v) in
    Value.set ba v >>= fun () ->
    Store.write t k ba >>= fun () ->
    Value.get ba

end
