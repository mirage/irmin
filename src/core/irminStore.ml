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

module type A = sig
  type t
  type key
  type value
  val write: t -> value -> key Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
end

module type ARAW = A with type value := IrminBuffer.t

module MakeI (S: ARAW) (K: IrminKey.S with type t = S.key) (V: IrminBase.S) = struct

  open Lwt

  type t = S.t

  type key = K.t

  type value = V.t

  let read t key =
    S.read t key >>= function
    | None   -> Lwt.return None
    | Some b -> Lwt.return (Some (V.get b))

  let read_exn t k =
    read t k >>= function
    | None   -> fail (K.Unknown k)
    | Some v -> return v

  let write t v =
    let buf = IrminBuffer.create (V.sizeof v) in
    V.set buf v;
    S.write t buf

  let mem t k =
    S.mem t k

end

module type M = sig
  type t
  type key
  type value
  val set: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
  val list: t -> key -> key list Lwt.t
end

module type MRAW = M with type key = string

module MakeM
    (S: MRAW)
    (K: IrminBase.STRINGABLE)
    (V: IrminBase.S with type t = S.value) =
struct

  open Lwt

  type t = S.t

  type key = K.t

  type value = V.t

  let set t key value =
    S.set t (K.to_string key) value

  let remove t key =
    S.remove t (K.to_string key)

  let read t key =
    S.read t (K.to_string key)

  let read_exn t key =
    S.read_exn t (K.to_string key)

  let mem t key =
    S.mem t (K.to_string key)

  let list t key =
    S.list t (K.to_string key) >>= fun ss ->
    let ks = List.map K.of_string ss in
    return ks

end

module type S = sig
  include M
  type revision
  val snapshot: t -> revision Lwt.t
  val revert: t -> revision -> unit Lwt.t
  val watch: t -> key -> (key * revision option) Lwt_stream.t
end
