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

module type X = sig
  type t
  type key
  type value
  val create: unit -> t Lwt.t
  val init: t -> unit Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
  val list: t -> key -> key list Lwt.t
end

module type A = sig
  include X
  val add: t -> value -> key Lwt.t
end

module type A_RAW = A with type value := IrminBuffer.ba

module MakeI (S: A_RAW) (K: IrminKey.S with type t = S.key) (V: IrminBase.S) = struct

  open Lwt

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let init t =
    S.init t

  let read t key =
    S.read t key >>= function
    | None    -> Lwt.return None
    | Some ba ->
      let buf = IrminBuffer.of_ba ba in
      Lwt.return (Some (V.get buf))

  let read_exn t k =
    read t k >>= function
    | None   -> fail (K.Unknown k)
    | Some v -> return v

  let add t v =
    let buf = IrminBuffer.create (V.sizeof v) in
    V.set buf v;
    S.add t (IrminBuffer.to_ba buf)

  let mem t k =
    S.mem t k

  let list t k =
    S.list t k

end

module type M = sig
  include X
  val update: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
end

module type M_RAW = M with type key = string

module MakeM
    (S: M_RAW)
    (K: IrminBase.STRINGABLE)
    (V: IrminBase.S with type t = S.value) =
struct

  open Lwt

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let init t =
    S.init t

  let update t key value =
    S.update t (K.to_string key) value

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
