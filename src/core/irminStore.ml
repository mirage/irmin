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

module type X_BINARY = X with type key := string
                          and type value := IrminBuffer.ba

module type A = sig
  include X
  val add: t -> value -> key Lwt.t
end

module type A_BINARY = A with type key := string
                          and type value := IrminBuffer.ba

module A (K: IrminKey.S) (V: IrminBase.S) (S: A_BINARY) = struct

  open Lwt

  exception Unknown of K.t

  let debug fmt =
    IrminLog.debug "A" fmt

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let init t =
    S.init t

  let read t key =
    S.read t (K.to_string key) >>= function
    | None    -> return_none
    | Some ba ->
      let buf = IrminBuffer.of_ba ba in
      return (Some (V.get buf))

  let read_exn t key =
    S.read_exn t (K.to_string key) >>= fun ba ->
    let buf = IrminBuffer.of_ba ba in
    return (V.get buf)

  let add t value =
    debug "add %s" (V.pretty value);
    let buf = IrminBuffer.create (V.sizeof value) in
    V.set buf value;
    S.add t (IrminBuffer.to_ba buf) >>= fun key ->
    let key = K.of_string key in
    debug "<-- add: %s -> key=%s" (V.pretty value) (K.pretty key);
    return key

  let mem t key =
    S.mem t (K.to_string key)

  let list t key =
    S.list t (K.to_string key) >>= fun ks ->
    return (List.map K.of_string ks)

end

module type M = sig
  include X
  val update: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
end

module type M_BINARY = M with type key := string
                          and type value := IrminBuffer.ba

module M
    (K: IrminKey.S)
    (V: IrminBase.S)
    (S: M_BINARY) =
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
    let buf = IrminBuffer.create (V.sizeof value) in
    V.set buf value;
    let ba = IrminBuffer.to_ba buf in
    S.update t (K.to_string key) ba

  let remove t key =
    S.remove t (K.to_string key)

  let read t key =
    S.read t (K.to_string key) >>= function
    | None    -> return_none
    | Some ba ->
      let buf = IrminBuffer.of_ba ba in
      return (Some (V.get buf))

  let read_exn t key =
    S.read_exn t (K.to_string key) >>= fun ba ->
    let buf = IrminBuffer.of_ba ba in
    return (V.get buf)

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
