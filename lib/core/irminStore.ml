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

open Core_kernel.Std

module type RO = sig
  type t
  type key
  type value
  val create: unit -> t Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
  val list: t -> key list -> key list Lwt.t
  val dump: t -> (key * value) list Lwt.t
end

module type RO_BINARY = RO with type key = string
                            and type value = Cstruct.buffer

module type RO_MAKER = functor (K: IrminKey.S) -> functor (V: Identifiable.S) ->
  RO with type key = K.t
      and type value = V.t

module RO_MAKER (S: RO_BINARY) (K: IrminKey.S) (V: Identifiable.S) = struct

  open Lwt

  module L = Log.Make(struct let section = "RO" end)

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let read t key =
    S.read t (K.to_raw key) >>= function
    | None    -> return_none
    | Some ba -> return (IrminMisc.read V.bin_t ba)

  let read_exn t key =
    read t key >>= function
    | None   -> fail (IrminKey.Unknown (K.to_string key))
    | Some v -> return v

  let mem t key =
    S.mem t (K.to_raw key)

  let list t keys =
    let keys = List.map ~f:K.to_raw keys in
    S.list t keys >>= fun ks ->
    let ks = List.map ~f:K.of_raw ks in
    return ks

  let dump t =
    S.dump t >>= fun l ->
    Lwt_list.fold_left_s (fun acc (s, ba) ->
        match IrminMisc.read V.bin_t ba with
        | None   -> return acc
        | Some v -> return ((K.of_raw s, v) :: acc)
      ) [] l

end

module type AO = sig
  include RO
  val add: t -> value -> key Lwt.t
end

module type AO_BINARY = AO with type key = string
                            and type value = Cstruct.buffer

module type AO_MAKER = functor (K: IrminKey.S) -> functor (V: Identifiable.S) ->
  AO with type key = K.t
      and type value = V.t

module AO_MAKER (S: AO_BINARY) (K: IrminKey.S) (V: Identifiable.S) = struct

  open Lwt

  include RO_MAKER(S)(K)(V)

  module LA = Log.Make(struct let section = "AO" end)

  let add t value =
    LA.debugf "add %s" (V.to_string value);
    S.add t (IrminMisc.write V.bin_t value) >>= fun key ->
    let key = K.of_raw key in
    LA.debugf "<-- add: %s -> key=%s" (V.to_string value) (K.to_string key);
    return key

end

module type RW = sig
  include RO
  val update: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
  val watch: t -> key -> value Lwt_stream.t
end

module type RW_BINARY = RW with type key = string
                            and type value = Cstruct.buffer

module type RW_MAKER = functor (K: IrminKey.S) -> functor (V: Identifiable.S) ->
  RW with type key = K.t
      and type value = V.t

module RW_MAKER (S: RW_BINARY) (K: IrminKey.S) (V: Identifiable.S) = struct

  include RO_MAKER(S)(K)(V)

  module LM = Log.Make(struct let section = "RW" end)

  let update t key value =
    LM.debug (lazy "update");
    S.update t (K.to_string key) (IrminMisc.write V.bin_t value)

  let remove t key =
    S.remove t (K.to_string key)

  let watch t key =
    Lwt_stream.map (fun v ->
        match IrminMisc.read V.bin_t v with
        | None   -> failwith "watch"
        | Some v -> v
      ) (S.watch t (K.to_string key))

end

module type S = sig
  include RW
  type snapshot
  val snapshot: t -> snapshot Lwt.t
  val revert: t -> snapshot -> unit Lwt.t
  val merge_snapshot: t -> snapshot -> snapshot -> snapshot Lwt.t
  val watch: t -> key -> (key * snapshot) Lwt_stream.t
  type dump
  val export: t -> snapshot list -> dump Lwt.t
  type branch
  val import: t -> branch -> dump -> unit Lwt.t
end

module type S_MAKER =
  functor (K: IrminKey.S) ->
  functor (V: Identifiable.S) ->
  functor (S: Identifiable.S) ->
  functor (D: Identifiable.S) ->
    S with type key = K.t
       and type value = V.t
       and type snapshot = S.t
       and type dump = D.t
