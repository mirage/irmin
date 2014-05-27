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
open Lwt
open IrminMerge.OP

module type S = sig
  include IrminBranch.STORE with type key = IrminPath.t
  module Snapshot: IrminSnapshot.STORE with type db = t
                                        and type state = Block.key
  module Dump: IrminDump.STORE with type db       = t
                                and type key      = Block.key
                                and type contents = Block.contents
  module View: IrminView.STORE with type db    = t
                                and type node  = Block.key
                                and type value = value
end

type ('key, 'contents, 'tag) t =
  (module S with type Block.key = 'key
             and type value     = 'contents
             and type branch    = 'tag)

let cast (type a) (type b) (type c) (t: (a, b, c) t) =
  let module M = (val t) in
  (module M: S)

module Make (Block: IrminBlock.STORE) (Tag: IrminTag.STORE with type value = Block.key) =
struct

  module S = IrminBranch.Make(Block)(Tag)
  module Snapshot = IrminSnapshot.Make(S)
  module Dump = IrminDump.Make(S)
  module View = IrminView.Store(S)
  include S

end

module RO_BINARY  (S: IrminStore.RO_BINARY) (K: IrminKey.S) (V: IrminIdent.S) = struct

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

module AO_BINARY (S: IrminStore.AO_BINARY)  (K: IrminKey.S) (V: IrminIdent.S) = struct

  include RO_BINARY(S)(K)(V)

  module LA = Log.Make(struct let section = "AO" end)

  let add t value =
    LA.debugf "add";
    S.add t (IrminMisc.write V.bin_t value) >>= fun key ->
    let key = K.of_raw key in
    LA.debugf "<-- added: %s" (K.to_string key);
    return key

end

module RW_BINARY (S: IrminStore.RW_BINARY) (K: IrminKey.S) (V: IrminIdent.S) = struct

  include RO_BINARY(S)(K)(V)

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

module Binary
    (AO: IrminStore.AO_BINARY)
    (RW: IrminStore.RW_BINARY)
    (K : IrminKey.S)
    (C : IrminContents.S)
    (T : IrminTag.S) =
struct
  module V = IrminBlock.S(K)(C)
  module B = IrminBlock.S(K)(C)
  module XBlock = IrminBlock.Make(K)(C)(AO_BINARY(AO)(K)(B))
  module XTag = IrminTag.Make(T)(K)(RW_BINARY(RW)(T)(K))
  include Make(XBlock)(XTag)
end

module type BACKEND = sig
  module RO (K: IrminKey.S) (V: IrminIdent.S): IrminStore.RO
  module AO (K: IrminKey.S) (V: IrminIdent.S): IrminStore.AO
  module RW (K: IrminKey.S) (V: IrminKey.S)  : IrminStore.RW
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S):
    S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t
end
