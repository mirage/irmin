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

open Lwt
open Merge.OP

module type S = sig
  include Branch.STORE with type key = Path.t
  module Dump: Dump.S with type t = t
  module Snapshot: Snapshot.STORE with type db = t
  module View: View.STORE with type db    = t
                           and type node  = Block.key
                           and type value = value
  module Sync: Sync.STORE with type db  = t
end

type ('key, 'contents, 'tag) t =
  (module S with type Block.key = 'key
             and type value     = 'contents
             and type branch    = 'tag)

let cast (type a) (type b) (type c) (t: (a, b, c) t) =
  let module M = (val t) in
  (module M: S)

module Make (Block: Block.STORE) (Tag: Tag.STORE with type value = Block.key) =
struct
  module S = Branch.Make(Block)(Tag)
  module Snapshot = Snapshot.Make(S)
  module Dump = Dump.Make(S)
  module View = View.Store(S)
  module Sync = Sync.Slow(S)
  include S
end

module RO_BINARY  (S: S.RO_BINARY) (K: Key.S) (V: I0) = struct

  module L = Log.Make(struct let section = "RO" end)

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let read t key =
    S.read t (K.to_raw key) >>= function
    | None    -> return_none
    | Some ba -> return (read_all (module V) ba)

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None   -> fail (Key.Unknown (K.pretty key))

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
        match read_all (module V) ba with
        | None   -> return acc
        | Some v -> return ((K.of_raw s, v) :: acc)
      ) [] l

end

module AO_BINARY (S: S.AO_BINARY)  (K: Key.S) (V: I0) = struct

  include RO_BINARY(S)(K)(V)

  module LA = Log.Make(struct let section = "AO" end)

  let add t value =
    LA.debugf "add";
    S.add t (write_all (module V) value) >>= fun key ->
    let key = K.of_raw key in
    LA.debugf "<-- added: %a" force (show (module K) key);
    return key

end

module RW_BINARY (S: S.RW_BINARY) (K: Key.S) (V: I0) = struct

  include RO_BINARY(S)(K)(V)

  module LM = Log.Make(struct let section = "RW" end)

  let update t key value =
    LM.debugf "update %a" force (show (module K) key);
    S.update t (K.to_raw key) (write_all (module V) value)

  let remove t key =
    S.remove t (K.to_raw key)

  let watch t key =
    Lwt_stream.map (fun v ->
        match read_all (module V) v with
        | None   -> failwith "watch"
        | Some v -> v
      ) (S.watch t (K.to_raw key))

end

module Binary
    (AO: S.AO_BINARY)
    (RW: S.RW_BINARY)
    (K : Key.S)
    (C : Contents.S)
    (T : Tag.S) =
struct
  module V = Block.S(K)(C)
  module B = Block.S(K)(C)
  module XBlock = Block.Make(K)(C)(AO_BINARY(AO)(K)(B))
  module XTag = Tag.Make(T)(K)(RW_BINARY(RW)(T)(K))
  include Make(XBlock)(XTag)
end


module type RO_MAKER =
  functor (K: Key.S)   ->
  functor (V: Misc.I0) ->
    S.RO with type key = K.t and type value = V.t

module type AO_MAKER =
  functor (K: Key.S)   ->
  functor (V: Misc.I0) ->
    S.AO with type key = K.t and type value = V.t

module type RW_MAKER =
  functor (K: Key.S) ->
  functor (V: Key.S) ->
    S.RW with type key = K.t and type value = V.t

module type S_MAKER =
  functor (K: Key.S)      ->
  functor (C: Contents.S) ->
  functor (T: Tag.S)      ->
    S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t

module type BACKEND = sig
  module RO: RO_MAKER
  module AO: AO_MAKER
  module RW: RW_MAKER
  module Make: S_MAKER
end

module Rec (AO: AO_MAKER) (S: S) = struct
  module K = S.Block.Key
  module C = Block.Rec(S.Block)
  module B = Block.S(K)(C)
  module AO = AO(K)(B)
  module XBlock = Block.Make(K)(C)(AO)
  include Make(XBlock)(S.Tag)
end
