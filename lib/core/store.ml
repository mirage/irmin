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
open Misc.OP

module BC (B: Block.STORE) (T: Tag.STORE with type value = B.key) =
  Branch.Make(B)(T)

module RO_BINARY  (S: Sig.RO_BINARY) (K: Sig.Uid) (V: Tc.I0) = struct

  module L = Log.Make(struct let section = "RO" end)

  type t = S.t

  type key = K.t

  type value = V.t

  let create () =
    S.create ()

  let read t key =
    S.read t (K.to_raw key) >>= function
    | None    -> return_none
    | Some ba -> return (Some (Tc.read_cstruct (module V) ba))

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None   -> fail (Uid.Unknown (K.pretty key))

  let mem t key =
    S.mem t (K.to_raw key)

  let list t keys =
    let keys = List.map K.to_raw keys in
    S.list t keys >>= fun ks ->
    let ks = List.map K.of_raw ks in
    return ks

  let dump t =
    S.dump t >>= fun l ->
    Lwt_list.fold_left_s (fun acc (s, ba) ->
        let v = Tc.read_cstruct (module V) ba in
        return ((K.of_raw s, v) :: acc)
      ) [] l

end

module AO_BINARY (S: Sig.AO_BINARY)  (K: Sig.Uid) (V: Tc.I0) = struct

  include RO_BINARY(S)(K)(V)

  module LA = Log.Make(struct let section = "AO" end)

  let add t value =
    LA.debugf "add";
    S.add t (Tc.write_cstruct (module V) value) >>= fun key ->
    let key = K.of_raw key in
    LA.debugf "<-- added: %a" force (show (module K) key);
    return key

end

module RW_BINARY (S: Sig.RW_BINARY) (K: Sig.Uid) (V: Tc.I0) = struct

  include RO_BINARY(S)(K)(V)

  module LM = Log.Make(struct let section = "RW" end)

  let update t key value =
    LM.debugf "update %a" force (show (module K) key);
    S.update t (K.to_raw key) (Tc.write_cstruct (module V) value)

  let remove t key =
    S.remove t (K.to_raw key)

  let watch t key =
    Lwt_stream.map (Tc.read_cstruct (module V)) (S.watch t (K.to_raw key))

end

module Binary
    (AO: Sig.AO_BINARY)
    (RW: Sig.RW_BINARY)
    (K : Sig.Uid)
    (C : Sig.Contents)
    (T : Sig.Tag) =
struct
  module V = Block.S(K)(C)
  module B = Block.S(K)(C)
  module XBlock = Block.Make(K)(C)(AO_BINARY(AO)(K)(B))
  module XTag = Tag.Make(T)(K)(RW_BINARY(RW)(T)(K))
  include BC(XBlock)(XTag)
end
