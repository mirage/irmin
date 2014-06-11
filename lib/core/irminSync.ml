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

module Log = Log.Make(struct let section = "SYNC" end)

type 'a store = (module IrminBranch.STORE with type branch = 'a)

type remote =
  | Store: 'a store * 'a -> remote
  | URI of string

let store m b = Store (m, b)

let uri s = URI s

module type STORE = sig
  type db
  type t
  type origin
  val fetch: db -> ?depth:int -> remote -> t option Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> t Lwt.t
  val push: db -> ?depth:int -> remote -> t option Lwt.t
  val push_exn: db -> ?depth:int -> remote -> t Lwt.t
  val update: db -> t -> unit Lwt.t
  val merge: db -> ?origin:origin -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: db -> ?origin:origin -> t -> unit Lwt.t
  include IrminIdent.S with type t := t
end

module type BACKEND = sig
  type t
  type key
  val fetch: t -> ?depth:int -> string -> key option Lwt.t
  val push : t -> ?depth:int -> string -> key option Lwt.t
end

module Fast
    (S: IrminBranch.STORE)
    (R: BACKEND with type t = S.t and type key = S.Block.key) =
struct

  module K = S.Block.Key
  module C = S.Value
  module T = S.Tag.Key

  module Tag = S.Tag
  module Block = S.Block
  module Commit = Block.Commit
  module Node = Block.Node
  module Contents = Block.Contents

  type origin = IrminOrigin.t

  type db = S.t

  include K

  let sync ?depth (type k)
      (type l) (module L: IrminBranch.STORE with type t = l and type Block.key = k) (l:l)
      (type r) (module R: IrminBranch.STORE with type t = r) (r:r)
    =
    let remote_key k = R.Block.Key.of_raw (L.Block.Key.to_raw k) in
    let local_key k = L.Block.Key.of_raw (R.Block.Key.to_raw k) in
    R.head r >>= function
    | None             -> return_none
    | Some remote_head ->
      begin
        L.head l >>= function
        | None     -> return_nil
        | Some key -> L.Block.Commit.list (L.commit_t l) ?depth [key]
      end
      >>= fun local_keys ->
      let local_keys = List.map ~f:remote_key local_keys in
      R.Block.Commit.list (R.commit_t r) ?depth [remote_head]
      >>= fun remote_keys ->
      let keys = R.Block.Key.Set.(to_list (diff (of_list remote_keys) (of_list local_keys))) in
      Log.debugf "sync keys=%s" (IrminMisc.pretty_list R.Block.Key.to_string keys);
      Lwt_list.iter_p (fun key ->
          R.Block.read (R.block_t r) key >>= function
          | None   -> return_unit
          | Some v ->
            R.Block.Value.to_string v
            |> L.Block.Value.of_string
            |> L.Block.add (L.block_t l)
            >>= fun _ -> return_unit
        ) keys
      >>= fun () ->
      return (Some (local_key remote_head))

  let fetch t ?depth remote =
    match remote with
    | URI uri                          ->
      Log.debugf "fetch URI %s" uri;
      R.fetch t ?depth uri
    | Store ((module R), branch) ->
      Log.debugf "fetch store";
      R.create ~branch () >>= fun r ->
      sync ?depth (module S) t (module R) r

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | None   -> fail (Failure "fetch")
    | Some d -> return d

  let push t ?depth remote =
    Log.debugf "push";
    match remote with
    | URI uri                          -> R.push t ?depth uri
    | Store ((module R), branch) ->
      R.create ~branch () >>= fun r ->
      sync ?depth (module R) r (module S) t >>= function
      | None   -> return_none
      | Some k ->
        R.update_commit r k >>= fun _ ->
        return (Some (S.Block.Key.of_raw (R.Block.Key.to_raw k)))

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | None   -> fail (Failure "push")
    | Some d -> return d

  let update =
    S.update_commit

  let merge t ?origin dump =
    let origin = match origin with
      | None   -> IrminOrigin.create "Merge pulled state."
      | Some o -> o in
    S.merge_commit t ~origin dump

  let merge_exn t ?origin dump =
    merge t ?origin dump >>=
    IrminMerge.exn

end

module Slow (S: IrminBranch.STORE) = struct
  module B = struct

    type t = S.t

    type key = S.Block.key

    let fetch t ?depth uri =
      Log.debugf "slow fetch";
      return_none

    let push t ?depth uri =
      Log.debugf "slow push";
      return_none

  end
  include Fast(S)(B)
end
