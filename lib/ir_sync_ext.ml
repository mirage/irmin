(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.sync" ~doc:"Irmin remote sync"
module Log = (val Logs.src_log src : Logs.LOG)

type remote =
  | Store: (module Ir_s.STORE_EXT with type t = 'a) * 'a -> remote
  | URI of string

let remote_store m x = Store (m, x)
let remote_uri s = URI s

module type STORE = sig
  type db
  type commit_id
  val fetch: db -> ?depth:int -> remote ->
    [`Head of commit_id | `No_head | `Error] Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> commit_id Lwt.t
  val pull: db -> ?depth:int -> remote -> [`Merge|`Update] ->
    [`Ok | `No_head | `Error] Ir_merge.result Lwt.t
  val pull_exn: db -> ?depth:int -> remote -> [`Merge|`Update] -> unit Lwt.t
  val push: db -> ?depth:int -> remote -> [`Ok | `Error] Lwt.t
  val push_exn: db -> ?depth:int -> remote -> unit Lwt.t
end

module Make (S: Ir_s.STORE_EXT) = struct

  module B = S.Private.Sync
  type db = S.t
  type commit_id = S.commit_id

  let conv (type x) (type y)
      (module X: Tc.S0 with type t = x) (module Y: Tc.S0 with type t = y)
      (x:x): y =
    Y.of_json (X.to_json x)

  let convert_slice (type r) (type s)
      (module RP: Ir_s.PRIVATE with type Slice.t = r)
      (module SP: Ir_s.PRIVATE with type Slice.t = s)
      r
    =
    SP.Slice.create () >>= fun s ->
    RP.Slice.iter_contents r (fun (k, v) ->
        let k = conv (module RP.Contents.Key) (module SP.Contents.Key) k in
        let v = conv (module RP.Contents.Val) (module SP.Contents.Val) v in
        SP.Slice.add_contents s (k, v)
      ) >>= fun () ->
    RP.Slice.iter_nodes r (fun (k, v) ->
        let k = conv (module RP.Node.Key) (module SP.Node.Key) k in
        let v = conv (module RP.Node.Val) (module SP.Node.Val) v in
        SP.Slice.add_node s (k, v)
      ) >>= fun () ->
    RP.Slice.iter_commits r (fun (k, v) ->
        let k = conv (module RP.Commit.Key) (module SP.Commit.Key) k in
        let v = conv (module RP.Commit.Val) (module SP.Commit.Val) v in
        SP.Slice.add_commit s (k, v)
      ) >>= fun () ->
    Lwt.return s

  let fetch t ?depth remote =
    match remote with
    | URI uri ->
      Log.debug (fun f -> f "fetch URI %s" uri);
      begin S.name t >>= function
        | None     -> Lwt.return `No_head
        | Some branch_id ->
          B.create (S.repo t) >>= fun g ->
          B.fetch g ?depth ~uri branch_id
      end
    | Store ((module R), r) ->
      Log.debug (fun f -> f "fetch store");
      let s_repo = S.repo t in
      S.Repo.heads s_repo >>= fun min ->
      let min = List.map (conv (module S.Hash) (module R.Hash) ) min in
      R.head r >>= function
      | None   -> Lwt.return `No_head
      | Some h ->
         R.Repo.export (R.repo r) ?depth ~min ~max:[h] >>= fun r_slice ->
         convert_slice (module R.Private) (module S.Private) r_slice >>= fun s_slice ->
         S.Repo.import s_repo s_slice >>= function
         | `Error -> Lwt.return `Error
         | `Ok    ->
           let h = conv (module R.Hash) (module S.Hash) h in
           return (`Head h)

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | `Head h  -> Lwt.return h
    | `No_head -> Lwt.fail (Failure "Sync.fetch_exn: no head!")
    | `Error   -> Lwt.fail (Failure "Sync.fetch_exn: fetch error!")

  let pull t ?depth remote kind =
    let open Ir_merge.OP in
    fetch t ?depth remote >>= function
    | `Error   -> ok `Error
    | `No_head -> ok `No_head
    | `Head k  ->
      match kind with
      | `Merge  -> S.merge_head t k  >>| fun () -> ok `Ok
      | `Update -> S.update_head t k >>= fun () -> ok `Ok

  let pull_exn t ?depth remote kind =
    pull t ?depth remote kind >>= Ir_merge.exn >>= function
    | `Ok      -> Lwt.return_unit
    | `No_head -> Lwt.fail (Failure "Sync.pull_exn: no head!")
    | `Error   -> Lwt.fail (Failure "Sync.pull_exn: pull error!")

  let push t ?depth remote =
    Log.debug (fun f -> f "push");
    match remote with
    | URI uri ->
      begin S.name t >>= function
        | None     -> return `Error
        | Some branch_id ->
          B.create (S.repo t) >>= fun g ->
          B.push g ?depth ~uri branch_id
      end
    | Store ((module R), r) ->
      S.head t >>= function
      | None   -> return `Error
      | Some h ->
        Log.debug (fun f -> f "push store");
        R.Repo.heads (R.repo r) >>= fun min ->
        let min = List.map (conv (module R.Hash) (module S.Hash)) min in
        S.Repo.export (S.repo t) ?depth ~min >>= fun s_slice ->
        convert_slice (module S.Private) (module R.Private) s_slice
        >>= fun r_slice -> R.Repo.import (R.repo r) r_slice >>= function
        | `Error -> Log.debug (fun f -> f "ERROR!"); Lwt.return `Error
        | `Ok    ->
          Log.debug (fun f -> f "OK!");
          let h = conv (module S.Hash) (module R.Hash) h in
          R.update_head r h >>= fun () ->
          Lwt.return `Ok

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | `Ok    -> return_unit
    | `Error -> fail (Failure "Sync.push_exn")

end
