(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Ir_merge.Infix

let src = Logs.Src.create "irmin.sync" ~doc:"Irmin remote sync"
module Log = (val Logs.src_log src : Logs.LOG)

let remote_store m x = Ir_s.Store (m, x)
let remote_uri s = Ir_s.URI s

module Make (S: Ir_s.STORE) = struct

  module B = S.Private.Sync
  type db = S.t
  type commit = S.commit

  let conv dx dy x =
    let len = Depyt.size_of dx x in
    let buf = Depyt.B (Bytes.create len) in
    let (_: int) = Depyt.write dx buf ~pos:0 x in
    let _, y = Depyt.read dy buf ~pos:0 in
    y

  let convert_slice (type r) (type s)
      (module RP: Ir_s.PRIVATE with type Slice.t = r)
      (module SP: Ir_s.PRIVATE with type Slice.t = s)
      r
    =
    SP.Slice.empty () >>= fun s ->
    RP.Slice.iter r (function
        | `Contents (k, v) ->
          let k = conv RP.Contents.Key.t SP.Contents.Key.t k in
          let v = conv RP.Contents.Val.t SP.Contents.Val.t v in
          SP.Slice.add s (`Contents (k, v))
        | `Node (k, v) ->
          let k = conv RP.Node.Key.t SP.Node.Key.t k in
          let v = conv RP.Node.Val.t SP.Node.Val.t v in
          SP.Slice.add s (`Node (k, v))
        | `Commit (k, v) ->
          let k = conv RP.Commit.Key.t SP.Commit.Key.t k in
          let v = conv RP.Commit.Val.t SP.Commit.Val.t v in
          SP.Slice.add s (`Commit (k, v))
      ) >>= fun () ->
    Lwt.return s

  let fetch t ?depth remote =
    match remote with
    | Ir_s.URI uri ->
      Log.debug (fun f -> f "fetch URI %s" uri);
      begin match S.status t with
        | `Empty | `Commit _  -> Lwt.return `No_head
        | `Branch b ->
          B.v (S.repo t) >>= fun g ->
          B.fetch g ?depth ~uri b
      end
    | Ir_s.Store ((module R), r) ->
      Log.debug (fun f -> f "fetch store");
      let s_repo = S.repo t in
      S.Repo.heads s_repo >>= fun min ->
      let min = List.map (conv S.Commit.t R.Commit.t) min in
      R.Head.find r >>= function
      | None   -> Lwt.return `No_head
      | Some h ->
        R.Repo.export (R.repo r) ?depth ~min ~max:[h] >>= fun r_slice ->
        convert_slice (module R.Private) (module S.Private) r_slice
        >>= fun s_slice ->
        S.Repo.import s_repo s_slice >|= function
        | `Error -> `Error
        | `Ok    ->
          let h = conv R.Commit.t S.Commit.t h in
          `Head h

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | `Head h  -> Lwt.return h
    | `No_head -> Lwt.fail (Failure "Sync.fetch_exn: no head!")
    | `Error   -> Lwt.fail (Failure "Sync.fetch_exn: fetch error!")

  let pull t ?depth remote kind =
    fetch t ?depth remote >>= function
    | `Error   -> Ir_merge.ok `Error
    | `No_head -> Ir_merge.ok `No_head
    | `Head k  ->
      match kind with
      | `Merge task -> S.Head.merge ~into:t task k  >>| fun () -> Ir_merge.ok `Ok
      | `Update     -> S.Head.set t k >>= fun () -> Ir_merge.ok `Ok

  let pull_exn t ?depth remote kind =
    pull t ?depth remote kind >>= function
    | Ok `Ok      -> Lwt.return_unit
    | Ok `No_head -> Lwt.fail_with "Sync.pull_exn: no head!"
    | Ok `Error   -> Lwt.fail_with "Sync.pull_exn: pull error!"
    | Error (`Conflict c) -> Lwt.fail_with ("Sync.pull_exn: " ^ c)

  let push t ?depth remote =
    Log.debug (fun f -> f "push");
    match remote with
    | Ir_s.URI uri ->
      begin match S.status t with
        | `Empty | `Commit _ -> Lwt.return `Error
        | `Branch br ->
          B.v (S.repo t) >>= fun g ->
          B.push g ?depth ~uri br
      end
    | Ir_s.Store ((module R), r) ->
      S.Head.find t >>= function
      | None   -> Lwt.return `Error
      | Some h ->
        Log.debug (fun f -> f "push store");
        R.Repo.heads (R.repo r) >>= fun min ->
        let min = List.map (conv R.Commit.t S.Commit.t) min in
        S.Repo.export (S.repo t) ?depth ~min >>= fun s_slice ->
        convert_slice (module S.Private) (module R.Private) s_slice
        >>= fun r_slice -> R.Repo.import (R.repo r) r_slice >>= function
        | `Error -> Log.debug (fun f -> f "ERROR!"); Lwt.return `Error
        | `Ok    ->
          Log.debug (fun f -> f "OK!");
          let h = conv S.Commit.t R.Commit.t h in
          R.Head.set r h >>= fun () ->
          Lwt.return `Ok

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | `Ok    -> Lwt.return_unit
    | `Error -> Lwt.fail (Failure "Sync.push_exn")

end
