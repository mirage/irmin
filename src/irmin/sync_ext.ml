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
open S

let invalid_argf fmt = Fmt.kstrf Lwt.fail_invalid_arg fmt

let src = Logs.Src.create "irmin.sync" ~doc:"Irmin remote sync"
module Log = (val Logs.src_log src : Logs.LOG)

let remote_store m x = S.Store (m, x)

module Make (S: S.STORE) = struct

  module B = S.Private.Sync
  type db = S.t
  type commit = S.commit

  let conv dx dy x =
    let str = Type.to_bin_string dx x in
    Type.of_bin_string dy str

  let convert_slice (type r) (type s)
      (module RP: PRIVATE with type Slice.t = r)
      (module SP: PRIVATE with type Slice.t = s)
      r
    =
    SP.Slice.empty () >>= fun s ->
    RP.Slice.iter r (function
        | `Contents (k, v) ->
          let k = conv RP.Contents.Key.t SP.Contents.Key.t k in
          let v = conv RP.Contents.Val.t SP.Contents.Val.t v in
          (match k, v with
           | Ok k, Ok v -> SP.Slice.add s (`Contents (k, v))
           | _ -> Lwt.return_unit)
        | `Node (k, v) ->
          let k = conv RP.Node.Key.t SP.Node.Key.t k in
          let v = conv RP.Node.Val.t SP.Node.Val.t v in
          (match k, v with
           | Ok k, Ok v -> SP.Slice.add s (`Node (k, v))
           | _ -> Lwt.return_unit)
        | `Commit (k, v) ->
          let k = conv RP.Commit.Key.t SP.Commit.Key.t k in
          let v = conv RP.Commit.Val.t SP.Commit.Val.t v in
          (match k, v with
           | Ok k, Ok v -> SP.Slice.add s (`Commit (k, v))
           | _ -> Lwt.return_unit)
      ) >>= fun () ->
    Lwt.return s

  let convs src dst l =
    List.fold_left (fun acc x ->
        match conv src dst x with
        | Ok x -> x::acc
        | _    -> acc
      ) [] l

  type fetch_error = [
    | `No_head
    | `Not_available
    | `Msg of string
  ]

  let pp_branch = Type.pp S.Branch.t
  let pp_hash = Type.pp S.Hash.t

  let fetch t ?depth remote: (commit, fetch_error) result Lwt.t =
    match remote with
    | Store ((module R), r) ->
      Log.debug (fun f -> f "fetch store");
      let s_repo = S.repo t in
      let r_repo = R.repo r in
      S.Repo.heads s_repo >>= fun min ->
      let min = convs S.(commit_t s_repo) R.(commit_t r_repo) min in
      begin
        R.Head.find r >>= function
        | None   -> Lwt.return (Error `No_head)
        | Some h ->
          R.Repo.export (R.repo r) ?depth ~min ~max:[h] >>= fun r_slice ->
          convert_slice (module R.Private) (module S.Private) r_slice
          >>= fun s_slice ->
          S.Repo.import s_repo s_slice >|= function
          | Error e -> Error (e :> fetch_error)
          | Ok ()   ->
            match conv R.(commit_t r_repo) S.(commit_t s_repo) h with
            | Ok h    -> Ok h
            | Error e -> Error (e :> fetch_error)
      end
    | S.E e ->
      begin match S.status t with
        | `Empty | `Commit _ -> Lwt.return (Error `No_head)
        | `Branch br ->
          Log.debug (fun l -> l "Fetching branch %a" pp_branch br);
          B.v (S.repo t) >>= fun g ->
          B.fetch g ?depth e br >>= function
          | Error _ as e -> Lwt.return e
          | Ok (Some c)         ->
            (Log.debug (fun l -> l "Fetched %a" pp_hash c);
            S.Commit.of_hash (S.repo t) c >|= function
            | None   -> Error `No_head
            | Some x -> Ok x)
          | Ok None ->
            S.Head.find t >>= function
            | Some h -> Lwt.return_ok h
            | None -> Lwt.return_error `No_head
      end
    | _ -> Lwt.return (Error `Not_available)

  let pp_fetch_error ppf = function
    | `No_head       -> Fmt.string ppf "empty head!"
    | `Not_available -> Fmt.string ppf "not available!"
    | `Msg m         -> Fmt.string ppf m

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | Ok h    -> Lwt.return h
    | Error e -> invalid_argf "Sync.fetch_exn: %a" pp_fetch_error e

  type pull_error = [ fetch_error | Merge.conflict ]

  let pp_pull_error ppf = function
    | #fetch_error as e -> pp_fetch_error ppf e
    | `Conflict c      -> Fmt.pf ppf "conflict: %s" c

  let pull t ?depth remote kind =
    fetch t ?depth remote >>= function
    | Error e  -> Lwt.return (Error (e :> pull_error))
    | Ok k     ->
      match kind with
      | `Set        -> S.Head.set t k >|= fun () -> Ok ()
      | `Merge info ->
        S.Head.merge ~into:t ~info k >|= fun x ->
        (x :> (unit, pull_error) result)

  let pull_exn t ?depth remote kind =
    pull t ?depth remote kind >>= function
    | Ok ()   -> Lwt.return_unit
    | Error e -> invalid_argf "Sync.pull_exn: %a" pp_pull_error e

  type push_error = [ fetch_error | `Detached_head ]

  let pp_push_error ppf = function
    | #fetch_error as e -> pp_fetch_error ppf e
    | `Detached_head    -> Fmt.string ppf "cannot push to a non-persistent store"

  let push t ?depth remote =
    Log.debug (fun f -> f "push");
    match remote with
    | Store ((module R), r) ->
      begin
        S.Head.find t >>= function
        | None   -> Lwt.return (Error (`No_head))
        | Some h ->
          Log.debug (fun f -> f "push store");
          R.Repo.heads (R.repo r) >>= fun min ->
          let r_repo = R.repo r in
          let s_repo = S.repo t in
          let min = convs R.(commit_t r_repo) S.(commit_t s_repo) min in
          S.Repo.export (S.repo t) ?depth ~min >>= fun s_slice ->
          convert_slice (module S.Private) (module R.Private) s_slice
          >>= fun r_slice -> R.Repo.import (R.repo r) r_slice >>= function
          | Error e -> Lwt.return (Error (e :> push_error))
          | Ok ()   ->
            match conv S.(commit_t s_repo) R.(commit_t r_repo) h with
            | Error e -> Lwt.return (Error (e :> push_error))
            | Ok h    -> R.Head.set r h >|= fun () -> Ok ()
      end
    | S.E e ->
      begin match S.status t with
        | `Empty    -> Lwt.return (Error `No_head)
        | `Commit _ -> Lwt.return (Error `Detached_head)
        | `Branch br ->
          B.v (S.repo t) >>= fun g ->
          (B.push g ?depth e br :> (unit, push_error) result Lwt.t)
      end
    | _ -> Lwt.return (Error `Not_available)

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | Ok ()   -> Lwt.return_unit
    | Error e -> invalid_argf "Sync.push_exn: %a" pp_push_error e

end
