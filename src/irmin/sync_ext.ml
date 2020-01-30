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

module Make (S : S.STORE) = struct
  module B = S.Private.Sync

  type db = S.t

  type commit = S.commit

  let conv dx dy x =
    let str = Type.to_bin_string dx x in
    Type.of_bin_string dy str

  let convert_slice (type r s) (module RP : PRIVATE with type Slice.t = r)
      (module SP : PRIVATE with type Slice.t = s) r =
    SP.Slice.empty () >>= fun s ->
    RP.Slice.iter r (function
      | `Contents (k, v) -> (
          let k = conv RP.Contents.Key.t SP.Contents.Key.t k in
          let v = conv RP.Contents.Val.t SP.Contents.Val.t v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Contents (k, v))
          | _ -> Lwt.return_unit )
      | `Node (k, v) -> (
          let k = conv RP.Node.Key.t SP.Node.Key.t k in
          let v = conv RP.Node.Val.t SP.Node.Val.t v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Node (k, v))
          | _ -> Lwt.return_unit )
      | `Commit (k, v) -> (
          let k = conv RP.Commit.Key.t SP.Commit.Key.t k in
          let v = conv RP.Commit.Val.t SP.Commit.Val.t v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Commit (k, v))
          | _ -> Lwt.return_unit ))
    >>= fun () -> Lwt.return s

  let convs src dst l =
    List.fold_left
      (fun acc x -> match conv src dst x with Ok x -> x :: acc | _ -> acc)
      [] l

  let pp_branch = Type.pp S.Branch.t

  let pp_hash = Type.pp S.Hash.t

  type status = [ `Empty | `Head of commit ]

  let pp_status ppf = function
    | `Empty -> Fmt.string ppf "empty"
    | `Head c -> Type.pp S.Hash.t ppf (S.Commit.hash c)

  let status_t t =
    let open Type in
    variant "status" (fun empty head ->
      function `Empty -> empty | `Head c -> head c)
    |~ case0 "empty" `Empty
    |~ case1 "head" S.(commit_t @@ repo t) (fun c -> `Head c)
    |> sealv

  let fetch t ?depth remote =
    match remote with
    | Store ((module R), r) -> (
        Log.debug (fun f -> f "fetch store");
        let s_repo = S.repo t in
        let r_repo = R.repo r in
        S.Repo.heads s_repo >>= fun min ->
        let min = convs S.(commit_t s_repo) R.(commit_t r_repo) min in
        R.Head.find r >>= function
        | None -> Lwt.return_ok `Empty
        | Some h -> (
            R.Repo.export (R.repo r) ?depth ~min ~max:(`Max [ h ])
            >>= fun r_slice ->
            convert_slice (module R.Private) (module S.Private) r_slice
            >>= fun s_slice ->
            S.Repo.import s_repo s_slice >|= function
            | Error e -> Error e
            | Ok () -> (
                match conv R.(commit_t r_repo) S.(commit_t s_repo) h with
                | Ok h -> Ok (`Head h)
                | Error e -> Error e ) ) )
    | S.E e -> (
        match S.status t with
        | `Empty | `Commit _ -> Lwt.return_ok `Empty
        | `Branch br -> (
            Log.debug (fun l -> l "Fetching branch %a" pp_branch br);
            B.v (S.repo t) >>= fun g ->
            B.fetch g ?depth e br >>= function
            | Error _ as e -> Lwt.return e
            | Ok (Some c) -> (
                Log.debug (fun l -> l "Fetched %a" pp_hash c);
                S.Commit.of_hash (S.repo t) c >|= function
                | None -> Ok `Empty
                | Some x -> Ok (`Head x) )
            | Ok None -> (
                S.Head.find t >>= function
                | Some h -> Lwt.return_ok (`Head h)
                | None -> Lwt.return_ok `Empty ) ) )
    | _ -> Lwt.return_error (`Msg "fetch operation is not available")

  let fetch_exn t ?depth remote =
    fetch t ?depth remote >>= function
    | Ok h -> Lwt.return h
    | Error (`Msg e) -> invalid_argf "Sync.fetch_exn: %s" e

  type pull_error = [ `Msg of string | Merge.conflict ]

  let pp_pull_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Conflict c -> Fmt.pf ppf "conflict: %s" c

  let pull t ?depth remote kind : (status, pull_error) result Lwt.t =
    fetch t ?depth remote >>= function
    | Error e -> Lwt.return_error (e :> pull_error)
    | Ok (`Head k) -> (
        match kind with
        | `Set -> S.Head.set t k >|= fun () -> Ok (`Head k)
        | `Merge info -> (
            S.Head.merge ~into:t ~info k >>= function
            | Ok () -> Lwt.return_ok (`Head k)
            | Error e -> Lwt.return_error (e :> pull_error) ) )
    | Ok `Empty -> Lwt.return_ok `Empty

  let pull_exn t ?depth remote kind =
    pull t ?depth remote kind >>= function
    | Ok x -> Lwt.return x
    | Error e -> invalid_argf "Sync.pull_exn: %a" pp_pull_error e

  type push_error = [ `Msg of string | `Detached_head ]

  let pp_push_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Detached_head -> Fmt.string ppf "cannot push to a non-persistent store"

  let push t ?depth remote =
    Log.debug (fun f -> f "push");
    match remote with
    | Store ((module R), r) -> (
        S.Head.find t >>= function
        | None -> Lwt.return_ok `Empty
        | Some h -> (
            Log.debug (fun f -> f "push store");
            R.Repo.heads (R.repo r) >>= fun min ->
            let r_repo = R.repo r in
            let s_repo = S.repo t in
            let min = convs R.(commit_t r_repo) S.(commit_t s_repo) min in
            S.Repo.export (S.repo t) ?depth ~min >>= fun s_slice ->
            convert_slice (module S.Private) (module R.Private) s_slice
            >>= fun r_slice ->
            R.Repo.import (R.repo r) r_slice >>= function
            | Error e -> Lwt.return_error (e :> push_error)
            | Ok () -> (
                match conv S.(commit_t s_repo) R.(commit_t r_repo) h with
                | Error e -> Lwt.return_error (e :> push_error)
                | Ok h ->
                    R.Head.set r h >>= fun () ->
                    S.Head.get t >|= fun head -> Ok (`Head head) ) ) )
    | S.E e -> (
        match S.status t with
        | `Empty -> Lwt.return_ok `Empty
        | `Commit _ -> Lwt.return_error `Detached_head
        | `Branch br -> (
            S.of_branch (S.repo t) br >>= S.Head.get >>= fun head ->
            B.v (S.repo t) >>= fun g ->
            B.push g ?depth e br >>= function
            | Ok () -> Lwt.return_ok (`Head head)
            | Error err -> Lwt.return_error (err :> push_error) ) )
    | _ -> Lwt.return_error (`Msg "push operation is not available")

  let push_exn t ?depth remote =
    push t ?depth remote >>= function
    | Ok x -> Lwt.return x
    | Error e -> invalid_argf "Sync.push_exn: %a" pp_push_error e
end
