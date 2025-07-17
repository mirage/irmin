(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
include Sync_intf

module type REMOTE = Remote.S

let invalid_argf fmt = Fmt.kstr invalid_arg fmt
let src = Logs.Src.create "irmin.sync" ~doc:"Irmin remote sync"

module Log = (val Logs.src_log src : Logs.LOG)

let remote_store m x = Store.Store (m, x)

module Make (S : Store.Generic_key.S) = struct
  module B = S.Backend.Remote

  type db = S.t
  type commit = S.commit
  type commit_key = S.commit_key [@@deriving irmin ~pp]
  type info = S.info

  let conv dx dy =
    let dx_to_bin_string = Type.(unstage (to_bin_string dx)) in
    let dy_of_bin_string = Type.(unstage (of_bin_string dy)) in
    Type.stage (fun x -> dy_of_bin_string (dx_to_bin_string x))

  let convert_slice (type r s) (module RP : Backend.S with type Slice.t = r)
      (module SP : Backend.S with type Slice.t = s) r =
    let conv_contents_k =
      Type.unstage (conv RP.Contents.Hash.t SP.Contents.Hash.t)
    in
    let conv_contents_v =
      Type.unstage (conv RP.Contents.Val.t SP.Contents.Val.t)
    in
    let conv_node_k = Type.unstage (conv RP.Node.Hash.t SP.Node.Hash.t) in
    let conv_node_v = Type.unstage (conv RP.Node.Val.t SP.Node.Val.t) in
    let conv_commit_k = Type.unstage (conv RP.Commit.Hash.t SP.Commit.Hash.t) in
    let conv_commit_v = Type.unstage (conv RP.Commit.Val.t SP.Commit.Val.t) in
    let s = SP.Slice.empty () in
    RP.Slice.iter r (function
      | `Contents (k, v) -> (
          let k = conv_contents_k k in
          let v = conv_contents_v v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Contents (k, v))
          | _ -> ())
      | `Node (k, v) -> (
          let k = conv_node_k k in
          let v = conv_node_v v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Node (k, v))
          | _ -> ())
      | `Commit (k, v) -> (
          let k = conv_commit_k k in
          let v = conv_commit_v v in
          match (k, v) with
          | Ok k, Ok v -> SP.Slice.add s (`Commit (k, v))
          | _ -> ()));
    s

  let convs src dst l =
    let conv = Type.unstage (conv src dst) in
    List.fold_left
      (fun acc x -> match conv x with Ok x -> x :: acc | _ -> acc)
      [] l

  let pp_branch = Type.pp S.Branch.t

  type status = [ `Empty | `Head of commit ]

  let pp_status ppf = function
    | `Empty -> Fmt.string ppf "empty"
    | `Head c -> S.Commit.pp_hash ppf c

  let status_t t =
    let open Type in
    variant "status" (fun empty head -> function
      | `Empty -> empty
      | `Head c -> head c)
    |~ case0 "empty" `Empty
    |~ case1 "head" S.(commit_t @@ repo t) (fun c -> `Head c)
    |> sealv

  let fetch t ?depth remote =
    match remote with
    | Store.Store ((module R), r) -> (
        [%log.debug "fetch store"];
        let s_repo = S.repo t in
        let r_repo = R.repo r in
        let conv =
          Type.unstage (conv R.(commit_t r_repo) S.(commit_t s_repo))
        in
        let min = S.Repo.heads s_repo in
        let min = convs S.(commit_t s_repo) R.(commit_t r_repo) min in
        match R.Head.find r with
        | None -> Ok `Empty
        | Some h -> (
            let r_slice =
              R.Repo.export (R.repo r) ?depth ~min ~max:(`Max [ h ])
            in
            let s_slice =
              convert_slice (module R.Backend) (module S.Backend) r_slice
            in
            match S.Repo.import s_repo s_slice with
            | Error e -> Error e
            | Ok () -> (
                match conv h with Ok h -> Ok (`Head h) | Error e -> Error e)))
    | S.E e -> (
        match S.status t with
        | `Empty | `Commit _ -> Ok `Empty
        | `Branch br -> (
            [%log.debug "Fetching branch %a" pp_branch br];
            let g = B.v (S.repo t) in
            match B.fetch g ?depth e br with
            | Error _ as e -> e
            | Ok (Some key) -> (
                [%log.debug "Fetched %a" pp_commit_key key];
                match S.Commit.of_key (S.repo t) key with
                | None -> Ok `Empty
                | Some x -> Ok (`Head x))
            | Ok None -> (
                match S.Head.find t with
                | Some h -> Ok (`Head h)
                | None -> Ok `Empty)))
    | _ -> Error (`Msg "fetch operation is not available")

  let fetch_exn t ?depth remote =
    match fetch t ?depth remote with
    | Ok h -> h
    | Error (`Msg e) -> invalid_argf "Sync.fetch_exn: %s" e

  type pull_error = [ `Msg of string | Merge.conflict ]

  let pp_pull_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Conflict c -> Fmt.pf ppf "conflict: %s" c

  let pull t ?depth remote kind : (status, pull_error) result =
    match fetch t ?depth remote with
    | Error e -> Error (e :> pull_error)
    | Ok (`Head k) -> (
        match kind with
        | `Set ->
            S.Head.set t k;
            Ok (`Head k)
        | `Merge info -> (
            match S.Head.merge ~into:t ~info k with
            | Ok () -> Ok (`Head k)
            | Error e -> Error (e :> pull_error)))
    | Ok `Empty -> Ok `Empty

  let pull_exn t ?depth remote kind =
    match pull t ?depth remote kind with
    | Ok x -> x
    | Error e -> invalid_argf "Sync.pull_exn: %a" pp_pull_error e

  type push_error = [ `Msg of string | `Detached_head ]

  let pp_push_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Detached_head -> Fmt.string ppf "cannot push to a non-persistent store"

  let push t ?depth remote =
    [%log.debug "push"];
    match remote with
    | Store.Store ((module R), r) -> (
        match S.Head.find t with
        | None -> Ok `Empty
        | Some h -> (
            [%log.debug "push store"];
            let min = R.Repo.heads (R.repo r) in
            let r_repo = R.repo r in
            let s_repo = S.repo t in
            let min = convs R.(commit_t r_repo) S.(commit_t s_repo) min in
            let conv =
              Type.unstage (conv S.(commit_t s_repo) R.(commit_t r_repo))
            in
            let s_slice = S.Repo.export (S.repo t) ?depth ~min in
            let r_slice =
              convert_slice (module S.Backend) (module R.Backend) s_slice
            in
            match R.Repo.import (R.repo r) r_slice with
            | Error e -> Error (e :> push_error)
            | Ok () -> (
                match conv h with
                | Error e -> Error (e :> push_error)
                | Ok h ->
                    R.Head.set r h;
                    let head = S.Head.get t in
                    Ok (`Head head))))
    | S.E e -> (
        match S.status t with
        | `Empty -> Ok `Empty
        | `Commit _ -> Error `Detached_head
        | `Branch br -> (
            let head = S.of_branch (S.repo t) br |> S.Head.get in
            let g = B.v (S.repo t) in
            match B.push g ?depth e br with
            | Ok () -> Ok (`Head head)
            | Error err -> Error (err :> push_error)))
    | _ -> Error (`Msg "push operation is not available")

  let push_exn t ?depth remote =
    match push t ?depth remote with
    | Ok x -> x
    | Error e -> invalid_argf "Sync.push_exn: %a" pp_push_error e
end
