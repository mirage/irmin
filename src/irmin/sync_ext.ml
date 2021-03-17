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

open! Import
include Sync_ext_intf

let src = Logs.Src.create "irmin.sync" ~doc:"Irmin remote sync"

module Log = (val Logs.src_log src : Logs.LOG)

module Remote (IO : IO.S) = struct
  module Merge = Merge.Make (IO)

  module type S =
    Store.S with type 'a io := 'a Merge.io and type 'a Merge.t = 'a Merge.t

  type S.remote += Store : (module S with type t = 'a) * 'a -> S.remote

  let remote_store m x = Store (m, x)
end

module Make
    (S : Store.S)
    (R : REMOTE with type 'a io := 'a S.io and type 'a merge = 'a S.Merge.t) =
struct
  module IO = S.IO
  open IO.Syntax
  module B = S.Private.Sync

  type db = S.t
  type commit = S.commit

  let ok x = IO.return (Ok x)
  let error e = IO.return (Error e)

  let invalid_argf fmt =
    Fmt.kstrf (fun msg -> IO.fail (Invalid_argument msg)) fmt

  let conv dx dy =
    let dx_to_bin_string = Type.(unstage (to_bin_string dx)) in
    let dy_of_bin_string = Type.(unstage (of_bin_string dy)) in
    Type.stage (fun x -> dy_of_bin_string (dx_to_bin_string x))

  module type P =
    Private.S with type 'a io := 'a S.io and type 'a merge := 'a S.Merge.t

  let convert_slice (type r s) (module RP : P with type Slice.t = r)
      (module SP : P with type Slice.t = s) r =
    let conv_contents_k =
      Type.unstage (conv RP.Contents.Key.t SP.Contents.Key.t)
    in
    let conv_contents_v =
      Type.unstage (conv RP.Contents.Val.t SP.Contents.Val.t)
    in
    let conv_node_k = Type.unstage (conv RP.Node.Key.t SP.Node.Key.t) in
    let conv_node_v = Type.unstage (conv RP.Node.Val.t SP.Node.Val.t) in
    let conv_commit_k = Type.unstage (conv RP.Commit.Key.t SP.Commit.Key.t) in
    let conv_commit_v = Type.unstage (conv RP.Commit.Val.t SP.Commit.Val.t) in
    let* s = SP.Slice.empty () in
    let+ () =
      RP.Slice.iter r (function
        | `Contents (k, v) -> (
            let k = conv_contents_k k in
            let v = conv_contents_v v in
            match (k, v) with
            | Ok k, Ok v -> SP.Slice.add s (`Contents (k, v))
            | _ -> IO.return ())
        | `Node (k, v) -> (
            let k = conv_node_k k in
            let v = conv_node_v v in
            match (k, v) with
            | Ok k, Ok v -> SP.Slice.add s (`Node (k, v))
            | _ -> IO.return ())
        | `Commit (k, v) -> (
            let k = conv_commit_k k in
            let v = conv_commit_v v in
            match (k, v) with
            | Ok k, Ok v -> SP.Slice.add s (`Commit (k, v))
            | _ -> IO.return ()))
    in
    s

  let convs src dst l =
    let conv = Type.unstage (conv src dst) in
    List.fold_left
      (fun acc x -> match conv x with Ok x -> x :: acc | _ -> acc)
      [] l

  let pp_branch = Type.pp S.Branch.t
  let pp_hash = Type.pp S.Hash.t

  type status = [ `Empty | `Head of commit ]

  let pp_status ppf = function
    | `Empty -> Fmt.string ppf "empty"
    | `Head c -> Type.pp S.Hash.t ppf (S.Commit.hash c)

  let status_t t =
    let open Type in
    variant "status" (fun empty head -> function
      | `Empty -> empty | `Head c -> head c)
    |~ case0 "empty" `Empty
    |~ case1 "head" S.(commit_t @@ repo t) (fun c -> `Head c)
    |> sealv

  let fetch t ?depth remote =
    match remote with
    | R.Store ((module R), r) -> (
        Log.debug (fun f -> f "fetch store");
        let s_repo = S.repo t in
        let r_repo = R.repo r in
        let conv =
          Type.unstage (conv R.(commit_t r_repo) S.(commit_t s_repo))
        in
        let* min = S.Repo.heads s_repo in
        let min = convs S.(commit_t s_repo) R.(commit_t r_repo) min in
        let* h = R.Head.find r in
        match h with
        | None -> ok `Empty
        | Some h -> (
            let* r_slice =
              R.Repo.export (R.repo r) ?depth ~min ~max:(`Max [ h ])
            in
            let* s_slice =
              convert_slice (module R.Private) (module S.Private) r_slice
            in
            let+ r = S.Repo.import s_repo s_slice in
            match r with
            | Error e -> Error e
            | Ok () -> (
                match conv h with Ok h -> Ok (`Head h) | Error e -> Error e)))
    | S.E e -> (
        match S.status t with
        | `Empty | `Commit _ -> ok `Empty
        | `Branch br -> (
            Log.debug (fun l -> l "Fetching branch %a" pp_branch br);
            let* g = B.v (S.repo t) in
            let* c = B.fetch g ?depth e br in
            match c with
            | Error _ as e -> IO.return e
            | Ok (Some c) -> (
                Log.debug (fun l -> l "Fetched %a" pp_hash c);
                let+ x = S.Commit.of_hash (S.repo t) c in
                match x with None -> Ok `Empty | Some x -> Ok (`Head x))
            | Ok None -> (
                let* h = S.Head.find t in
                match h with Some h -> ok (`Head h) | None -> ok `Empty)))
    | _ -> error (`Msg "fetch operation is not available")

  let fetch_exn t ?depth remote =
    let* r = fetch t ?depth remote in
    match r with
    | Ok h -> IO.return h
    | Error (`Msg e) -> invalid_argf "Sync.fetch_exn: %s" e

  type pull_error = [ `Msg of string | Merge.conflict ]

  let pp_pull_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Conflict c -> Fmt.pf ppf "conflict: %s" c

  let pull t ?depth remote kind : (status, pull_error) result IO.t =
    let* r = fetch t ?depth remote in
    match r with
    | Error e -> error (e :> pull_error)
    | Ok (`Head k) -> (
        match kind with
        | `Set ->
            let+ () = S.Head.set t k in
            Ok (`Head k)
        | `Merge info -> (
            let* r = S.Head.merge ~into:t ~info k in
            match r with
            | Ok () -> ok (`Head k)
            | Error e -> error (e :> pull_error)))
    | Ok `Empty -> ok `Empty

  let pull_exn t ?depth remote kind =
    let* r = pull t ?depth remote kind in
    match r with
    | Ok x -> IO.return x
    | Error e -> invalid_argf "Sync.pull_exn: %a" pp_pull_error e

  type push_error = [ `Msg of string | `Detached_head ]

  let pp_push_error ppf = function
    | `Msg s -> Fmt.string ppf s
    | `Detached_head -> Fmt.string ppf "cannot push to a non-persistent store"

  let push t ?depth remote =
    Log.debug (fun f -> f "push");
    match remote with
    | R.Store ((module R), r) -> (
        let* h = S.Head.find t in
        match h with
        | None -> ok `Empty
        | Some h -> (
            Log.debug (fun f -> f "push store");
            let* min = R.Repo.heads (R.repo r) in
            let r_repo = R.repo r in
            let s_repo = S.repo t in
            let min = convs R.(commit_t r_repo) S.(commit_t s_repo) min in
            let conv =
              Type.unstage (conv S.(commit_t s_repo) R.(commit_t r_repo))
            in
            let* s_slice = S.Repo.export (S.repo t) ?depth ~min in
            let* r_slice =
              convert_slice (module S.Private) (module R.Private) s_slice
            in
            let* i = R.Repo.import (R.repo r) r_slice in
            match i with
            | Error e -> error (e :> push_error)
            | Ok () -> (
                match conv h with
                | Error e -> error (e :> push_error)
                | Ok h ->
                    let* () = R.Head.set r h in
                    let+ head = S.Head.get t in
                    Ok (`Head head))))
    | S.E e -> (
        match S.status t with
        | `Empty -> ok `Empty
        | `Commit _ -> error `Detached_head
        | `Branch br -> (
            let* branch = S.of_branch (S.repo t) br in
            let* head = S.Head.get branch in
            let* g = B.v (S.repo t) in
            let* r = B.push g ?depth e br in
            match r with
            | Ok () -> ok (`Head head)
            | Error err -> error (err :> push_error)))
    | _ -> error (`Msg "push operation is not available")

  let push_exn t ?depth remote =
    let* r = push t ?depth remote in
    match r with
    | Ok x -> IO.return x
    | Error e -> invalid_argf "Sync.push_exn: %a" pp_push_error e
end
