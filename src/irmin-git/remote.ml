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

open Import
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Make
    (G : Git.S)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (B : Irmin.Branch.S) =
struct
  let src = Logs.Src.create "irmin.git-remote" ~doc:"Git remote"

  module Gitlog = (val Logs.src_log src : Logs.LOG)
  module H = Irmin.Hash.Make (G.Hash)

  type t = G.t
  type commit = H.t
  type branch = B.t
  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  let git_of_branch_str str = Git.Reference.v ("refs/heads/" ^ str)
  let git_of_branch r = git_of_branch_str (Irmin.Type.to_string B.t r)

  (* let o_head_of_git = function None -> Ok None | Some k -> Ok (Some k) *)

  let msgf fmt = Fmt.kstr (fun err -> `Msg err) fmt
  let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

  let fetch t ?depth (ctx, e) br =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "fetch %a" Smart_git.Endpoint.pp e];
    let push_stdout msg = Gitlog.info (fun f -> f "%s" msg)
    and push_stderr msg = Gitlog.warn (fun f -> f "%s" msg)
    and deepen =
      match depth with Some depth -> Some (`Depth depth) | None -> None
    and reference = git_of_branch br
    and capabilities =
      [
        `Side_band_64k;
        `Multi_ack_detailed;
        `Ofs_delta;
        `Thin_pack;
        `Report_status;
      ]
    in
    S.fetch ~push_stdout ~push_stderr ~capabilities ~ctx e t ?deepen
      (`Some [ (reference, reference) ])
    >>= function
    | Error `Not_found -> Lwt.return (Error (`Msg "not found"))
    | Error (`Msg err) -> Lwt.return (Error (`Msg err))
    | Error (`Exn err) -> Lwt.return (Error (`Msg (Printexc.to_string err)))
    | Error err ->
        Fmt.kstr (fun e -> Lwt.return (Error (`Msg e))) "%a" S.pp_error err
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (_, [ (reference, hash) ])) ->
        let value = Git.Reference.uid hash in
        let br =
          Git.Reference.v ("refs/remotes/origin/" ^ Irmin.Type.to_string B.t br)
        in
        G.Ref.write t br value >|= reword_error (msgf "%a" G.pp_error)
        >>? fun () ->
        G.Ref.write t reference value >|= reword_error (msgf "%a" G.pp_error)
        >>? fun () -> Lwt.return (Ok (Some hash))
    | _ -> assert false

  let push t ?depth:_ (ctx, e) br =
    Lwt_eio.run_lwt @@ fun () ->
    [%log.debug "push %a" Smart_git.Endpoint.pp e];
    let reference = git_of_branch br in
    let capabilities =
      [
        `Side_band_64k;
        `Multi_ack_detailed;
        `Ofs_delta;
        `Thin_pack;
        `Report_status;
      ]
    in
    S.push ~capabilities ~ctx e t [ `Update (reference, reference) ]
    >|= function
    | Error (`Msg err) -> Error (`Msg err)
    | Error (`Exn exn) -> Error (`Msg (Printexc.to_string exn))
    | Error `Not_found -> Error (`Msg "not found")
    | Error err -> Error (`Msg (Fmt.str "%a" S.pp_error err))
    | Ok () -> Ok ()
end
