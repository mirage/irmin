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

open Astring

module type S =
  Irmin.S
    with type step = string
     and type key = string list
     and type contents = string
     and type branch = string

let ignore_srcs src =
  List.mem (Logs.Src.name src)
    [
      "git.inflater.decoder";
      "git.deflater.encoder";
      "git.encoder";
      "git.decoder";
      "git.loose";
      "git.store";
      "cohttp.lwt.io";
    ]

let reporter ?(prefix = "") () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (pad 15 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    if ignore_srcs src then Format.ikfprintf k ppf fmt
    else with_stamp header tags k fmt
  in
  { Logs.report }

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())

let line ppf ?color c =
  let line = String.v ~len:80 (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string) line
  | None -> Fmt.pf ppf "%s\n%!" line

let line msg =
  let line () = line Fmt.stderr ~color:`Yellow '-' in
  line ();
  Logs.info (fun f -> f "ASSERT %s" msg);
  line ()

let store : (module Irmin.S_MAKER) -> (module Irmin.Metadata.S) -> (module S) =
 fun (module B) (module M) ->
  let module S =
    B (M) (Irmin.Contents.String) (Irmin.Path.String_list) (Irmin.Branch.String)
      (Irmin.Hash.SHA1)
  in
  (module S)

type t = {
  name : string;
  init : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config : Irmin.config;
  store : (module S);
  stats : (unit -> int * int) option;
}

let ( / ) = Filename.concat

let testable t = Alcotest.testable (Irmin.Type.pp t) (Irmin.Type.equal t)

let check t = Alcotest.check (testable t)

let checks t =
  let t = Alcotest.slist (testable t) Irmin.Type.(compare t) in
  Alcotest.check t
