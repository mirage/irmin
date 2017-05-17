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

module type Test_S = sig
  include Irmin.S with type step = string
                   and type key = string list
                   and type contents = string
                   and type branch = string
  val author: Repo.t -> commit -> string option Lwt.t
end

let reporter ?(prefix="") () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())

let line msg =
  let line () = Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  Logs.info (fun f -> f "ASSERT %s" msg);
  line ()

let store:
  (module Irmin.S_MAKER) -> (module Irmin.Metadata.S) -> (module Test_S) =
  fun (module B) (module M) ->
    let module S = struct
      include
        B (M)
          (Irmin.Contents.String)
          (Irmin.Path.String_list)
          (Irmin.Branch.String)
          (Irmin.Hash.SHA1)
      let author _t _id = failwith "Only used for testing Git stores"
    end
    in (module S)

type kind = [
  | `Core
  | `Git
  | `Http
  | `Unix
]

type t = {
  name  : string;
  kind  : kind;
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config: Irmin.config;
  store : (module Test_S);
  stats: (unit -> int * int) option;
}

let failf fmt = Fmt.kstrf Alcotest.fail fmt
let (/) = Filename.concat
let testable t = Alcotest.testable (Irmin.Type.dump t) (Irmin.Type.equal t)
let check t = Alcotest.check (testable t)
let checks t =
  let t = Alcotest.slist (testable t) Irmin.Type.(compare t) in
  Alcotest.check t
