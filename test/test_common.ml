(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type Irmin_git_S = Irmin_git.S
open Astring

open Irmin_unix

module type Test_S = sig
  include Irmin.S
  module Internals: sig
    val commit_of_id: Repo.t -> commit_id -> Git.Commit.t option Lwt.t
  end
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
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("%s%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
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

let cmp_opt fn x y =
  match x, y with
  | Some x, Some y -> fn x y
  | None  , None   -> true
  | Some _, None
  | None  , Some _ -> false

let printer_opt fn = function
  | None   -> "<none>"
  | Some v -> fn v

let rec cmp_list fn x y =
  match x, y with
  | xh::xt, yh::yt -> fn xh yh && cmp_list fn xt yt
  | []    , []     -> true
  | _              -> false

let printer_list fn = function
  | [] -> "[]"
  | l  -> Printf.sprintf "[ %s ]"
            (String.concat ~sep:", " (List.map fn l))

let line msg =
  let line () = Alcotest.line stderr ~color:`Yellow '-' in
  line ();
  Logs.info (fun f -> f "ASSERT %s" msg);
  line ()

module Make (S: Irmin.S) = struct

  let cmp_list eq comp l1 l2 =
    cmp_list eq (List.sort comp l1) (List.sort comp l2)

  let error msg expected got =
    let msg = Printf.sprintf "Fail %s: expecting %s, got %s" msg expected got in
    failwith msg

  let aux cmp printer msg x y =
    line msg;
    if not (cmp x y) then error msg (printer x) (printer y)

  let assert_equal (type t) (module S: Tc.S0 with type t = t) msg =
    aux S.equal (Tc.show (module S)) msg

  module Set (S: Tc.S0) = struct
    module L = Tc.List(S)
    include L
    let compare x y =
      let x = List.sort S.compare x in
      let y = List.sort S.compare y in
      L.compare x y
    let equal x y =
      let x = List.sort S.compare x in
      let y = List.sort S.compare y in
      L.equal x y
  end

  module KV = S.Private.Contents.Key
  module KN = S.Private.Node.Key
  module KC = S.Hash

  module RV = Tc.App1(Irmin.Merge.Result)(Tc.Option(KV))
  module RN = Tc.App1(Irmin.Merge.Result)(KN)
  module RC = Tc.App1(Irmin.Merge.Result)(KC)

  module Contents = S.Private.Contents
  module Node = S.Private.Node
  module Commit = S.Private.Commit

  module T = S.Ref
  module K = S.Key
  module V = S.Val
  module N = Node.Val
  module C = Commit.Val

  module Succ = Set( Tc.Pair(S.Key.Step)(S.Private.Node.Key) )

end

open Lwt

let create: (module Irmin.S_MAKER) -> [`String | `Json] -> (module Test_S) =
  fun (module B) c ->
    let (module C: Irmin.Contents.S) = match c with
      | `String -> (module Irmin.Contents.String)
      | `Json   -> (module Irmin.Contents.Json)
    in
    let module S = struct
      include B(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)
      module Internals = struct
        let commit_of_id _t _id = failwith "Only used for testing Git stores"
      end
    end
    in (module S)

type kind = [`Mem | `Fs | `Git | `Http of kind]

type t = {
  name  : string;
  kind  : kind;
  cont  : [`Json | `String];
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config: Irmin.config;
  store : (module Test_S);
}

let none () =
  return_unit

let string_of_contents = function
  | `Json   -> "-json"
  | `String -> ""

let (/) = Filename.concat

let mem_store = create (module Irmin_mem.Make)
let irf_store = create (module Irmin_fs.Make)

let http_store (module S: Test_S) =
  create (module Irmin_http.Make(S.Private.Node.Val.Metadata))

let git_store c =
  let (module C: Irmin.Contents.S) = match c with
    | `String -> (module Irmin.Contents.String)
    | `Json   -> (module Irmin.Contents.Json)
  in
  let module S = Irmin_git.FS(C)(Irmin.Ref.String)(Irmin.Hash.SHA1) in
  (module S: Irmin_git_S)
