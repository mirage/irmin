(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
open Common
module S = Test_gc.Store
module Dispatcher = Irmin_pack_unix.Dispatcher.Make (File_manager)

let root = Filename.concat "_build" "test-dispatcher"
let src = Logs.Src.create "tests.dispatcher" ~doc:"Test dispatcher"

module Log = (val Logs.src_log src : Logs.LOG)

let setup_store () =
  rm_dir root;
  let config = S.config root in
  let* t = S.init_with_config config in
  let* _ = S.commit_1 t in
  let* t, c2 = S.commit_2 t in
  let* t = S.checkout_exn t c2 in
  let* t, _c3 = S.commit_3 t in
  [%log.debug "Gc c1, keep c2, c3"];
  let* () = S.start_gc t c2 in
  let* () = S.finalise_gc t in
  let* () = S.close t in
  Lwt.return config

type t = { off : Int63.t; len : int; hex : string }

(* predefined values based on [setup_store]. *)
(* node_1 belongs to commit_1, it is removed from the store. *)
let node_1 = { off = Int63.of_int 30; len = 26; hex = "" }

(* node_2 belongs to commit_2, it is in the prefix. *)
let node_2 =
  {
    off = Int63.of_int 240;
    len = 35;
    hex =
      "db1998ebe97d8ddffef5fcc7a235f16f33f3ccb6520d000108016100000000000000cd";
  }

let commit_2 =
  {
    off = Int63.of_int 275;
    len = 42;
    hex =
      "8fcdbb49a171d9f5eb578c094d24a3ccaaa156c444140000000000000000f00000000000000000000000";
  }

(* node_3 belongs to commit_3, it is in the suffix. *)
let node_3 =
  {
    off = Int63.of_int 346;
    len = 46;
    hex =
      "2b5a114440d5fcef4b20b85171af8d140dfc7eca5218000206016400000000000000b3060166000000000000013d";
  }

let check_hex msg buf expected =
  Alcotest.(check string)
    msg expected
    (Bytes.to_string buf |> Hex.of_string |> Hex.show)

let test_read () =
  let* config = setup_store () in
  let fm = File_manager.open_ro config |> Errs.raise_if_error in
  let dsp = Dispatcher.v fm |> Errs.raise_if_error in
  let _ =
    Alcotest.check_raises "cannot read node_1"
      (Irmin_pack_unix.Errors.Pack_error
         (`Invalid_sparse_read (`Before, Int63.of_int 30)))
      (fun () ->
        let buf = Bytes.create node_1.len in
        let _ = Dispatcher.read_exn dsp ~off:node_1.off ~len:node_1.len buf in
        ())
  in
  let test_accessor msg obj =
    let buf = Bytes.create obj.len in
    let _ = Dispatcher.read_exn dsp ~off:obj.off ~len:obj.len buf in
    check_hex msg buf obj.hex
  in
  test_accessor "node_2" node_2;
  test_accessor "commit_2" commit_2;
  test_accessor "node_3" node_3;

  File_manager.close fm |> Errs.raise_if_error;
  Lwt.return_unit

let tests = [ Alcotest_lwt.test_case "read" `Quick (fun _switch -> test_read) ]
