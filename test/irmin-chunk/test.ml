 (*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

let () =
  Printexc.record_backtrace true

let key_t: Test_chunk.Key.t Alcotest.testable = (module Test_chunk.Key)
let value_t: Test_chunk.Value.t Alcotest.testable = (module Test_chunk.Value)

let run f () =
  Lwt_main.run (f ());
  flush stderr;
  flush stdout

let test_add_read ?(stable=false) (module AO: Test_chunk.S) () =
  AO.v () >>= fun t ->
  let test size =
    let name = Printf.sprintf "size %d" size in
    let v = String.make size 'x' in
    AO.batch t (fun t -> AO.add t v) >>= fun k ->
    if stable then (
      let str = Irmin.Type.to_bin_string Test_chunk.Value.t v in
      Alcotest.(check key_t) (name ^ " is stable") k (Test_chunk.Key.digest str)
    );
    AO.find t k >|= fun v' ->
    Alcotest.(check @@ option value_t) name (Some v) v'
  in
  let x = 40 in
  Lwt_list.iter_s test [
    x-1  ; x  ; x+1;
    x*2-1; x*2; x*2+1;
    x*x-1; x*x; x*x+1;
    x*x*x;
  ]

let simple =
  "simple", [
    "add/read: in-memory"       , `Quick, run @@ test_add_read (module Test_chunk.Mem);
    "add/read: in-memory+chunks", `Quick, run @@ test_add_read (module Test_chunk.MemChunk);
  ]

let stable =
  let test stable = test_add_read ~stable (module Test_chunk.MemChunk) in
  "stable", [
    "add/read: simple", `Quick, run @@ test false;
    "add/read: stable", `Quick, run @@ test true;
  ]

let () =
  Irmin_test.Store.run "irmin-chunk" ~misc:[simple; stable] [
    `Quick, Test_chunk.suite
  ]
