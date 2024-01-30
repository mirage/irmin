(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Irmin.Export_for_backends

let () = Printexc.record_backtrace true
let key_t : Test_chunk.Key.t Alcotest.testable = (module Test_chunk.Key)
let value_t : Test_chunk.Value.t Alcotest.testable = (module Test_chunk.Value)

let run f () =
  let () = f () in
  flush stderr;
  flush stdout

let hash x = Test_chunk.Key.hash (fun l -> l x)
let hash_contents x = hash ("B" ^ x)
let value_to_bin = Irmin.Type.(unstage (to_bin_string Test_chunk.Value.t))

let test_add_read ?(stable = false) (module AO : Test_chunk.S) () =
  Eio.Switch.run @@ fun sw ->
  let t = AO.v ~sw in
  let test size =
    let name = Printf.sprintf "size %d" size in
    let v = String.make size 'x' in
    let k = AO.batch t (fun t -> AO.add t v) in
    (if stable then
       let str = value_to_bin v in
       Alcotest.(check key_t) (name ^ " is stable") k (hash_contents str));
    let v' = AO.find t k in
    Alcotest.(check @@ option value_t) name (Some v) v'
  in
  let x = 40 in
  List.iter test
    [
      x - 1;
      x;
      x + 1;
      (x * 2) - 1;
      x * 2;
      (x * 2) + 1;
      (x * x) - 1;
      x * x;
      (x * x) + 1;
      x * x * x;
    ]

let simple =
  ( "simple",
    [
      ( "add/read: in-memory",
        `Quick,
        run @@ test_add_read (module Test_chunk.Mem) );
      ( "add/read: in-memory+chunks",
        `Quick,
        run @@ test_add_read (module Test_chunk.MemChunk) );
    ] )

let stable =
  let test stable = test_add_read ~stable (module Test_chunk.MemChunk) in
  ( "stable",
    [
      ("add/read: simple", `Quick, run @@ test false);
      ("add/read: stable", `Quick, run @@ test true);
    ] )

let () =
  Eio_main.run @@ fun _env ->
  Irmin_test.Store.run "irmin-chunk" ~slow:true ~misc:[ simple; stable ]
    ~sleep:Eio_unix.sleep
    [ (`Quick, Test_chunk.suite) ]
