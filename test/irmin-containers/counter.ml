(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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
module C = Irmin_containers.Counter.Mem

let path = [ "tmp"; "counter" ]

let config ~sw root =
  let key = Irmin.Backend.Conf.root Irmin_mem.Conf.spec in
  let conf = Irmin.Backend.Conf.singleton Irmin_mem.Conf.spec key root in
  C.Store.Repo.v ~sw conf

let merge_into_exn = merge_into_exn (module C.Store)

let test_inc ~fs () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw ~fs __FUNCTION__ |> C.Store.main in
  C.inc ~path t;
  let () =
    C.read ~path t
    |> Alcotest.(check int64) "checked - increment without using by" 1L
  in
  C.inc ~by:2L ~path t;
  C.read ~path t |> Alcotest.(check int64) "checked - increment using by" 3L

let test_dec ~fs () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw ~fs __FUNCTION__ |> C.Store.main in
  C.dec ~path t;
  let () =
    C.read ~path t
    |> Alcotest.(check int64) "checked - decrement without using by" (-1L)
  in
  C.dec ~by:2L ~path t;
  C.read ~path t |> Alcotest.(check int64) "checked - decrement using by" (-3L)

let test_clone_merge ~fs () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw ~fs __FUNCTION__ |> C.Store.main in
  C.inc ~by:5L ~path t;
  let b = C.Store.clone ~src:t ~dst:"cl" in
  C.inc ~by:2L ~path b;
  C.dec ~by:4L ~path t;
  let () =
    C.read ~path t |> Alcotest.(check int64) "checked - value of main" 1L
  in
  let () =
    C.read ~path b |> Alcotest.(check int64) "checked - value of clone" 7L
  in
  merge_into_exn b ~into:t;
  C.read t ~path
  |> Alcotest.(check int64) "checked - value of main after merging" 3L

let test_branch_merge ~fs () =
  Eio.Switch.run @@ fun sw ->
  let r = config ~sw ~fs __FUNCTION__ in
  let b1 = C.Store.of_branch r "b1" in
  let b2 = C.Store.of_branch r "b2" in
  let b3 = C.Store.of_branch r "b3" in
  let b4 = C.Store.of_branch r "b4" in
  C.inc ~by:5L ~path b1;
  C.dec ~by:2L ~path b2;
  merge_into_exn b1 ~into:b3;
  merge_into_exn b2 ~into:b3;
  merge_into_exn b2 ~into:b4;
  merge_into_exn b1 ~into:b4;
  let () =
    C.read ~path b3 |> Alcotest.(check int64) "checked - value of b3" 3L
  in
  C.read ~path b4 |> Alcotest.(check int64) "checked - value of b4" 3L

let test_cases ~fs =
  [
    ( "counter",
      [
        Alcotest.test_case "Increment" `Quick (test_inc ~fs);
        Alcotest.test_case "Decrement" `Quick (test_dec ~fs);
      ] );
    ( "counter store",
      [
        Alcotest.test_case "Clone and merge" `Quick (test_clone_merge ~fs);
        Alcotest.test_case "Branch and merge" `Quick (test_branch_merge ~fs);
      ] );
  ]
