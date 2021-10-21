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
let config () = C.Store.Repo.v (Irmin_mem.config ())
let merge_into_exn = merge_into_exn (module C.Store)

let test_inc _ () =
  let* t = config () >>= C.Store.main in
  C.inc ~path t >>= fun () ->
  let* () =
    C.read ~path t
    >|= Alcotest.(check int64) "checked - increment without using by" 1L
  in
  C.inc ~by:2L ~path t >>= fun () ->
  C.read ~path t >|= Alcotest.(check int64) "checked - increment using by" 3L

let test_dec _ () =
  let* t = config () >>= C.Store.main in
  C.dec ~path t >>= fun () ->
  let* () =
    C.read ~path t
    >|= Alcotest.(check int64) "checked - decrement without using by" 2L
  in
  C.dec ~by:2L ~path t >>= fun () ->
  C.read ~path t >|= Alcotest.(check int64) "checked - decrement using by" 0L

let test_clone_merge _ () =
  let* t = config () >>= C.Store.main in
  C.inc ~by:5L ~path t >>= fun () ->
  let* b = C.Store.clone ~src:t ~dst:"cl" in
  C.inc ~by:2L ~path b >>= fun () ->
  C.dec ~by:4L ~path t >>= fun () ->
  let* () =
    C.read ~path t >|= Alcotest.(check int64) "checked - value of main" 1L
  in
  let* () =
    C.read ~path b >|= Alcotest.(check int64) "checked - value of clone" 7L
  in
  merge_into_exn b ~into:t >>= fun () ->
  C.read t ~path
  >|= Alcotest.(check int64) "checked - value of main after merging" 3L

let test_branch_merge _ () =
  let* r = config () in
  let* b1 = C.Store.of_branch r "b1" in
  let* b2 = C.Store.of_branch r "b2" in
  let* b3 = C.Store.of_branch r "b3" in
  let* b4 = C.Store.of_branch r "b4" in
  C.inc ~by:5L ~path b1 >>= fun () ->
  C.dec ~by:2L ~path b2 >>= fun () ->
  merge_into_exn b1 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b4 >>= fun () ->
  merge_into_exn b1 ~into:b4 >>= fun () ->
  let* () =
    C.read ~path b3 >|= Alcotest.(check int64) "checked - value of b3" 3L
  in
  C.read ~path b4 >|= Alcotest.(check int64) "checked - value of b4" 3L

let test_cases =
  [
    ( "counter",
      [
        Alcotest_lwt.test_case "Increment" `Quick test_inc;
        Alcotest_lwt.test_case "Decrement" `Quick test_dec;
      ] );
    ( "counter store",
      [
        Alcotest_lwt.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest_lwt.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
