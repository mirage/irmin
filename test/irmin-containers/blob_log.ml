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

open Common
open Lwt.Infix
module B = Irmin_containers.Blob_log.Mem (Irmin.Contents.String)

let path = [ "tmp"; "blob" ]
let config () = B.Store.Repo.v (Irmin_mem.config ())
let merge_into_exn = merge_into_exn (module B.Store)

let test_empty_read _ () =
  config ()
  >>= B.Store.master
  >>= B.read_all ~path
  >|= Alcotest.(check (list string)) "checked - reading empty log" []

let test_append _ () =
  config () >>= B.Store.master >>= fun t ->
  B.append ~path t "master.1" >>= fun () ->
  B.append ~path t "master.2" >>= fun () ->
  B.read_all ~path t
  >|= Alcotest.(check (list string))
        "checked - log after appending" [ "master.2"; "master.1" ]

let test_clone_merge _ () =
  config () >>= B.Store.master >>= fun t ->
  B.Store.clone ~src:t ~dst:"cl" >>= fun b ->
  B.append ~path b "clone.1" >>= fun () ->
  B.append ~path t "master.3" >>= fun () ->
  merge_into_exn b ~into:t >>= fun () ->
  B.read_all ~path t
  >|= Alcotest.(check (list string))
        "checked - log after appending"
        [ "master.3"; "clone.1"; "master.2"; "master.1" ]

let test_branch_merge _ () =
  config () >>= fun r ->
  B.Store.of_branch r "b1" >>= fun b1 ->
  B.Store.of_branch r "b2" >>= fun b2 ->
  B.Store.of_branch r "b3" >>= fun b3 ->
  B.Store.of_branch r "b4" >>= fun b4 ->
  B.append ~path b1 "b1.1" >>= fun () ->
  B.append ~path b2 "b2.1" >>= fun () ->
  B.append ~path b1 "b1.2" >>= fun () ->
  B.append ~path b1 "b1.3" >>= fun () ->
  B.append ~path b2 "b2.2" >>= fun () ->
  B.append ~path b1 "b1.4" >>= fun () ->
  merge_into_exn b1 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b4 >>= fun () ->
  merge_into_exn b1 ~into:b4 >>= fun () ->
  B.read_all ~path b3
  >|= Alcotest.(check (list string))
        "checked - value of b3"
        [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]
  >>= fun () ->
  B.read_all ~path b4
  >|= Alcotest.(check (list string))
        "checked - value of b4"
        [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]

let test_cases =
  [
    ( "blob_log",
      [
        Alcotest_lwt.test_case "Read empty log" `Quick test_empty_read;
        Alcotest_lwt.test_case "Append" `Quick test_append;
      ] );
    ( "blob_log store",
      [
        Alcotest_lwt.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest_lwt.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
