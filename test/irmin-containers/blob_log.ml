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
module B = Irmin_containers.Blob_log.Mem (Irmin.Contents.String)

let path = [ "tmp"; "blob" ]
let config ~sw = B.Store.Repo.v ~sw (Irmin_mem.config ())
let merge_into_exn = merge_into_exn (module B.Store)

let test_empty_read () =
  Eio.Switch.run @@ fun sw ->
  let config = config ~sw in
  let main = B.Store.main config in
  B.read_all ~path main
  |> Alcotest.(check (list string)) "checked - reading empty log" []

let test_append () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw |> B.Store.main in
  B.append ~path t "main.1";
  B.append ~path t "main.2";
  B.read_all ~path t
  |> Alcotest.(check (list string))
       "checked - log after appending" [ "main.2"; "main.1" ]

let test_clone_merge () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw |> B.Store.main in
  B.append ~path t "main.1";
  B.append ~path t "main.2";
  let b = B.Store.clone ~src:t ~dst:"cl" in
  B.append ~path b "clone.1";
  B.append ~path t "main.3";
  merge_into_exn b ~into:t;
  B.read_all ~path t
  |> Alcotest.(check (list string))
       "checked - log after appending"
       [ "main.3"; "clone.1"; "main.2"; "main.1" ]

let test_branch_merge () =
  Eio.Switch.run @@ fun sw ->
  let r = config ~sw in
  let b1 = B.Store.of_branch r "b1" in
  let b2 = B.Store.of_branch r "b2" in
  let b3 = B.Store.of_branch r "b3" in
  let b4 = B.Store.of_branch r "b4" in
  B.append ~path b1 "b1.1";
  B.append ~path b2 "b2.1";
  B.append ~path b1 "b1.2";
  B.append ~path b1 "b1.3";
  B.append ~path b2 "b2.2";
  B.append ~path b1 "b1.4";
  merge_into_exn b1 ~into:b3;
  merge_into_exn b2 ~into:b3;
  merge_into_exn b2 ~into:b4;
  merge_into_exn b1 ~into:b4;
  let () =
    B.read_all ~path b3
    |> Alcotest.(check (list string))
         "checked - value of b3"
         [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]
  in
  B.read_all ~path b4
  |> Alcotest.(check (list string))
       "checked - value of b4"
       [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]

let test_cases =
  [
    ( "blob_log",
      [
        Alcotest.test_case "Read empty log" `Quick test_empty_read;
        Alcotest.test_case "Append" `Quick test_append;
      ] );
    ( "blob_log store",
      [
        Alcotest.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
