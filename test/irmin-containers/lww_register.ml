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

module In = struct
  type t = int

  let t = Irmin.Type.int
end

module L = Irmin_containers.Lww_register.Mem (In)

let merge_into_exn = merge_into_exn (module L.Store)
let path = [ "tmp"; "lww" ]
let config ~sw = L.Store.Repo.v ~sw (Irmin_mem.config ())

let test_empty_read () =
  Eio.Switch.run @@ fun sw ->
  config ~sw
  |> L.Store.main
  |> L.read ~path
  |> Alcotest.(check (option int))
       "checked - reading register without writing" None

let test_write () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw |> L.Store.main in
  L.write ~path t 1;
  L.write ~path t 3;
  L.read ~path t
  |> Alcotest.(check (option int)) "checked - writing to register" (Some 3)

let test_clone_merge () =
  Eio.Switch.run @@ fun sw ->
  let t = config ~sw |> L.Store.main in
  let b = L.Store.clone ~src:t ~dst:"cl" in
  L.write ~path t 5;
  L.write ~path b 10;
  let () =
    L.read ~path t
    |> Alcotest.(check (option int)) "checked - value of main" (Some 5)
  in
  let () =
    L.read ~path b
    |> Alcotest.(check (option int)) "checked - value of clone" (Some 10)
  in
  merge_into_exn b ~into:t;
  L.read ~path t
  |> Alcotest.(check (option int))
       "checked - value of main after merging" (Some 10)

let test_branch_merge () =
  Eio.Switch.run @@ fun sw ->
  let r = config ~sw in
  let b1 = L.Store.of_branch r "b1" in
  let b2 = L.Store.of_branch r "b2" in
  let b3 = L.Store.of_branch r "b3" in
  let b4 = L.Store.of_branch r "b4" in
  L.write ~path b1 6;
  L.write ~path b2 3;
  merge_into_exn b1 ~into:b3;
  merge_into_exn b2 ~into:b3;
  merge_into_exn b2 ~into:b4;
  merge_into_exn b1 ~into:b4;
  let () =
    L.read ~path b3
    |> Alcotest.(check (option int)) "checked - value of b3" (Some 3)
  in
  L.read ~path b4
  |> Alcotest.(check (option int)) "checked - value of b4" (Some 3)

let test_cases =
  [
    ( "lww_register",
      [
        Alcotest.test_case "Read" `Quick test_empty_read;
        Alcotest.test_case "Write" `Quick test_write;
      ] );
    ( "lww_register store",
      [
        Alcotest.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
