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

module In = struct
  type t = int

  let t = Irmin.Type.int
end

module L = Irmin_containers.Lww_register.Mem (In)

let merge_into_exn = merge_into_exn (module L.Store)
let path = [ "tmp"; "lww" ]
let config () = L.Store.Repo.v (Irmin_mem.config ())

let test_empty_read _ () =
  config ()
  >>= L.Store.master
  >>= L.read ~path
  >|= Alcotest.(check (option int))
        "checked - reading register without writing" None

let test_write _ () =
  config () >>= L.Store.master >>= fun t ->
  L.write ~path t 1 >>= fun () ->
  L.write ~path t 3 >>= fun () ->
  L.read ~path t
  >|= Alcotest.(check (option int)) "checked - writing to register" (Some 3)

let test_clone_merge _ () =
  config () >>= L.Store.master >>= fun t ->
  L.Store.clone ~src:t ~dst:"cl" >>= fun b ->
  L.write ~path t 5 >>= fun () ->
  L.write ~path b 10 >>= fun () ->
  L.read ~path t
  >|= Alcotest.(check (option int)) "checked - value of master" (Some 5)
  >>= fun () ->
  L.read ~path b
  >|= Alcotest.(check (option int)) "checked - value of clone" (Some 10)
  >>= fun () ->
  merge_into_exn b ~into:t >>= fun () ->
  L.read ~path t
  >|= Alcotest.(check (option int))
        "checked - value of master after merging" (Some 10)

let test_branch_merge _ () =
  config () >>= fun r ->
  L.Store.of_branch r "b1" >>= fun b1 ->
  L.Store.of_branch r "b2" >>= fun b2 ->
  L.Store.of_branch r "b3" >>= fun b3 ->
  L.Store.of_branch r "b4" >>= fun b4 ->
  L.write ~path b1 6 >>= fun () ->
  L.write ~path b2 3 >>= fun () ->
  merge_into_exn b1 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b4 >>= fun () ->
  merge_into_exn b1 ~into:b4 >>= fun () ->
  L.read ~path b3
  >|= Alcotest.(check (option int)) "checked - value of b3" (Some 3)
  >>= fun () ->
  L.read ~path b4
  >|= Alcotest.(check (option int)) "checked - value of b4" (Some 3)

let test_cases =
  [
    ( "lww_register",
      [
        Alcotest_lwt.test_case "Read" `Quick test_empty_read;
        Alcotest_lwt.test_case "Write" `Quick test_write;
      ] );
    ( "lww_register store",
      [
        Alcotest_lwt.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest_lwt.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
