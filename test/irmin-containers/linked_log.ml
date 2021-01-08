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

module CAS = struct
  module CAS_Maker = Irmin.Content_addressable (Irmin_mem.Append_only)

  let config = Irmin_mem.config ()
end

module L = Irmin_containers.Linked_log.Mem (CAS) (Irmin.Contents.String) ()

let merge_into_exn = merge_into_exn (module L.Store)
let path = [ "tmp"; "link" ]
let config () = L.Store.Repo.v (Irmin_mem.config ())

let test_empty_read _ () =
  config ()
  >>= L.Store.master
  >>= L.read_all ~path
  >|= Alcotest.(check (list string)) "checked - reading empty log" []

let test_append_read_all _ () =
  config () >>= L.Store.master >>= fun t ->
  L.append ~path t "master.1" >>= fun () ->
  L.append ~path t "master.2" >>= fun () ->
  L.read_all ~path t
  >|= Alcotest.(check (list string))
        "checked - log after appending" [ "master.2"; "master.1" ]

let test_read_incr _ () =
  config () >>= L.Store.master >>= L.get_cursor ~path >>= fun cur ->
  L.read ~num_items:1 cur >>= fun (l, cur) ->
  Alcotest.(check (list string)) "checked - read one item" [ "master.2" ] l;
  L.read ~num_items:1 cur >>= fun (l, cur) ->
  Alcotest.(check (list string)) "checked - read one more item" [ "master.1" ] l;
  L.read ~num_items:1 cur >|= fun (l, _) ->
  Alcotest.(check (list string)) "checked - read one more item" [] l

let test_read_excess _ () =
  config () >>= L.Store.master >>= L.get_cursor ~path >>= fun cur ->
  L.read ~num_items:10 cur >|= fun (l, _) ->
  Alcotest.(check (list string))
    "checked - read 10 items" [ "master.2"; "master.1" ] l

let test_clone_merge _ () =
  config () >>= L.Store.master >>= fun t ->
  L.Store.clone ~src:t ~dst:"cl" >>= fun b ->
  L.append ~path b "clone.1" >>= fun () ->
  L.append ~path t "master.3" >>= fun () ->
  merge_into_exn b ~into:t >>= fun () ->
  L.read_all ~path t
  >|= Alcotest.(check (list string))
        "checked - log after appending"
        [ "master.3"; "clone.1"; "master.2"; "master.1" ]

let test_branch_merge _ () =
  config () >>= fun r ->
  L.Store.of_branch r "b1" >>= fun b1 ->
  L.Store.of_branch r "b2" >>= fun b2 ->
  L.Store.of_branch r "b3" >>= fun b3 ->
  L.Store.of_branch r "b4" >>= fun b4 ->
  L.append ~path b1 "b1.1" >>= fun () ->
  L.append ~path b2 "b2.1" >>= fun () ->
  L.append ~path b1 "b1.2" >>= fun () ->
  L.append ~path b1 "b1.3" >>= fun () ->
  L.append ~path b2 "b2.2" >>= fun () ->
  L.append ~path b1 "b1.4" >>= fun () ->
  merge_into_exn b1 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b3 >>= fun () ->
  merge_into_exn b2 ~into:b4 >>= fun () ->
  merge_into_exn b1 ~into:b4 >>= fun () ->
  L.read_all ~path b3
  >|= Alcotest.(check (list string))
        "checked - value of b3"
        [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]
  >>= fun () ->
  L.read_all ~path b4
  >|= Alcotest.(check (list string))
        "checked - value of b4"
        [ "b1.4"; "b2.2"; "b1.3"; "b1.2"; "b2.1"; "b1.1" ]

let test_cases =
  [
    ( "linked_log",
      [
        Alcotest_lwt.test_case "Read empty log" `Quick test_empty_read;
        Alcotest_lwt.test_case "Append and real all" `Quick test_append_read_all;
        Alcotest_lwt.test_case "Read incrementally with cursor" `Quick
          test_read_incr;
        Alcotest_lwt.test_case "Read excess with cursor" `Quick test_read_excess;
      ] );
    ( "linked_log store",
      [
        Alcotest_lwt.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest_lwt.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]
