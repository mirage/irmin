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
module Int63 = Optint.Int63
module Ranges = Irmin_pack_unix.Ranges

let test () =
  let input =
    [ (90, 10); (80, 5); (70, 10); (87, 1); (60, 5); (50, 5); (65, 2); (55, 5) ]
  in
  let ranges = Ranges.make () in
  List.iter
    (fun (off, len) -> Ranges.add ~off:(Int63.of_int off) ~len ranges)
    input;
  let output = ref [] in
  Ranges.iter
    (fun ~off ~len -> output := (Int63.to_int off, Int63.to_int len) :: !output)
    ranges;
  let expected = [ (90, 10); (87, 1); (70, 15); (50, 17) ] in
  Alcotest.(check (list (pair int int))) "out of order" expected !output;
  Lwt.return_unit

let tests =
  [ Alcotest_lwt.test_case "test ranges" `Quick (fun _switch () -> test ()) ]
