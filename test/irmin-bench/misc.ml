(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

let snap_around_42 () =
  let f ~sd =
    Irmin_traces.Trace_stat_summary_utils.snap_to_integer ~significant_digits:sd
  in
  Alcotest.(check (float 0.)) "Already integer" (f ~sd:0 42.) 42.;
  Alcotest.(check (float 0.)) "Always snap" (f ~sd:0 42.1) 42.;
  Alcotest.(check (float 0.)) "Always snap" (f ~sd:0 42.4999999999) 42.;
  Alcotest.(check (float 0.)) "Always snap" (f ~sd:0 42.5000000001) 43.;

  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.1) 42.1;
  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.01) 42.01;
  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.00110000) 42.00110000;
  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.00100000) 42.00100000;
  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.00099999) 42.00099999;
  Alcotest.(check (float 0.)) "No snap" (f ~sd:4 42.00011000) 42.00011000;

  Alcotest.(check (float 0.)) "Snap" (f ~sd:4 42.00009999) 42.;
  Alcotest.(check (float 0.)) "Snap" (f ~sd:4 42.00001100) 42.;
  Alcotest.(check (float 0.)) "Snap" (f ~sd:4 42.00001000) 42.;
  Alcotest.(check (float 0.)) "Snap" (f ~sd:4 42.00000999) 42.;
  Alcotest.(check (float 0.)) "Already integer" (f ~sd:4 42.) 42.

let test_cases =
  [
    ( "snap_to_integer",
      [ Alcotest.test_case "snap_around_42" `Quick snap_around_42 ] );
  ]
