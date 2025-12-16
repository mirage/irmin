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

let test_transaction_count () =
  let f =
    Irmin_traces.Trace_stat_summary_utils
    .approx_transaction_count_of_block_count
  in
  let b0, t0 = (1814, 497) in
  let b1, t1 = (10031, 10337) in
  let b2, t2 = (8478, 6444) in
  Alcotest.(check int) "1st week" (f b0) t0;
  Alcotest.(check int) "2 1st weeks" (f (b0 + b1)) (t0 + t1);
  Alcotest.(check int) "3 1st weeks" (f (b0 + b1 + b2)) (t0 + t1 + t2);
  Alcotest.(check int)
    "2 weeks & half of one"
    (f (b0 + b1 + (b2 / 2)))
    (t0 + t1 + (t2 / 2));
  Alcotest.(check int) "only 3rd week" (f ~first_block_idx:(b0 + b1) b2) t2;
  Alcotest.(check int)
    "a third of second week"
    (f ~first_block_idx:(b0 + (b1 / 3)) (b1 / 3))
    (t1 / 3);
  Alcotest.(check (float 0.5e6))
    "Tx count may 5th 2021"
    (f 1457727 |> float_of_int)
    15_000_000.

let test_operation_count () =
  let f =
    Irmin_traces.Trace_stat_summary_utils.approx_operation_count_of_block_count
  in
  Alcotest.(check (float 0.5e6))
    "Ops count may 5th 2021"
    (f 1457727 |> float_of_int)
    47_500_000.

let test_cases =
  [
    ("snap int", [ Alcotest.test_case "snap_around_42" `Quick snap_around_42 ]);
    ( "tx count",
      [
        Alcotest.test_case "test_transaction_count" `Quick
          test_transaction_count;
      ] );
    ( "ops count",
      [ Alcotest.test_case "test_operation_count" `Quick test_operation_count ]
    );
  ]
