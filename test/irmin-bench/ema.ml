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
module Ema = Irmin_traces.Trace_stat_summary_utils.Exponential_moving_average

(* Section 1/2 - Tools *)

let pp_ema ppf ema =
  let open Ema in
  Format.fprintf ppf "peek:%g; hs:%g; vf:%g" (peek_or_nan ema)
    (hidden_state ema) (void_fraction ema)

let feed ~n v ema =
  assert (n >= 0);
  let rec aux ema = function 0 -> ema | i -> aux (Ema.update ema v) (i - 1) in
  aux ema n

let feed_none ~n ema =
  assert (n >= 0);
  let rec aux ema = function 0 -> ema | i -> aux (Ema.forget ema) (i - 1) in
  aux ema n

(* Section 2/2 - Tests *)

let test_momentum () =
  let ema = Ema.create 0.5 in
  let ema = feed ~n:9999 1000. ema in
  Alcotest.(check (float 1e-100)) "Ema primed" (Ema.void_fraction ema) 0.;
  let ema = feed ~n:1 0. ema in
  Alcotest.(check (float 0.1)) "1st half life" (Ema.peek_exn ema) 500.;
  let ema = feed ~n:1 0. ema in
  Alcotest.(check (float 0.1)) "2nd half life" (Ema.peek_exn ema) 250.;
  let ema = feed ~n:1 0. ema in
  Alcotest.(check (float 0.1)) "3rd half life" (Ema.peek_exn ema) 125.

let test_momentum_zero () =
  let ema = Ema.create 0. in
  let ema = feed ~n:1 500. ema in
  Alcotest.(check (float 0.1)) "1st value" (Ema.peek_exn ema) 500.;
  let ema = feed ~n:1 250. ema in
  Alcotest.(check (float 0.1)) "2nd value" (Ema.peek_exn ema) 250.;
  let ema = feed ~n:1 125. ema in
  Alcotest.(check (float 0.1)) "3rd value" (Ema.peek_exn ema) 125.

let test_momentum_high () =
  let ema = Ema.create 0.9999999 in
  let ema = feed ~n:1 500. ema in
  Alcotest.(check (float 0.1)) "1st mean" (Ema.peek_exn ema) 500.;
  let ema = feed ~n:1 300. ema in
  Alcotest.(check (float 0.1))
    "2nd mean" (Ema.peek_exn ema)
    ((500. +. 300.) /. 2.);
  let ema = feed ~n:1 100. ema in
  Alcotest.(check (float 0.1))
    "3rd mean" (Ema.peek_exn ema)
    ((500. +. 300. +. 100.) /. 3.);

  let ema = feed ~n:10000 1000. ema in
  Alcotest.(check (float 10.)) "4th mean" (Ema.peek_exn ema) 1000.;
  let ema = feed ~n:10000 2000. ema in
  Alcotest.(check (float 10.)) "5th mean" (Ema.peek_exn ema) 1500.

let test_from_half_life () =
  let ema = Ema.from_half_life 10. in
  let ema = feed ~n:9999 1000. ema in
  Alcotest.(check (float 1e-100)) "Ema primed" (Ema.void_fraction ema) 0.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "1st half life" (Ema.peek_exn ema) 500.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "2nd half life" (Ema.peek_exn ema) 250.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "3rd half life" (Ema.peek_exn ema) 125.

let test_from_half_life_ratio () =
  let ema = Ema.from_half_life_ratio 0.1 100. in
  let ema = feed ~n:9999 1000. ema in
  Alcotest.(check (float 1e-100)) "Ema primed" (Ema.void_fraction ema) 0.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "1st half life" (Ema.peek_exn ema) 500.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "2nd half life" (Ema.peek_exn ema) 250.;
  let ema = feed ~n:10 0. ema in
  Alcotest.(check (float 0.1)) "3rd half life" (Ema.peek_exn ema) 125.

let test_update_batch () =
  let ema0 = Ema.create 0.5 |> feed ~n:10 42. in
  let ema1 = feed ~n:5 21. ema0 in
  let ema1' = Ema.update_batch ema0 21. 5. in
  Alcotest.(check (float 0.0001))
    "2way batch" (Ema.peek_exn ema1) (Ema.peek_exn ema1')

let test_relevance () =
  let ema = Ema.create ~relevance_threshold:0.51 0.5 in
  Alcotest.(check bool) "Ema empty" (Ema.is_relevant ema) false;
  let ema = feed ~n:1 42. ema in
  Alcotest.(check bool) "1U" (Ema.is_relevant ema) true;
  let ema = feed_none ~n:1 ema in
  Alcotest.(check bool) "1U, 1F" (Ema.is_relevant ema) false;
  let ema = feed ~n:1 42. ema in
  Alcotest.(check bool) "1U, 1F, 1U" (Ema.is_relevant ema) true;
  let ema = feed ~n:99 42. ema |> feed_none ~n:1 in
  Alcotest.(check bool) "1U, 1F, 100U, 1F" (Ema.is_relevant ema) true;
  let ema = feed_none ~n:1 ema in
  Alcotest.(check bool) "1U, 1F, 100U, 2F" (Ema.is_relevant ema) false

let test_relevance_low () =
  let ema = Ema.create ~relevance_threshold:0.01 0.5 in
  Alcotest.(check bool) "Ema empty" (Ema.is_relevant ema) false;
  let ema = feed ~n:5 42. ema in
  Alcotest.(check bool) "5U" (Ema.is_relevant ema) false;
  let ema = feed ~n:10 42. ema in
  Alcotest.(check bool) "10U" (Ema.is_relevant ema) true;
  let ema = feed_none ~n:1 ema in
  Alcotest.(check bool) "10U, 1F" (Ema.is_relevant ema) false

let test_relevance_one () =
  let ema = Ema.create ~relevance_threshold:1.0 0.5 in
  Alcotest.(check bool) "Ema empty" (Ema.is_relevant ema) false;
  let ema = feed ~n:1 42. ema in
  Alcotest.(check bool) "1U" (Ema.is_relevant ema) true;
  let ema = feed_none ~n:10 ema in
  Alcotest.(check bool) "1U, 10F" (Ema.is_relevant ema) true

let test_commutativity_shift () =
  let add a b = List.map2 ( +. ) a b in
  let mom = 0.1 in
  let xs = [ 10.0; -5.0; 6.0; -7.0; -20.0; 5.0; 9.0 ] in
  let ys = [ 7.0; 3.0; -6.0; -1.0; -3.0; 4.0; 7.0 ] in
  let res = Ema.map mom (add xs ys) in
  let res' = add (Ema.map mom xs) (Ema.map mom ys) in
  Alcotest.(check (list (float 0.0001))) "ema(a + b) = ema(a) + ema(b)" res res'

let test_commutativity_log_mul () =
  let mul a b = List.map2 ( *. ) a b in
  let log l = List.map Float.log l in
  let exp l = List.map Float.exp l in
  let mom = 0.1 in
  let xs = [ 10.0; 5.0; 6.0; 7.0; 20.0; 5.0; 9.0 ] in
  let ys = [ 7.0; 3.0; 6.0; 1.0; 3.0; 4.0; 7.0 ] in
  let res = mul xs ys |> log |> Ema.map mom |> exp in
  let res' =
    mul (xs |> log |> Ema.map mom |> exp) (ys |> log |> Ema.map mom |> exp)
  in
  Alcotest.(check (list (float 0.0001)))
    "exp(ema(log(a))) * exp(ema(log(b))) = exp(ema(log(a * b)))" res res'

let test_cases =
  [
    ( "ema",
      [
        Alcotest.test_case "momentum" `Quick test_momentum;
        Alcotest.test_case "momentum_zero" `Quick test_momentum_zero;
        Alcotest.test_case "momentum_high" `Quick test_momentum_high;
        Alcotest.test_case "from_half_life" `Quick test_from_half_life;
        Alcotest.test_case "from_half_life_ratio" `Quick
          test_from_half_life_ratio;
        Alcotest.test_case "batch" `Quick test_update_batch;
        Alcotest.test_case "relevance" `Quick test_relevance;
        Alcotest.test_case "relevance_low" `Quick test_relevance_low;
        Alcotest.test_case "relevance_one" `Quick test_relevance_one;
        Alcotest.test_case "commutativity shift" `Quick test_commutativity_shift;
        Alcotest.test_case "commutativity logmul" `Quick
          test_commutativity_log_mul;
      ] );
  ]
