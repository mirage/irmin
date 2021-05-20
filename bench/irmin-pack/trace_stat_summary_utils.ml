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

type histo = (float * int) list [@@deriving repr]
type curve = float list [@@deriving repr]

let snap_to_integer ~significant_digits v =
  if significant_digits < 0 then
    invalid_arg "significant_digits should be greater or equal to zero.";
  if not @@ Float.is_finite v then v
  else if Float.is_integer v then v
  else
    (* This scope is about choosing between [v] and [Float.round v]. *)
    let significant_digits = float_of_int significant_digits in
    let v' = Float.round v in
    if v' = 0. then (* Do not snap numbers close to 0. *)
      v
    else
      let round_distance = Float.abs (v -. v') in
      assert (round_distance <= 0.5);
      (* The smaller [round_distance], the greater [significant_digits']. *)
      let significant_digits' = -.Float.log10 round_distance in
      assert (significant_digits' > 0.);
      if significant_digits' >= significant_digits then v' else v

let pp_six_digits_with_spacer ppf v =
  let s = Printf.sprintf "%.6f" v in
  let len = String.length s in
  let a = String.sub s 0 (len - 3) in
  let b = String.sub s (len - 3) 3 in
  Format.fprintf ppf "%s_%s" a b

let create_pp_real ?(significant_digits = 7) examples =
  let examples = List.map (snap_to_integer ~significant_digits) examples in
  let all_integer =
    List.for_all
      (fun v -> Float.is_integer v || not (Float.is_finite v))
      examples
  in
  let absmax =
    List.fold_left
      (fun acc v ->
        if not @@ Float.is_finite acc then v
        else if not @@ Float.is_finite v then acc
        else Float.abs v |> max acc)
      Float.neg_infinity examples
  in
  let finite_pp =
    if absmax /. 1e12 >= 10. then fun ppf v ->
      Format.fprintf ppf "%.3f T" (v /. 1e12)
    else if absmax /. 1e9 >= 10. then fun ppf v ->
      Format.fprintf ppf "%.3f G" (v /. 1e9)
    else if absmax /. 1e6 >= 10. then fun ppf v ->
      Format.fprintf ppf "%.3f M" (v /. 1e6)
    else if absmax /. 1e3 >= 10. then fun ppf v ->
      Format.fprintf ppf "%#d" (Float.round v |> int_of_float)
    else if all_integer then fun ppf v ->
      Format.fprintf ppf "%#d" (Float.round v |> int_of_float)
    else if absmax /. 1. >= 10. then fun ppf v -> Format.fprintf ppf "%.3f" v
    else if absmax /. 1e-3 >= 10. then pp_six_digits_with_spacer
    else fun ppf v -> Format.fprintf ppf "%.3e" v
  in
  fun ppf v ->
    if Float.is_nan v then Format.fprintf ppf "n/a"
    else if Float.is_infinite v then Format.fprintf ppf "%f" v
    else finite_pp ppf v

let create_pp_seconds examples =
  let absmax =
    List.fold_left
      (fun acc v ->
        if not @@ Float.is_finite acc then v
        else if not @@ Float.is_finite v then acc
        else Float.abs v |> max acc)
      Float.neg_infinity examples
  in
  let finite_pp =
    if absmax >= 60. then fun ppf v -> Mtime.Span.pp_float_s ppf v
    else if absmax < 100. *. 1e-12 then fun ppf v ->
      Format.fprintf ppf "%.3e s" v
    else if absmax < 100. *. 1e-9 then fun ppf v ->
      Format.fprintf ppf "%.3f ns" (v *. 1e9)
    else if absmax < 100. *. 1e-6 then fun ppf v ->
      Format.fprintf ppf "%.3f \xc2\xb5s" (v *. 1e6)
    else if absmax < 100. *. 1e-3 then fun ppf v ->
      Format.fprintf ppf "%.3f ms" (v *. 1e3)
    else fun ppf v -> Format.fprintf ppf "%.3f  s" v
  in
  fun ppf v ->
    if Float.is_nan v then Format.fprintf ppf "n/a"
    else if Float.is_infinite v then Format.fprintf ppf "%f" v
    else finite_pp ppf v

let pp_percent ppf v =
  if not @@ Float.is_finite v then Format.fprintf ppf "%4f" v
  else if v = 0. then Format.fprintf ppf "  0%%"
  else if v < 10. /. 100. then Format.fprintf ppf "%3.1f%%" (v *. 100.)
  else if v < 1000. /. 100. then Format.fprintf ppf "%3.0f%%" (v *. 100.)
  else if v < 1000. then Format.fprintf ppf "%3.0fx" v
  else if v < 9.5e9 then (
    let long_repr = Printf.sprintf "%.0e" v in
    assert (String.length long_repr = 5);
    Format.fprintf ppf "%ce%cx" long_repr.[0] long_repr.[4])
  else Format.fprintf ppf "++++"

let weekly_stats = Tezos_history_metrics.weekly_stats

let approx_value_count_of_block_count value_of_row ?(first_block_idx = 0)
    wished_block_count =
  let end_block_idx = first_block_idx + wished_block_count in
  let blocks_of_row (_, _, _, v) = v in
  let fold (week_block0_idx, acc_value, acc_blocks) row =
    let week_blocks = blocks_of_row row in
    let week_value = value_of_row row in
    assert (acc_blocks <= wished_block_count);
    let nextweek_block0_idx = week_block0_idx + week_blocks in
    let kept_block_count =
      let left =
        if first_block_idx >= nextweek_block0_idx then `After
        else if first_block_idx <= week_block0_idx then `Before
        else `Inside
      in
      let right =
        if end_block_idx >= nextweek_block0_idx then `After
        else if end_block_idx <= week_block0_idx then `Before
        else `Inside
      in
      match (left, right) with
      | `After, `After -> 0
      | `Before, `Before -> 0
      | `Before, `After -> week_blocks
      | `Inside, `After -> first_block_idx - week_block0_idx
      | `Inside, `Inside -> end_block_idx - first_block_idx
      | `Before, `Inside -> wished_block_count - acc_blocks
      | `Inside, `Before -> assert false
      | `After, (`Before | `Inside) -> assert false
    in
    assert (kept_block_count >= 0);
    assert (kept_block_count <= week_blocks);
    let kept_tx_count =
      let f = float_of_int in
      f week_value /. f week_blocks *. f kept_block_count
      |> Float.round
      |> int_of_float
    in
    assert (kept_tx_count >= 0);
    assert (kept_tx_count <= week_value);
    let acc_blocks' = acc_blocks + kept_block_count in
    let acc_value' = acc_value + kept_tx_count in
    (nextweek_block0_idx, acc_value', acc_blocks')
  in
  let _, acc_value, acc_blocks = List.fold_left fold (0, 0, 0) weekly_stats in
  assert (acc_blocks <= wished_block_count);
  if acc_blocks = wished_block_count then acc_value
  else
    (* Extrapolate for the following weeks *)
    let latest_weeks_tx_count, latest_weeks_block_count =
      match List.rev weekly_stats with
      | rowa :: rowb :: rowc :: _ ->
          let value =
            List.map value_of_row [ rowa; rowb; rowc ] |> List.fold_left ( + ) 0
          in
          let blocks =
            List.map blocks_of_row [ rowa; rowb; rowc ]
            |> List.fold_left ( + ) 0
          in
          (value, blocks)
      | _ -> assert false
    in
    let missing_blocks = wished_block_count - acc_blocks in
    let missing_value =
      let f = float_of_int in
      f latest_weeks_tx_count /. f latest_weeks_block_count *. f missing_blocks
      |> Float.round
      |> int_of_float
    in
    acc_value + missing_value

let approx_transaction_count_of_block_count =
  approx_value_count_of_block_count (fun (_, txs, _, _) -> txs)

let approx_operation_count_of_block_count =
  approx_value_count_of_block_count (fun (_, _, ops, _) -> ops)

module Exponential_moving_average = struct
  type t = {
    momentum : float;
    relevance_threshold : float;
    opp_momentum : float;
    hidden_state : float;
    void_fraction : float;
  }

  let create ?(relevance_threshold = 1.) momentum =
    if momentum < 0. || momentum >= 1. then invalid_arg "Wrong momentum";
    if relevance_threshold < 0. || relevance_threshold > 1. then
      invalid_arg "Wrong relevance_threshold";
    {
      momentum;
      relevance_threshold;
      opp_momentum = 1. -. momentum;
      hidden_state = 0.;
      void_fraction = 1.;
    }

  let from_half_life ?relevance_threshold hl =
    if hl < 0. then invalid_arg "Wrong half life";
    create ?relevance_threshold (if hl = 0. then 0. else log 0.5 /. hl |> exp)

  let from_half_life_ratio ?relevance_threshold hl_ratio step_count =
    if hl_ratio < 0. then invalid_arg "Wrong half life ratio";
    if step_count < 0. then invalid_arg "Wront step count";
    step_count *. hl_ratio |> from_half_life ?relevance_threshold

  let momentum ema = ema.momentum
  let hidden_state ema = ema.hidden_state
  let void_fraction ema = ema.void_fraction
  let is_relevant ema = ema.void_fraction < ema.relevance_threshold

  let peek_exn ema =
    if is_relevant ema then ema.hidden_state /. (1. -. ema.void_fraction)
    else failwith "Can't peek an irrelevant EMA"

  let peek_or_nan ema =
    if is_relevant ema then ema.hidden_state /. (1. -. ema.void_fraction)
    else Float.nan

  let update ema sample =
    let hidden_state =
      (* The first term is the "forget" term, the second one is the "remember"
         term. *)
      (ema.momentum *. ema.hidden_state) +. (ema.opp_momentum *. sample)
    in
    let void_fraction =
      (* [update] decreases the quantity of "void". *)
      ema.momentum *. ema.void_fraction
    in
    { ema with hidden_state; void_fraction }

  let update_batch ema sample sample_size =
    if sample_size <= 0. then invalid_arg "Wrong sample_size";
    let momentum = ema.momentum ** sample_size in
    let opp_momentum = 1. -. momentum in
    (* From this point, the code is identical to [update]. *)
    let hidden_state =
      (ema.hidden_state *. momentum) +. (sample *. opp_momentum)
    in
    let void_fraction = ema.void_fraction *. momentum in
    { ema with hidden_state; void_fraction }

  (** [peek ema] is equal to [forget ema |> peek]. Modulo floating point
      imprecisions and relevance changes.

      Proof:

      {v
         v0 = hs0 / (1 - vf0)
         v1 = hs1 / (1 - vf1)
         hs1 = mom * hs0
         vf1 = mom * vf0 + (1 - mom)
         hs0 / (1 - vf0) = hs1         / (1 -  vf1)
         hs0 / (1 - vf0) = (mom * hs0) / (1 -  (mom * vf0 + (1 - mom)))
         hs0 / (1 - vf0) = (mom * hs0) / (1 -  (mom * vf0 +  1 - mom))
         hs0 / (1 - vf0) = (mom * hs0) / (1 + (-mom * vf0 -  1 + mom))
         hs0 / (1 - vf0) = (mom * hs0) / (1 -   mom * vf0 -  1 + mom)
         hs0 / (1 - vf0) = (mom * hs0) / (     -mom * vf0      + mom)
         hs0 / (1 - vf0) = (hs0)       / (     -1   * vf0      + 1)
         hs0 / (1 - vf0) = hs0 / (1 - vf0)
         v0 = v1
      v} *)
  let forget ema =
    let hidden_state = ema.momentum *. ema.hidden_state in
    let void_fraction =
      (* [forget] increases the quantity of "void".

          Where [update] does: [ema.m * ema.vf + ema.opp_m * 0],
                [forget] does: [ema.m * ema.vf + ema.opp_m * 1]. *)
      (ema.momentum *. ema.void_fraction) +. ema.opp_momentum
    in
    { ema with hidden_state; void_fraction }

  let forget_batch ema sample_size =
    if sample_size <= 0. then invalid_arg "Wrong sample_size";
    let momentum = ema.momentum ** sample_size in
    let opp_momentum = 1. -. momentum in
    (* From this point, the code is identical to [forget]. *)
    let hidden_state = ema.hidden_state *. momentum in
    let void_fraction = (ema.void_fraction *. momentum) +. opp_momentum in
    { ema with hidden_state; void_fraction }

  let map ?relevance_threshold momentum vec0 =
    List.fold_left
      (fun (ema, rev_result) v0 ->
        let ema = update ema v0 in
        let v1 = peek_or_nan ema in
        (ema, v1 :: rev_result))
      (create ?relevance_threshold momentum, [])
      vec0
    |> snd
    |> List.rev
end

module Resample = struct
  let should_sample ~i0 ~len0 ~i1 ~len1 =
    assert (len0 >= 2);
    assert (len1 >= 2);
    assert (i0 < len0);
    assert (i0 >= 0);
    assert (i1 >= 0);
    if i1 >= len1 then `Out_of_bounds
    else
      let i0 = float_of_int i0 in
      let len0 = float_of_int len0 in
      let i1 = float_of_int i1 in
      let len1 = float_of_int len1 in
      let progress0_left = (i0 -. 1.) /. (len0 -. 1.) in
      let progress0_right = i0 /. (len0 -. 1.) in
      let progress1 = i1 /. (len1 -. 1.) in
      if progress1 <= progress0_left then `Before
      else if progress1 <= progress0_right then (
        let where_in_interval =
          (progress1 -. progress0_left) /. (progress0_right -. progress0_left)
        in
        assert (where_in_interval > 0.);
        assert (where_in_interval <= 1.);
        `Inside where_in_interval)
      else `After

  type acc = {
    mode : [ `Interpolate | `Next_neighbor ];
    len0 : int;
    len1 : int;
    i0 : int;
    i1 : int;
    prev_v0 : float;
    rev_samples : curve;
  }

  let create_acc mode ~len0 ~len1 ~v00 =
    let mode = (mode :> [ `Interpolate | `Next_neighbor ]) in
    if len0 < 2 then invalid_arg "Can't resample curves below 2 points";
    if len1 < 2 then invalid_arg "Can't resample curves below 2 points";
    { mode; len0; len1; i0 = 1; i1 = 1; prev_v0 = v00; rev_samples = [ v00 ] }

  let accumulate ({ mode; len0; len1; i0; i1; prev_v0; rev_samples } as acc) v0
      =
    assert (i0 >= 1);
    assert (i1 >= 1);
    if i0 >= len0 then failwith "Accumulate called to much";
    if i1 >= len1 then failwith "Accumulate called to much";
    let rec aux i1 rev_samples =
      match should_sample ~len1 ~i0 ~len0 ~i1 with
      | `Inside where_inside ->
          if i1 = len1 - 1 then (
            assert (i0 = len0 - 1);
            assert (where_inside = 1.));
          let v1 =
            match mode with
            | `Next_neighbor -> v0
            | `Interpolate -> prev_v0 +. (where_inside *. (v0 -. prev_v0))
          in
          aux (i1 + 1) (v1 :: rev_samples)
      | `After -> (i1, rev_samples)
      | `Before -> assert false
      | `Out_of_bounds ->
          assert (i0 = len0 - 1);
          assert (i1 = len1);
          (i1, rev_samples)
    in
    let i1, rev_samples = aux i1 rev_samples in
    { acc with i0 = i0 + 1; i1; prev_v0 = v0; rev_samples }

  let finalise { len1; rev_samples; _ } =
    if List.length rev_samples <> len1 then failwith "Finalise called too soon";
    List.rev rev_samples

  let resample_vector mode vec0 len1 =
    let len0 = List.length vec0 in
    if len0 < 2 then invalid_arg "Can't resample curves below 2 points";
    let v00, vec0 =
      match vec0 with hd :: tl -> (hd, tl) | _ -> assert false
    in
    let acc = create_acc mode ~len0 ~len1 ~v00 in
    List.fold_left accumulate acc vec0 |> finalise
end

module Variable_summary = struct
  type t = {
    max_value : float * int;
    min_value : float * int;
    mean : float;
    diff : float;
    distribution : histo;
    evolution : curve;
  }
  [@@deriving repr]

  type acc = {
    (* Accumulators *)
    first_value : float;
    last_value : float;
    max_value : float * int;
    min_value : float * int;
    sum_value : float;
    value_count : int;
    distribution : Bentov.histogram;
    rev_evolution : curve;
    ma : Exponential_moving_average.t;
    next_in_idx : int;
    next_out_idx : int;
    (* Constants *)
    in_period_count : int;
    out_sample_count : int;
    evolution_resampling_mode :
      [ `Interpolate | `Prev_neighbor | `Next_neighbor ];
    scale : [ `Linear | `Log ];
  }

  let create_acc ~evolution_smoothing ~evolution_resampling_mode
      ~distribution_bin_count ~scale ~in_period_count ~out_sample_count =
    if in_period_count < 2 then
      invalid_arg "in_period_count should be greater than 1";
    if out_sample_count < 2 then
      invalid_arg "out_sample_count should be greater than 1";
    {
      first_value = Float.nan;
      last_value = Float.nan;
      max_value = (Float.nan, 0);
      min_value = (Float.nan, 0);
      sum_value = 0.;
      value_count = 0;
      distribution = Bentov.create distribution_bin_count;
      rev_evolution = [];
      ma =
        (match evolution_smoothing with
        | `None -> Exponential_moving_average.create 0.
        | `Ema (half_life_ratio, relevance_threshold) ->
            Exponential_moving_average.from_half_life_ratio ~relevance_threshold
              half_life_ratio
              (float_of_int in_period_count));
      next_in_idx = 0;
      next_out_idx = 0;
      in_period_count;
      out_sample_count;
      evolution_resampling_mode;
      scale;
    }

  let accumulate acc occurences_of_variable_in_period =
    let xs = occurences_of_variable_in_period in
    let xs = List.filter (fun v -> not (Float.is_nan v)) xs in
    let i = acc.next_in_idx in
    let sample_count = List.length xs |> float_of_int in
    assert (i < acc.in_period_count);

    let accumulate_in_sample
        (first, last, ((topv, _) as top), ((botv, _) as bot), histo, ma) v =
      let first = if Float.is_nan first then v else first in
      let last = if Float.is_nan v then last else v in
      let top = if Float.is_nan topv || topv < v then (v, i) else top in
      let bot = if Float.is_nan botv || botv > v then (v, i) else bot in
      let v =
        match acc.scale with
        | `Linear -> v
        | `Log ->
            if Float.is_infinite v then v
            else if v <= 0. then
              failwith
                "Input samples to a Variable_summary should be > 0. when \
                 scale=`Log."
            else Float.log v
      in
      let histo = Bentov.add v histo in
      let ma =
        Exponential_moving_average.update_batch ma v (1. /. sample_count)
      in
      (first, last, top, bot, histo, ma)
    in
    let first_value, last_value, max_value, min_value, distribution, ma =
      List.fold_left accumulate_in_sample
        ( acc.first_value,
          acc.last_value,
          acc.max_value,
          acc.min_value,
          acc.distribution,
          acc.ma )
        xs
    in
    let ma =
      if sample_count = 0. then Exponential_moving_average.forget ma else ma
    in
    let rev_evolution, next_out_idx =
      let rec aux rev_samples next_out_idx =
        match
          Resample.should_sample ~i0:i ~len0:acc.in_period_count
            ~i1:next_out_idx ~len1:acc.out_sample_count
        with
        | `Before -> assert false
        | `Inside where_in_block ->
            let out_sample =
              let v_after = Exponential_moving_average.peek_or_nan ma in
              if where_in_block = 1. then v_after
              else (
                assert (where_in_block > 0.);
                assert (next_out_idx > 0);
                let v_before = Exponential_moving_average.peek_or_nan acc.ma in
                match acc.evolution_resampling_mode with
                | `Prev_neighbor -> v_before
                | `Next_neighbor -> v_after
                | `Interpolate ->
                    v_before +. ((v_after -. v_before) *. where_in_block))
            in
            aux (out_sample :: rev_samples) (next_out_idx + 1)
        | `After | `Out_of_bounds -> (rev_samples, next_out_idx)
      in
      aux acc.rev_evolution acc.next_out_idx
    in

    {
      acc with
      first_value;
      last_value;
      max_value;
      min_value;
      sum_value = List.fold_left ( +. ) acc.sum_value xs;
      value_count = acc.value_count + List.length xs;
      distribution;
      rev_evolution;
      ma;
      next_in_idx = acc.next_in_idx + 1;
      next_out_idx;
    }

  let finalise acc =
    assert (acc.next_out_idx = acc.out_sample_count);
    assert (acc.next_out_idx = List.length acc.rev_evolution);
    assert (acc.next_in_idx = acc.in_period_count);
    let f = match acc.scale with `Linear -> Fun.id | `Log -> Float.exp in
    let distribution =
      let open Bentov in
      bins acc.distribution |> List.map (fun b -> (f b.center, b.count))
    in
    let evolution = List.rev_map f acc.rev_evolution in
    {
      max_value = acc.max_value;
      min_value = acc.min_value;
      mean = acc.sum_value /. float_of_int acc.value_count;
      diff = acc.last_value -. acc.first_value;
      distribution;
      evolution;
    }
end

module Parallel_folders = struct
  type ('row, 'acc, 'v) folder = {
    acc : 'acc;
    accumulate : 'acc -> 'row -> 'acc;
    finalise : 'acc -> 'v;
  }

  let folder acc accumulate finalise = { acc; accumulate; finalise }

  type ('res, 'row, 'v) folders =
    | F0 : ('res, 'row, 'res) folders
    | F1 :
        ('row, 'acc, 'v) folder * ('res, 'row, 'rest) folders
        -> ('res, 'row, 'v -> 'rest) folders

  type ('res, 'row, 'f, 'rest) open_t =
    ('res, 'row, 'rest) folders -> 'f * ('res, 'row, 'f) folders

  let open_ : 'f -> ('res, 'row, 'f, 'f) open_t =
   fun constructor folders -> (constructor, folders)

  let app :
      type res f v rest acc row.
      (res, row, f, v -> rest) open_t ->
      (row, acc, v) folder ->
      (res, row, f, rest) open_t =
   fun open_t folder folders -> open_t (F1 (folder, folders))

  let ( |+ ) = app

  type ('res, 'row) t = T : 'f * ('res, 'row, 'f) folders -> ('res, 'row) t

  let seal : type res row f. (res, row, f, res) open_t -> (res, row) t =
   fun open_t ->
    let constructor, folders = open_t F0 in
    T (constructor, folders)

  let accumulate : type res row. (res, row) t -> row -> (res, row) t =
   fun (T (constructor, folders)) row ->
    let rec aux : type v. (res, row, v) folders -> (res, row, v) folders =
      function
      | F0 -> F0
      | F1 (folder, t) as f -> (
          let acc = folder.acc in
          let acc' = folder.accumulate acc row in
          let t' = aux t in
          (* Avoid reallocating [F1] and [folder] when possible. *)
          match (acc == acc', t == t') with
          | true, true -> f
          | true, false -> F1 (folder, t')
          | false, (true | false) -> F1 ({ folder with acc = acc' }, t'))
    in
    let folders = aux folders in
    T (constructor, folders)

  let finalise : type res row. (res, row) t -> res =
    let rec aux : type c. (res, row, c) folders -> c -> res = function
      | F0 -> Fun.id
      | F1 (f, fs) ->
          fun constructor ->
            let v = f.finalise f.acc in
            let finalise_remaining = aux fs in
            let constructor = constructor v in
            finalise_remaining constructor
    in
    fun (T (constructor, folders)) -> aux folders constructor
end
