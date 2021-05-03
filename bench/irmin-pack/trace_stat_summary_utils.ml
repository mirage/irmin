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

(** List of tuples of week timestamp, week transaction count, week ops count and
    week block count.

    There is ~1 block per minute in tezos and 10080 minutes in a week. Hence the
    content of the last columns.

    Computed using:

    https://api.tzstats.com/series/block.json?collapse=1w&columns=n_tx,n_ops,count *)
let weekly_stats =
  [
    (1529884800000, 497, 16396, 1814);
    (1530489600000, 10337, 103366, 10031);
    (1531094400000, 6444, 24425, 8478);
    (1531699200000, 6268, 41311, 9674);
    (1532304000000, 4423, 126915, 9916);
    (1532908800000, 3951, 153091, 9561);
    (1533513600000, 3709, 151066, 9088);
    (1534118400000, 5103, 164351, 9442);
    (1534723200000, 7744, 174163, 9504);
    (1535328000000, 16856, 196819, 9627);
    (1535932800000, 9080, 195198, 9756);
    (1536537600000, 11296, 200613, 9759);
    (1537142400000, 34291, 234489, 9935);
    (1537747200000, 11029, 203505, 9657);
    (1538352000000, 15221, 211566, 9718);
    (1538956800000, 12279, 212965, 9857);
    (1539561600000, 16400, 216941, 9706);
    (1540166400000, 17277, 224464, 9875);
    (1540771200000, 13674, 219879, 9855);
    (1541376000000, 17780, 222633, 9781);
    (1541980800000, 43862, 243942, 9582);
    (1542585600000, 265754, 552326, 8583);
    (1543190400000, 29911, 199036, 8446);
    (1543795200000, 46262, 255443, 9395);
    (1544400000000, 46809, 265757, 9433);
    (1545004800000, 17226, 223122, 9601);
    (1545609600000, 14758, 224245, 9671);
    (1546214400000, 20025, 234288, 9775);
    (1546819200000, 15696, 227917, 9667);
    (1547424000000, 20349, 235370, 9743);
    (1548028800000, 17173, 232767, 9725);
    (1548633600000, 17551, 234505, 9714);
    (1549238400000, 22169, 241307, 9741);
    (1549843200000, 17892, 239578, 9787);
    (1550448000000, 17899, 239569, 9784);
    (1551052800000, 23444, 244467, 9749);
    (1551657600000, 17721, 236558, 9666);
    (1552262400000, 24563, 247532, 9743);
    (1552867200000, 22835, 250750, 9835);
    (1553472000000, 23011, 251950, 9845);
    (1554076800000, 29518, 257341, 9739);
    (1554681600000, 21775, 246456, 9683);
    (1555286400000, 30670, 257784, 9771);
    (1555891200000, 27023, 251536, 9687);
    (1556496000000, 22465, 248722, 9767);
    (1557100800000, 32423, 259129, 9773);
    (1557705600000, 27606, 258295, 9859);
    (1558310400000, 24614, 252204, 9771);
    (1558915200000, 32625, 248919, 9520);
    (1559520000000, 26519, 251351, 9736);
    (1560124800000, 32499, 258754, 9683);
    (1560729600000, 33250, 257565, 9578);
    (1561334400000, 29034, 252155, 9611);
    (1561939200000, 37005, 260170, 9597);
    (1562544000000, 31342, 255413, 9618);
    (1563148800000, 27970, 250786, 9563);
    (1563753600000, 43176, 273231, 9702);
    (1564358400000, 31888, 268034, 9877);
    (1564963200000, 38565, 272631, 9781);
    (1565568000000, 40317, 269809, 9669);
    (1566172800000, 32076, 266277, 9802);
    (1566777600000, 36307, 269958, 9789);
    (1567382400000, 35313, 263821, 9683);
    (1567987200000, 30223, 264633, 9799);
    (1568592000000, 38428, 276965, 9913);
    (1569196800000, 38314, 279131, 9914);
    (1569801600000, 42121, 282076, 9921);
    (1570406400000, 32572, 272638, 9900);
    (1571011200000, 33358, 261102, 9723);
    (1571616000000, 52981, 280392, 9812);
    (1572220800000, 40154, 270988, 9876);
    (1572825600000, 61329, 296468, 9889);
    (1573430400000, 40552, 262779, 9608);
    (1574035200000, 39421, 264409, 9742);
    (1574640000000, 55234, 289723, 9931);
    (1575244800000, 46487, 284110, 9976);
    (1575849600000, 64465, 307251, 9978);
    (1576454400000, 53005, 289408, 9920);
    (1577059200000, 42832, 281309, 9975);
    (1577664000000, 56314, 294350, 9961);
    (1578268800000, 51173, 289804, 9966);
    (1578873600000, 68059, 309998, 10023);
    (1579478400000, 64670, 308314, 10001);
    (1580083200000, 71682, 314239, 10034);
    (1580688000000, 77387, 320814, 9996);
    (1581292800000, 100282, 346279, 9975);
    (1581897600000, 86670, 329707, 9957);
    (1582502400000, 76753, 311822, 9901);
    (1583107200000, 83277, 304617, 9745);
    (1583712000000, 80841, 304818, 9802);
    (1584316800000, 94173, 322773, 9905);
    (1584921600000, 71931, 300080, 9921);
    (1585526400000, 69493, 299321, 9977);
    (1586131200000, 89672, 321104, 9984);
    (1586736000000, 74397, 308355, 10054);
    (1587340800000, 100712, 336139, 10055);
    (1587945600000, 93507, 327961, 10037);
    (1588550400000, 112609, 346175, 10039);
    (1589155200000, 91704, 324183, 10033);
    (1589760000000, 102430, 335567, 10058);
    (1590364800000, 94686, 324824, 10029);
    (1590969600000, 91343, 322663, 10029);
    (1591574400000, 125174, 357268, 10032);
    (1592179200000, 95985, 329653, 10023);
    (1592784000000, 122939, 357827, 10035);
    (1593388800000, 95589, 327834, 10029);
    (1593993600000, 128419, 362984, 10032);
    (1594598400000, 122955, 359831, 10044);
    (1595203200000, 121149, 354555, 10033);
    (1595808000000, 123127, 356107, 10026);
    (1596412800000, 139315, 372670, 10004);
    (1597017600000, 157274, 393230, 10022);
    (1597622400000, 135269, 366162, 9974);
    (1598227200000, 148236, 374878, 9964);
    (1598832000000, 127456, 352065, 9963);
    (1599436800000, 151080, 373997, 9952);
    (1600041600000, 126361, 350427, 9968);
    (1600646400000, 140182, 365980, 10025);
    (1601251200000, 138945, 362958, 9987);
    (1601856000000, 125262, 348689, 10029);
    (1602460800000, 163734, 386645, 10011);
    (1603065600000, 130914, 354378, 9999);
    (1603670400000, 165121, 389401, 10033);
    (1604275200000, 138447, 361921, 10028);
    (1604880000000, 189794, 404942, 9870);
    (1605484800000, 146999, 362987, 9841);
    (1606089600000, 164426, 389926, 9989);
    (1606694400000, 168238, 392715, 10022);
    (1607299200000, 140107, 361585, 10031);
    (1607904000000, 189296, 412488, 10009);
    (1608508800000, 152377, 376164, 10008);
    (1609113600000, 198728, 420519, 9846);
    (1609718400000, 185671, 406782, 9654);
    (1610323200000, 179296, 398205, 9664);
    (1610928000000, 218730, 439105, 9667);
    (1611532800000, 172690, 392509, 9691);
    (1612137600000, 187690, 410862, 9643);
    (1612742400000, 264982, 494725, 9773);
    (1613347200000, 232473, 465874, 9975);
    (1613952000000, 273033, 504512, 9991);
    (1614556800000, 240863, 470355, 9998);
    (1615161600000, 302082, 533510, 10011);
    (1615766400000, 258743, 487761, 10004);
    (1616371200000, 344647, 569310, 9989);
    (1616976000000, 445942, 674148, 10012);
    (1617580800000, 547832, 775660, 9947);
    (1618185600000, 656723, 887993, 10012);
    (1618790400000, 811462, 1036103, 9922);
    (1619395200000, 810847, 1040980, 9952);
    (1620000000000, 273545, 359345, 3779);
  ]

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
    distribution : histo;
    evolution : curve;
  }
  [@@deriving repr]

  type acc = {
    (* Accumulators *)
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

    let accumulate_in_sample (((topv, _) as top), ((botv, _) as bot), histo, ma)
        v =
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
      (top, bot, histo, ma)
    in
    let max_value, min_value, distribution, ma =
      List.fold_left accumulate_in_sample
        (acc.max_value, acc.min_value, acc.distribution, acc.ma)
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
