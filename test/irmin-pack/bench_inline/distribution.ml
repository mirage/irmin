(** Size distribution generators for inline contents benchmarking *)

type t =
  | Uniform of { min : int; max : int }
  | Bimodal of { small : int; large : int; small_ratio : float }
  | Log_normal of { mu : float; sigma : float; min : int; max : int }
  | Zipfian of { min : int; max : int; exponent : float; cdf : float array }
  | Fixed of int list  (* Cycle through fixed sizes *)

let uniform ~min ~max = Uniform { min; max }
let bimodal ~small ~large ~small_ratio = Bimodal { small; large; small_ratio }
let log_normal ~mu ~sigma ~min ~max = Log_normal { mu; sigma; min; max }
let fixed sizes = Fixed sizes

(* Zipfian: P(k) ∝ 1/k^s where k is the rank (1-indexed) *)
let zipfian ~min ~max ~exponent =
  let n = max - min + 1 in
  (* Precompute CDF for efficient sampling *)
  let weights = Array.init n (fun i ->
    1.0 /. (float_of_int (i + 1) ** exponent)
  ) in
  let total = Array.fold_left ( +. ) 0.0 weights in
  let cdf = Array.make n 0.0 in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := !acc +. weights.(i) /. total;
    cdf.(i) <- !acc
  done;
  Zipfian { min; max; exponent; cdf }

(* Box-Muller transform for normal distribution *)
let random_normal () =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  let z = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
  z

(* Binary search for Zipfian sampling *)
let binary_search_cdf cdf u =
  let rec go lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if cdf.(mid) < u then go (mid + 1) hi
      else go lo mid
  in
  go 0 (Array.length cdf - 1)

let sample_one = function
  | Uniform { min; max } ->
      if min >= max then min else min + Random.int (max - min + 1)
  | Bimodal { small; large; small_ratio } ->
      if Random.float 1.0 < small_ratio then small else large
  | Log_normal { mu; sigma; min; max } ->
      let z = random_normal () in
      let x = exp (mu +. sigma *. z) in
      let size = int_of_float x in
      Int.max min (Int.min max size)
  | Zipfian { min; cdf; _ } ->
      let u = Random.float 1.0 in
      let rank = binary_search_cdf cdf u in
      min + rank
  | Fixed sizes ->
      let n = List.length sizes in
      List.nth sizes (Random.int n)

let sample ~n dist =
  Array.init n (fun _ -> sample_one dist)

(* Statistics *)
let mean arr =
  let sum = Array.fold_left ( + ) 0 arr in
  float_of_int sum /. float_of_int (Array.length arr)

let percentile arr p =
  let sorted = Array.copy arr in
  Array.sort Int.compare sorted;
  let idx = int_of_float (p *. float_of_int (Array.length sorted - 1)) in
  sorted.(idx)

let describe arr =
  let sorted = Array.copy arr in
  Array.sort Int.compare sorted;
  let n = Array.length sorted in
  {||}
  ^ Printf.sprintf "n=%d, min=%d, p50=%d, p90=%d, p99=%d, max=%d, mean=%.1f"
      n
      sorted.(0)
      sorted.(n / 2)
      sorted.(n * 9 / 10)
      sorted.(n * 99 / 100)
      sorted.(n - 1)
      (mean arr)

(* Predefined distributions for testing *)

(** All contents below inlining threshold *)
let all_small = uniform ~min:1 ~max:10

(** All contents above inlining threshold *)
let all_large = uniform ~min:50 ~max:200

(** Mix around the threshold (16 bytes) *)
let around_threshold = uniform ~min:5 ~max:30

(** 80% small, 20% large - typical metadata-heavy workload *)
let mostly_small = bimodal ~small:8 ~large:100 ~small_ratio:0.8

(** 20% small, 80% large - typical content-heavy workload *)
let mostly_large = bimodal ~small:8 ~large:100 ~small_ratio:0.2

(** Log-normal with median around 20 bytes *)
let log_normal_small = log_normal ~mu:3.0 ~sigma:1.0 ~min:1 ~max:1000

(** Log-normal with median around 150 bytes *)
let log_normal_medium = log_normal ~mu:5.0 ~sigma:1.0 ~min:1 ~max:10000

(** Zipfian with exponent 1.0 (classic Zipf's law), sizes 1-100 *)
let zipfian_small = zipfian ~min:1 ~max:100 ~exponent:1.0

(** Zipfian with exponent 1.0, sizes 1-1000 (wider range) *)
let zipfian_medium = zipfian ~min:1 ~max:1000 ~exponent:1.0

(** Zipfian with steeper exponent 1.5 (even more skewed toward small) *)
let zipfian_steep = zipfian ~min:1 ~max:100 ~exponent:1.5
