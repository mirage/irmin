(** Reporter for the benchmarking format described in
    {:https://github.com/gs0510/index-benchmarks#benchmarks-format}. *)

let ( >> ) f g x = g (f x)

let unit_of_metric_name = function
  | "major-allocated" -> Some "words"
  | "minor-allocated" -> Some "words"
  | "monotonic-clock" -> Some "ns"
  | _ -> None

type metric = { metric_name : string; unit : string option }

let metric_to_string { metric_name; unit } =
  metric_name ^ Option.fold ~none:"" ~some:(Fmt.str " (%s)") unit

type measurements = (metric * float) list

let measurements_to_yojson ms =
  `Assoc (ms |> List.map (fun (m, v) -> (metric_to_string m, `Float v)))

type output = { results : bench_result list }

and bench_result = {
  bench_name : string; [@key "name"]
  measurements : measurements; [@key "metrics"]
}
[@@deriving to_yojson]

(** Lift a binary function to operate over a larger type using an inner
    projection. *)
let under2 : type a b c. (a -> b) -> (b -> b -> c) -> a -> a -> c =
 fun f g a b -> g (f a) (f b)

let sort_results =
  let sort_measurements =
    List.sort
      (under2 (fun ({ metric_name; _ }, _) -> metric_name) String.compare)
  in
  List.map (fun { bench_name; measurements } ->
      { bench_name; measurements = sort_measurements measurements })
  >> List.sort (under2 (fun { bench_name; _ } -> bench_name) String.compare)

let replace2 : type k v. (k, (k, v) Hashtbl.t) Hashtbl.t -> k -> k -> v -> unit
    =
 fun h k1 k2 v ->
  match Hashtbl.find_opt h k1 with
  | Some h_inner -> Hashtbl.replace h_inner k2 v
  | None ->
      let h_inner = Hashtbl.create 0 in
      Hashtbl.replace h_inner k2 v;
      Hashtbl.replace h k1 h_inner

let hashtbl_transpose : type a b. ((a, (a, b) Hashtbl.t) Hashtbl.t as 'h) -> 'h
    =
 fun h ->
  let new_h = Hashtbl.create 0 in
  Hashtbl.iter
    (fun a_outer ->
      Hashtbl.iter (fun a_inner b -> replace2 new_h a_inner a_outer b))
    h;
  new_h

let add_measurement :
    string -> Bechamel.Analyze.OLS.t -> ((metric * float) list as 'acc) -> 'acc
    =
 fun metric_name analysis ->
  let metric = { metric_name; unit = unit_of_metric_name metric_name } in
  let value =
    let open Bechamel.Analyze.OLS in
    match (estimates analysis, predictors analysis) with
    | Some [ value ], [ "run" ] -> value
    | estimates, predictors ->
        Fmt.failwith "Unexpected results: { estimates = %a; predictors = %a }"
          Fmt.(Dump.option (Dump.list float))
          estimates
          Fmt.(Dump.list string)
          predictors
  in
  List.cons (metric, value)

let pp_results =
  Fmt.using
    ((* Bechamel reports results indexed by [metric] then by [bench_name], but
        the output format indexes in the reverse order. *)
     hashtbl_transpose
    >> (fun h ->
         Hashtbl.fold
           (fun bench_name measurements ->
             let measurements = Hashtbl.fold add_measurement measurements [] in
             List.cons { bench_name; measurements })
           h [])
    >> sort_results
    >> (fun results -> { results })
    >> output_to_yojson)
    Yojson.Safe.pretty_print
