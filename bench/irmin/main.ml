open Bechamel
open Toolkit
module T = Irmin.Type
module Hash = Irmin.Hash.BLAKE2B

let ( >> ) f g x = g (f x)

module Generic_op = struct
  type op =
    | Consumer : { consume : 'a. 'a T.t -> ('a -> _) T.staged } -> op
    | Codec : {
        encode : 'a. 'a T.t -> ('a -> string) T.staged;
        decode : 'a. 'a T.t -> (string -> 'a) T.staged;
      }
        -> op

  type t = { name : string; operation : op }

  let bin_string : t =
    let encode (type a) (ty : a T.t) = T.to_bin_string ty in
    let decode (type a) (ty : a T.t) =
      let f = T.unstage (T.of_bin_string ty) in
      T.stage (f >> Result.get_ok : string -> a)
    in
    { name = "bin_string"; operation = Codec { encode; decode } }

  let bin : t =
    let encode (type a) (ty : a T.t) =
      let f = T.unstage (T.encode_bin ty) in
      T.stage
        (fun a ->
           let buffer = Buffer.create 0 in
           f a (Buffer.add_string buffer);
           Buffer.contents buffer
          : a -> string)
    in
    let decode (type a) (ty : a T.t) =
      let f = T.unstage (T.decode_bin ty) in
      T.stage (fun s -> f s 0 |> snd : string -> a)
    in
    { name = "bin"; operation = Codec { encode; decode } }

  let pre_hash : t =
    let consume (type a) (ty : a T.t) =
      let f = T.unstage (T.pre_hash ty) in
      T.stage
        (fun a ->
           let buffer = Buffer.create 0 in
           f a (Buffer.add_string buffer);
           Buffer.contents buffer
          : a -> string)
    in
    { name = "pre_hash"; operation = Consumer { consume } }

  let short_hash : t =
    let consume (type a) (ty : a T.t) =
      let f = T.short_hash ty in
      T.stage (f : a -> int)
    in
    { name = "short_hash"; operation = Consumer { consume } }
end

(** Generators for random data *)
module Faker = struct
  let () = Random.self_init ()

  let char_int () = Random.int (1 lsl 8)

  let short_int () = (1 lsl 8) + Random.int (1 lsl 8)

  let long_int () = (1 lsl 16) + Random.int (1 lsl 16)

  let int32 () = Random.int32 Int32.max_int

  let int64 () = Random.int64 Int64.max_int

  let char () = char_of_int (Random.int 256)

  let string n () = String.init n (fun _i -> char ())

  let bytes n () = Bytes.init n (fun _i -> char ())

  let pair a b () = (a (), b ())

  let triple a b c () = (a (), b (), c ())
end

let nb_runs = 30_000

module Data = struct
  type t =
    | E : {
        typ : 'a T.t;
        random : 'a array;
        encoded : string array option;
            (** For operations with a [decode] action *)
      }
        -> t

  (** Given a generic operation, generate arrays of {!nb_runs} elements needed
      to benchmark it on each type we care about. *)
  module Generate (Op : sig
    val t : Generic_op.op
  end) =
  struct
    let mk_data (type a) (typ : a T.t) faker =
      let random = Array.init nb_runs (fun _ -> faker ()) in
      let encoded =
        match Op.t with
        | Consumer _ -> None
        | Codec { encode; _ } ->
            let f = T.unstage (encode typ) in
            Some (random |> Array.map f)
      in
      E { typ; random; encoded }

    let hash =
      mk_data Hash.t (fun () -> Hash.hash (fun f -> f (Faker.string 1024 ())))

    let string_1024 = mk_data T.string Faker.(string 1024)

    let bytes_1024 = mk_data T.bytes Faker.(bytes 1024)

    let char_int = mk_data T.int Faker.char_int

    let short_int = mk_data T.int Faker.short_int

    let long_int = mk_data T.int Faker.long_int

    let int32 = mk_data T.int32 Faker.int32

    let int64 = mk_data T.int64 Faker.int64

    let pair_string_int =
      mk_data T.(pair string int) Faker.(pair (string 10) long_int)

    let triple_short_int =
      mk_data
        T.(triple int int int)
        Faker.(triple short_int short_int short_int)
  end
end

(** Stage a function with inputs chosen sequentially from an array of size
    {!nb_runs}. *)
let distrib_arr (type a b) (f : (a -> b) T.staged) (arr : a array) :
    (unit -> unit) Staged.t =
  let i = ref 0 in
  let f = T.unstage f in
  Staged.stage @@ fun () ->
  let (_ : b) = f arr.(!i mod nb_runs) in
  incr i

let test ~op ~name (E data : Data.t) =
  let tests =
    match op.Generic_op.operation with
    | Codec { encode; decode } ->
        [
          Test.make ~name:"enc" (distrib_arr (encode data.typ) data.random);
          Test.make ~name:"dec"
            (distrib_arr (decode data.typ) (data.encoded |> Option.get));
        ]
    | Consumer { consume } ->
        [ Test.make ~name:"" (distrib_arr (consume data.typ) data.random) ]
  in

  Test.make_grouped ~name tests

let test_operation ~name (op : Generic_op.t) =
  Fmt.epr "Generating data for `%s' codec ...%!" op.name;
  let module Data = Data.Generate (struct
    let t = op.operation
  end) in
  Fmt.epr " Done!\n%!";
  let test = test ~op in
  Test.make_grouped ~name
    [
      Test.make_grouped ~name:"int"
        [
          test ~name:"char (0 <= b <= 7)" Data.char_int;
          test ~name:"short (8 <= b <= 15)" Data.short_int;
          test ~name:"long (16 <= b <= 32)" Data.long_int;
        ];
      test ~name:"hash" Data.hash;
      test ~name:"string<1024>" Data.string_1024;
      test ~name:"bytes<1024>" Data.bytes_1024;
      test ~name:"int32" Data.int32;
      test ~name:"int64" Data.int64;
      test ~name:"pair_string_int" Data.pair_string_int;
      test ~name:"triple_short_int" Data.triple_short_int;
    ]

let suite =
  Test.make_grouped ~name:""
    [
      test_operation ~name:"bin" Generic_op.bin;
      test_operation ~name:"bin_string" Generic_op.bin_string;
      test_operation ~name:"short_hash" Generic_op.short_hash;
      test_operation ~name:"pre_hash" Generic_op.pre_hash;
    ]

type csv_line = { bench_name : string; metric : string; value : float }

let compare_csv_line a b =
  match String.compare a.bench_name b.bench_name with
  | 0 -> (
      match String.compare a.metric b.metric with
      | 0 -> Float.compare a.value b.value
      | o -> o)
  | o -> o

let unit_of_metric = function
  | "major-allocated" -> "words"
  | "minor-allocated" -> "words"
  | "monotonic-clock" -> "ns"
  | s -> Fmt.failwith "Unexpected unit: %s" s

let pp_results_csv ppf results =
  Fmt.string ppf "bench_name,metric,value\n";
  Hashtbl.fold
    (fun metric bench_values ->
      Hashtbl.fold
        (fun bench_name analysis ->
          let value, _ =
            let open Bechamel.Analyze.OLS in
            match (estimates analysis, predictors analysis) with
            | Some [ value ], [ "run" ] -> (value, ())
            | estimates, predictors ->
                Fmt.failwith
                  "Unexpected results: { estimates = %a; predictors = %a }"
                  Fmt.(Dump.option (Dump.list float))
                  estimates
                  Fmt.(Dump.list string)
                  predictors
          in
          List.cons { metric; bench_name; value })
        bench_values)
    results []
  |> List.sort compare_csv_line
  |> List.iter (fun { bench_name; metric; value } ->
         Fmt.pf ppf "%s,%s (%s),%f\n" bench_name metric (unit_of_metric metric)
           value)

let benchmark () =
  Fmt.epr "Running benchmarks\n%!";
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let raw_results =
    let quota = Mtime.Span.of_uint64_ns 1_000_000_000L (* 1s *) in
    Benchmark.all (Benchmark.cfg ~run:nb_runs ~quota ()) instances suite
  in
  List.map (fun instance -> Analyze.all ols instance raw_results) instances
  |> Analyze.merge ols instances

let () = benchmark () |> Fmt.pr "%a%!" pp_results_csv
