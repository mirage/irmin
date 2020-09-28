open Bechamel
open Toolkit
module T = Irmin.Type
module Hash = Irmin.Hash.BLAKE2B
open Output

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

  let size_of : t =
    let consume (type a) (ty : a T.t) =
      let f = T.(unstage (size_of ty)) in
      T.stage (f : a -> int option)
    in
    { name = "size_of"; operation = Consumer { consume } }
end

(** Generators for random data *)
module Faker = struct
  let () = Random.self_init ()

  let unit () = ()

  let bool () =
    match Random.int 2 with 0 -> false | 1 -> true | _ -> assert false

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

  type record = { foo : string; bar : int64; baz : bool * unit }
  [@@deriving irmin]

  type variant = Foo of string | Bar of int64 | Baz of bool * unit
  [@@deriving irmin]

  let record () =
    { foo = string 10 (); bar = int64 (); baz = (bool (), unit ()) }

  let variant () =
    match Random.int 3 with
    | 0 -> Foo (string 10 ())
    | 1 -> Bar (int64 ())
    | 2 -> Baz (bool (), unit ())
    | _ -> assert false

  type fixed_string = string

  let fixed_string_t = T.string_of (`Fixed 5)

  let fixed_string () = string 5 ()

  type 'a node = { left : 'a; v : fixed_string; right : 'a } [@@deriving irmin]

  type tree = Branch of tree node | Leaf of int [@@deriving irmin]

  let tree () =
    let rec inner depth =
      if depth > 12 then Leaf (long_int ())
      else
        let child = inner (depth + 1) in
        Branch { left = child; v = fixed_string (); right = child }
    in
    inner 0
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

    let unit = mk_data T.unit Faker.unit

    let bool = mk_data T.bool Faker.bool

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

    let record = mk_data Faker.record_t Faker.record

    let variant = mk_data Faker.variant_t Faker.variant

    let tree = mk_data Faker.tree_t Faker.tree
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
          Test.make ~name:"encoder" (distrib_arr (encode data.typ) data.random);
          Test.make ~name:"decoder"
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
      test ~name:"unit" Data.unit;
      test ~name:"bool" Data.bool;
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
      test ~name:"record" Data.record;
      test ~name:"variant" Data.variant;
      test ~name:"tree" Data.tree;
    ]

let suite () =
  Test.make_grouped ~name:""
    [
      test_operation ~name:"bin" Generic_op.bin;
      test_operation ~name:"bin_string" Generic_op.bin_string;
      test_operation ~name:"short_hash" Generic_op.short_hash;
      test_operation ~name:"pre_hash" Generic_op.pre_hash;
      test_operation ~name:"size_of" Generic_op.size_of;
    ]

let benchmark () =
  let suite = suite () in
  Fmt.epr "Running benchmarks\n%!";
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let raw_results =
    let quota = Mtime.Span.of_uint64_ns 5_000_000_000L (* 5s *) in
    Benchmark.all (Benchmark.cfg ~run:nb_runs ~quota ()) instances suite
  in
  List.map (fun instance -> Analyze.all ols instance raw_results) instances
  |> Analyze.merge ols instances

let ignore_eexist f = try f () with Unix.Unix_error (EEXIST, _, _) -> ()

let () =
  Random.self_init ();
  let output_formatter =
    match Sys.argv with
    | [| _ |] -> Fmt.stdout
    | [| _; "--output-dir"; dir |] ->
        let fname = Fmt.str "results-%lx.json" (Random.int32 Int32.max_int) in
        let latest, data_output =
          let data_file name =
            let open Fpath in
            v (Sys.getcwd ()) // v dir / name |> normalize |> to_string
          in
          (data_file "latest.json", data_file fname)
        in
        Fmt.epr "Results streaming to %s\n%!" data_output;
        ignore_eexist (fun () -> Unix.mkdir dir 0o777);
        if Unix.has_symlink () then
          ignore_eexist (fun () -> Unix.symlink data_output latest);
        open_out data_output |> Format.formatter_of_out_channel
    | a -> Fmt.failwith "Unexpected arguments: `%a'" Fmt.(Dump.array string) a
  in
  benchmark () |> Fmt.pf output_formatter "%a%!" pp_results
