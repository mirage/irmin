open Bechamel
open Toolkit
module T = Irmin.Type
module H = Irmin.Hash.SHA1

module Generic_codec = struct
  type t =
    | G : {
        name : string;
        encode : 'a. 'a T.t -> [ `Staged of 'a -> string ];
        decode : 'a. 'a T.t -> [ `Staged of string -> 'a ];
      }
        -> t

  let bin_string : t =
    let encode ty = `Staged (T.to_bin_string ty) in
    let decode ty =
      let f = T.of_bin_string ty in
      `Staged (fun s -> f s |> Result.get_ok)
    in
    G { name = "bin_string"; encode; decode }

  let bin : t =
    let encode ty =
      let f = T.encode_bin ty ?headers:None in
      `Staged
        (fun s ->
          let buffer = Buffer.create 0 in
          f s (Buffer.add_string buffer);
          Buffer.contents buffer)
    in
    let decode ty =
      let f = T.decode_bin ty ?headers:None in
      `Staged (fun s -> f s 0 |> snd)
    in
    G { name = "bin"; encode; decode }
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

(** Arrays of structured random data for use in the benchmarks below *)
let nb_runs = 30_000

module Data = struct
  type t =
    | E : { typ : 'a T.t; unencoded : 'a array; encoded : string array } -> t

  module Generate (Codec : sig
    val t : Generic_codec.t
  end) =
  struct
    let mk_data (type a) (typ : a T.t) faker =
      let (G { encode; _ }) = Codec.t in
      let unencoded = Array.init nb_runs (fun _ -> faker ()) in
      let encoded =
        let (`Staged f) = encode typ in
        unencoded |> Array.map f
      in
      E { typ; unencoded; encoded }

    let hash =
      mk_data H.t (fun () -> H.hash (fun f -> f (Faker.string 1024 ())))

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

let run_encode (Generic_codec.G { encode; _ }) typ arr =
  let i = ref 0 in
  let (`Staged f) = encode typ in
  Staged.stage @@ fun () ->
  let _ = f arr.(!i mod nb_runs) in
  incr i

let run_decode (Generic_codec.G { decode; _ }) typ arr =
  let i = ref 0 in
  let (`Staged f) = decode typ in
  Staged.stage @@ fun () ->
  let _ = f arr.(!i mod nb_runs) in
  incr i

(** Benchmark an encoder/decoder pair *)
let test_codec ~generic_codec:f ~name (E data : Data.t) =
  Test.make_grouped ~name
    [
      Test.make ~name:"enc" (run_encode f data.typ data.unencoded);
      Test.make ~name:"dec" (run_decode f data.typ data.encoded);
    ]

let test_generic_codec ~name
    (Generic_codec.G { name = fname; _ } as generic_codec) =
  Fmt.epr "Generating data for `%s' codec ...%!" fname;
  let module Data = Data.Generate (struct
    let t = generic_codec
  end) in
  Fmt.epr " Done!\n%!";
  let test_codec = test_codec ~generic_codec in
  Test.make_grouped ~name
    [
      Test.make_grouped ~name:"int"
        [
          test_codec ~name:"char (0 <= b <= 7)" Data.char_int;
          test_codec ~name:"short (8 <= b <= 15)" Data.short_int;
          test_codec ~name:"long (16 <= b <= 32)" Data.long_int;
        ];
      test_codec ~name:"hash" Data.hash;
      test_codec ~name:"string<1024>" Data.string_1024;
      test_codec ~name:"bytes<1024>" Data.bytes_1024;
      test_codec ~name:"int32" Data.int32;
      test_codec ~name:"int64" Data.int64;
      test_codec ~name:"pair_string_int" Data.pair_string_int;
      test_codec ~name:"triple_short_int" Data.triple_short_int;
    ]

let suite =
  Test.make_grouped ~name:""
    [
      test_generic_codec ~name:"bin" Generic_codec.bin;
      test_generic_codec ~name:"str" Generic_codec.bin_string;
    ]

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

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

type rect = Bechamel_notty.rect = { w : int; h : int }

let () =
  Bechamel_notty.Unit.add Instance.monotonic_clock "ns";
  Bechamel_notty.Unit.add Instance.minor_allocated "w";
  Bechamel_notty.Unit.add Instance.major_allocated "mw";
  let window =
    match Notty_unix.winsize Unix.stdout with
    | Some (w, h) -> { w; h }
    | None -> { w = 80; h = 1 }
  in
  img (window, benchmark ()) |> Notty_unix.eol |> Notty_unix.output_image
