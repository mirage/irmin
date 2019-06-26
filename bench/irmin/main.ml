open Bechamel
open Toolkit
module T = Irmin.Type
module H = Irmin.Hash.SHA1

let () = Random.self_init ()

let random_char_int () = Random.int (1 lsl 8)

let random_short_int () = (1 lsl 8) + Random.int (1 lsl 8)

let random_long_int () = (1 lsl 16) + Random.int (1 lsl 16)

let random_int32 () = Random.int32 Int32.max_int

let random_int64 () = Random.int64 Int64.max_int

let random_char () = char_of_int (Random.int 256)

let random_string n = String.init n (fun _i -> random_char ())

let nb_runs = 30_000

let random_hashes =
  Array.init nb_runs (fun _ ->
      let r = random_string 1024 in
      H.hash r )

let random_hashes_encoded = Array.map (T.to_bin_string H.t) random_hashes

let random_char_ints = Array.init nb_runs (fun _ -> random_char_int ())

let random_char_ints_encoded =
  Array.map (T.to_bin_string T.int) random_char_ints

let random_short_ints = Array.init nb_runs (fun _ -> random_short_int ())

let random_short_ints_encoded =
  Array.map (T.to_bin_string T.int) random_short_ints

let random_long_ints = Array.init nb_runs (fun _ -> random_long_int ())

let random_long_ints_encoded =
  Array.map (T.to_bin_string T.int) random_long_ints

let random_ints32 = Array.init nb_runs (fun _ -> random_int32 ())

let random_ints32_encoded = Array.map (T.to_bin_string T.int32) random_ints32

let random_ints64 = Array.init nb_runs (fun _ -> random_int64 ())

let random_ints64_encoded = Array.map (T.to_bin_string T.int64) random_ints64

let encode arr ty =
  let i = ref 0 in
  Staged.stage (fun () ->
      let _ = T.to_bin_string ty arr.(!i mod nb_runs) in
      incr i )

let decode arr ty =
  let i = ref 0 in
  Staged.stage (fun () ->
      let _ = T.of_bin_string ty arr.(!i mod nb_runs) in
      incr i )

let int_tests =
  let encoding =
    Test.make_grouped ~name:"enc"
      [ Test.make ~name:"char (0 <= b <= 7)" (encode random_char_ints T.int);
        Test.make ~name:"short (8 <= b <= 15)" (encode random_short_ints T.int);
        Test.make ~name:"long (16 <= b <= 32)" (encode random_long_ints T.int)
      ]
  in
  let decoding =
    Test.make_grouped ~name:"dec"
      [ Test.make ~name:"char (0 <= b <= 7)"
          (decode random_char_ints_encoded T.int);
        Test.make ~name:"short (8 <= b <= 15)"
          (decode random_short_ints_encoded T.int);
        Test.make ~name:"long (16 <= b <= 32)"
          (decode random_long_ints_encoded T.int)
      ]
  in
  Test.make_grouped ~name:"int" [ encoding; decoding ]

let int32_tests =
  Test.make_grouped ~name:"int32"
    [ Test.make ~name:"enc" (encode random_ints32 T.int32);
      Test.make ~name:"dec" (decode random_ints32_encoded T.int32)
    ]

let int64_tests =
  Test.make_grouped ~name:"int64"
    [ Test.make ~name:"enc" (encode random_ints64 T.int64);
      Test.make ~name:"dec" (decode random_ints64_encoded T.int64)
    ]

let hash_tests =
  Test.make_grouped ~name:"hash"
    [ Test.make ~name:"enc" (encode random_hashes H.t);
      Test.make ~name:"dec" (decode random_hashes_encoded H.t)
    ]

let test =
  Test.make_grouped ~name:"" [ int_tests; hash_tests; int32_tests; int64_tests ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let raw_results =
    Benchmark.all ~run:nb_runs ~quota:Benchmark.(s 1.) instances test
  in
  List.map (fun instance -> Analyze.all ols instance raw_results) instances
  |> Analyze.merge ols instances

let () = Bechamel_notty.Unit.add Instance.monotonic_clock "ns"

let () = Bechamel_notty.Unit.add Instance.minor_allocated "w"

let () = Bechamel_notty.Unit.add Instance.major_allocated "mw"

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

type rect = Bechamel_notty.rect = { w : int; h : int }

let rect w h = { w; h }

let () =
  let window =
    match Notty_unix.winsize Unix.stdout with
    | Some (_, _) -> { w = 80; h = 1 }
    | None -> { w = 80; h = 1 }
  in
  img (window, benchmark ()) |> Notty_unix.eol |> Notty_unix.output_image
