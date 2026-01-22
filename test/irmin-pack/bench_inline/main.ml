(** CLI for inline contents benchmark *)

let () = Random.init 42

(* All available distributions *)
let distributions_list = [
  ("uniform-small", Distribution.all_small);
  ("uniform-large", Distribution.all_large);
  ("around-threshold", Distribution.around_threshold);
  ("mostly-small", Distribution.mostly_small);
  ("mostly-large", Distribution.mostly_large);
  ("log-normal-small", Distribution.log_normal_small);
  ("log-normal-medium", Distribution.log_normal_medium);
  ("zipfian-small", Distribution.zipfian_small);
  ("zipfian-medium", Distribution.zipfian_medium);
  ("zipfian-steep", Distribution.zipfian_steep);
]

(* Distribution argument *)
let distribution =
  let doc = Printf.sprintf
    "Size distribution. One of: %s"
    (String.concat ", " (List.map fst distributions_list)) in
  Cmdliner.Arg.(
    value
    & opt (enum distributions_list) Distribution.around_threshold
    & info [ "distribution"; "d" ] ~docv:"DIST" ~doc)

let distribution_name =
  Cmdliner.Arg.(
    value
    & opt string "around-threshold"
    & info [ "dist-name" ] ~docv:"NAME" ~doc:"Distribution name for output")

let n_contents =
  Cmdliner.Arg.(
    value
    & opt int 10_000
    & info [ "n"; "contents" ] ~docv:"N" ~doc:"Number of contents to create")

let inline_contents =
  Cmdliner.Arg.(
    value
    & flag
    & info [ "inline" ] ~doc:"Enable inline contents")

let inline_threshold =
  Cmdliner.Arg.(
    value
    & opt int 48
    & info [ "threshold"; "t" ] ~docv:"BYTES"
        ~doc:"Inlining threshold in bytes (serialized size)")

let n_reads =
  Cmdliner.Arg.(
    value
    & opt int 1_000
    & info [ "reads" ] ~docv:"N" ~doc:"Number of read operations for latency measurement")

let runs =
  Cmdliner.Arg.(
    value
    & opt int 5
    & info [ "runs" ] ~docv:"N" ~doc:"Number of benchmark iterations")

let run_all_flag =
  Cmdliner.Arg.(
    value
    & flag
    & info [ "all" ] ~doc:"Run all distributions")

let csv_header =
  Cmdliner.Arg.(
    value
    & flag
    & info [ "header" ] ~doc:"Print CSV header")

let config distribution n_contents inline_contents inline_threshold n_reads runs =
  Bench.{ distribution; n_contents; inline_contents; inline_threshold; n_reads; runs }

(* Single distribution run *)
let run_single csv_header dist_name config =
  Logs.set_level None;
  if csv_header then Bench.print_csv_header ();
  Eio_main.run @@ fun env ->
  let result = Bench.run ~fs:env#fs ~config in
  Bench.print_csv_row ~dist_name ~config result

(* Run all distributions with both inline modes *)
let run_all n_contents inline_threshold n_reads runs =
  Logs.set_level None;
  let distributions = distributions_list in
  Bench.print_csv_header ();
  Eio_main.run @@ fun env ->
  List.iter (fun (dist_name, distribution) ->
    (* First run without inlining *)
    let config = Bench.{ distribution; n_contents; inline_contents = false;
                         inline_threshold; n_reads; runs } in
    let result = Bench.run ~fs:env#fs ~config in
    Bench.print_csv_row ~dist_name ~config result;
    (* Then run with inlining *)
    let config = Bench.{ config with inline_contents = true } in
    let result = Bench.run ~fs:env#fs ~config in
    Bench.print_csv_row ~dist_name ~config result
  ) distributions

(* Run threshold optimization: test multiple thresholds on selected distributions *)
let run_optimize n_contents n_reads runs =
  Logs.set_level None;
  let thresholds = [ 8; 16; 24; 32; 48; 64; 96; 128 ] in
  (* Use representative distributions *)
  let distributions = [
    ("zipfian-steep", Distribution.zipfian_steep);
    ("zipfian-small", Distribution.zipfian_small);
    ("log-normal-small", Distribution.log_normal_small);
    ("mostly-small", Distribution.mostly_small);
  ] in
  Bench.print_csv_header ();
  Eio_main.run @@ fun env ->
  List.iter (fun (dist_name, distribution) ->
    (* Baseline: no inlining *)
    let config = Bench.{ distribution; n_contents; inline_contents = false;
                         inline_threshold = 0; n_reads; runs } in
    let result = Bench.run ~fs:env#fs ~config in
    Bench.print_csv_row ~dist_name ~config result;
    (* Test each threshold *)
    List.iter (fun inline_threshold ->
      let config = Bench.{ distribution; n_contents; inline_contents = true;
                           inline_threshold; n_reads; runs } in
      let result = Bench.run ~fs:env#fs ~config in
      Bench.print_csv_row ~dist_name ~config result
    ) thresholds
  ) distributions

let cmd_run =
  let run csv_header dist_name distribution n_contents inline_contents inline_threshold n_reads runs all =
    if all then
      run_all n_contents inline_threshold n_reads runs
    else
      run_single csv_header dist_name (config distribution n_contents inline_contents inline_threshold n_reads runs)
  in
  let doc = "Run inline contents benchmark" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "run" ~doc)
    Cmdliner.Term.(const run $ csv_header $ distribution_name $ distribution
                   $ n_contents $ inline_contents $ inline_threshold
                   $ n_reads $ runs $ run_all_flag)

let cmd_optimize =
  let doc = "Find optimal inlining threshold by testing multiple values" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "optimize" ~doc)
    Cmdliner.Term.(const run_optimize $ n_contents $ n_reads $ runs)

(* Threshold sweep - test different content sizes around a given threshold *)
let sweep_threshold =
  Cmdliner.Arg.(
    value
    & opt int 16
    & info [ "threshold"; "t" ] ~docv:"BYTES" ~doc:"Inlining threshold to test around")

let sweep_range =
  Cmdliner.Arg.(
    value
    & opt int 10
    & info [ "range"; "r" ] ~docv:"BYTES" ~doc:"Range around threshold to sweep")

let run_sweep n_contents n_reads runs threshold range =
  Logs.set_level None;
  Printf.printf "# Threshold sweep: testing sizes %d to %d bytes\n"
    (threshold - range) (threshold + range);
  Printf.printf "size,inline,store_bytes,write_ms,read_p50_us,read_p99_us\n";
  Eio_main.run @@ fun env ->
  for size = threshold - range to threshold + range do
    let distribution = Distribution.fixed [size] in
    List.iter (fun inline_contents ->
      let config = Bench.{ distribution; n_contents; inline_contents;
                           inline_threshold = threshold; n_reads; runs } in
      let (store_size, write_time, p50, p99, _p999, _inlined, _non_inlined, _sizes) =
        Bench.run ~fs:env#fs ~config in
      Printf.printf "%d,%b,%d,%.2f,%.2f,%.2f\n"
        size inline_contents store_size write_time p50 p99
    ) [ false; true ]
  done

let cmd_sweep =
  let doc = "Sweep content sizes around inlining threshold" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "sweep" ~doc)
    Cmdliner.Term.(const run_sweep $ n_contents $ n_reads $ runs $ sweep_threshold $ sweep_range)

let cmds = [ cmd_run; cmd_optimize; cmd_sweep ]

let default_cmd =
  let doc = "Inline contents benchmark for irmin-pack" in
  Cmdliner.Cmd.info "bench_inline" ~doc

let () = Stdlib.exit @@ Cmdliner.Cmd.eval @@ Cmdliner.Cmd.group default_cmd cmds
