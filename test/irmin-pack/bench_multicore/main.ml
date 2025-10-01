let () = Random.init 0

let range =
  let parse str =
    try Scanf.sscanf str "%i-%i" (fun x y -> `Ok (x, y))
    with End_of_file -> (
      try Scanf.sscanf str "%i" (fun x -> `Ok (x, x))
      with End_of_file -> `Error ("not a range: " ^ str))
  in
  let print h (x, y) =
    if x = y then Format.fprintf h "%i" x else Format.fprintf h "%i-%i" x y
  in
  (parse, print)

let elements =
  Cmdliner.Arg.(
    value
    & opt int 500_000
    & info [ "elements" ] ~docv:"ELEMENTS" ~doc:"Number of leaves in the tree")

let nb_tasks =
  Cmdliner.Arg.(
    value
    & opt int 10_000
    & info [ "tasks" ] ~docv:"TASKS" ~doc:"Number of tasks")

let nb_finds =
  Cmdliner.Arg.(
    value
    & opt int 33
    & info [ "finds" ] ~docv:"FINDS" ~doc:"Number of Tree.find queries per task")

let nb_adds =
  Cmdliner.Arg.(
    value
    & opt int 33
    & info [ "adds" ] ~docv:"ADDS" ~doc:"Number of Tree.add operations per task")

let nb_rems =
  Cmdliner.Arg.(
    value
    & opt int 33
    & info [ "rems" ] ~docv:"REMS"
        ~doc:"Number of Tree.remove operations per task")

let nb_runs =
  Cmdliner.Arg.(
    value
    & opt int 10
    & info [ "runs" ] ~docv:"RUNS" ~doc:"Repeat benchmark N times")

let branching =
  Cmdliner.Arg.(
    value
    & opt range (1, 10_000)
    & info [ "branch" ] ~docv:"BRANCH" ~doc:"Node branching factor")

let balance =
  Cmdliner.Arg.(
    value
    & opt range (1, 10_000)
    & info [ "balance" ] ~docv:"BALANCE" ~doc:"Node branching balance")

let name_length =
  Cmdliner.Arg.(
    value
    & opt range (10, 256)
    & info [ "name-length" ] ~docv:"NAME_LENGTH" ~doc:"Name length")

let contents_length =
  Cmdliner.Arg.(
    value
    & opt range (10, 1000)
    & info [ "contents-length" ] ~docv:"CONTENTS_LENGTH" ~doc:"Contents length")

let max_depth =
  Cmdliner.Arg.(
    value
    & opt int 200
    & info [ "height" ] ~docv:"HEIGHT" ~doc:"Tree max height")

let warm =
  Cmdliner.Arg.(
    value
    & flag
    & info [ "warm" ] ~docv:"WARM" ~doc:"Warm up the tree in memory")

let config warm elements max_depth branching balance contents_length name_length
    nb_tasks nb_finds nb_adds nb_rems nb_runs =
  {
    Gen.warm;
    elements;
    max_depth;
    branching;
    balance;
    contents_length;
    name_length;
    nb_tasks;
    nb_finds;
    nb_adds;
    nb_rems;
    nb_runs;
  }

let config =
  Cmdliner.Term.(
    const config
    $ warm
    $ elements
    $ max_depth
    $ branching
    $ balance
    $ contents_length
    $ name_length
    $ nb_tasks
    $ nb_finds
    $ nb_adds
    $ nb_rems
    $ nb_runs)

let bench_half config =
  Logs.set_level None;
  Eio_main.run @@ fun env -> Bench.half ~fs:env#fs ~d_mgr:env#domain_mgr ~config

let bench_full config =
  Logs.set_level None;
  Eio_main.run @@ fun env -> Bench.full ~fs:env#fs ~d_mgr:env#domain_mgr ~config

let cmd_half =
  let doc = "Half-diamond benchmark" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "half" ~doc)
    Cmdliner.Term.(const bench_half $ config)

let cmd_full =
  let doc = "Full-diamond benchmark" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "full" ~doc)
    Cmdliner.Term.(const bench_full $ config)

let cmds = [ cmd_half; cmd_full ]

let default_cmd =
  let doc = "Irmin multicore benchmarks" in
  Cmdliner.Cmd.info "help" ~doc

let () = Stdlib.exit @@ Cmdliner.Cmd.eval @@ Cmdliner.Cmd.group default_cmd cmds
