let suite = [ ("tree", Test_tree.suite); ("hash", Test_hash.suite) ]

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Lwt_main.run (Alcotest_lwt.run "irmin" suite)
