let suite = [ ("tree", Test_tree.suite); ("hash", Test_hash.suite) ]

let () = Lwt_main.run (Alcotest_lwt.run "irmin" suite)
