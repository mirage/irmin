let suite = [ ("tree", Test_tree.suite) ]

let () = Lwt_main.run (Alcotest_lwt.run "irmin" suite)
