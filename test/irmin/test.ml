let lift_suite_lwt =
  List.map (fun (a, b, t) ->
      Alcotest_lwt.test_case a b (fun _ () ->
          t ();
          Lwt.return_unit))

let suite =
  [ ("type", Test_type.suite |> lift_suite_lwt); ("tree", Test_tree.suite) ]

let () = Lwt_main.run (Alcotest_lwt.run "irmin" suite)
