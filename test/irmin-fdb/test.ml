let () =
  Irmin_test.Store.run "irmin-fdb" ~misc:[] [
    `Quick , Test_fdb.suite;
  ]
