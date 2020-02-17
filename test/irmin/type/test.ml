let suite = Test_optics.suite @ Test_optics_effectful.suite

let () = Alcotest.run "irmin.type" suite
