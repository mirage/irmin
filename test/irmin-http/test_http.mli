type test = Alcotest.speed_level * Irmin_test.t

val servers : test list

val suites : test list -> test list

val with_server : test list -> (unit -> unit) -> unit
