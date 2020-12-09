val run :
  string ->
  misc:unit Alcotest.test list ->
  (Alcotest.speed_level * Common.t) list ->
  unit
