val run :
  string ->
  ?slow:bool ->
  misc:unit Alcotest.test list ->
  (Alcotest.speed_level * Common.t) list ->
  unit
