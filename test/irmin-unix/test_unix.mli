module Git : sig
  val misc : unit Alcotest.test_case list

  val store : (module Test_git.G)

  val suite : Irmin_test.t
end

module Http : sig
  val servers : (Alcotest.speed_level * Irmin_test.t) list
end

module FS : sig
  val suite : Irmin_test.t
end
