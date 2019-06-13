val suite : Irmin_test.t

val suite_generic : Irmin_test.t

val test_db : string

module type S = sig
  include Irmin_test.S

  val init : unit -> unit Lwt.t
end

module type G = sig
  include S

  module Git : Irmin_git.G
end

val misc : (module G) -> unit Alcotest.test_case list

val mem : (module G)
