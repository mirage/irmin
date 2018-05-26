module Test_common:
  sig
    module type Test_S = sig
      include Irmin.S with type step = string
                   and type key = string list
                   and type contents = string
                   and type branch = string
      val author: Repo.t -> commit -> string option Lwt.t
    end

    type kind = [
      | `Core
      | `Git
      | `Http
      | `Unix
    ]

    val reporter: ?prefix:string -> unit -> Logs.reporter

    type t = {
      name  : string;
      kind  : kind;
      init  : unit -> unit Lwt.t;
      clean : unit -> unit Lwt.t;
      config: Irmin.config;
      store : (module Test_S);
      stats: (unit -> int * int) option;
    }

    val line: string -> unit
    val store: (module Irmin.S_MAKER) -> (module Irmin.Metadata.S) -> (module Test_S)
    val failf: ('a, Format.formatter, unit, 'b) format4 -> 'a
    val testable: 'a Irmin.Type.t -> 'a Alcotest.testable
    val check: 'a Irmin.Type.t -> string -> 'a -> 'a -> unit
    val checks: 'a Irmin.Type.t -> string -> 'a list -> 'a list -> unit

  end

module Test_link:
  sig

    module Hash = Irmin.Hash.SHA1

    module type S = sig
      include Irmin.LINK with type key = Hash.t and type value = Hash.t
      val v: unit -> t Lwt.t
    end

    val test: string -> (module S) -> string * [> `Quick ] * (unit -> unit)
  end

module Test_store:
  sig
    val run: string -> misc:unit Alcotest.test list -> (Alcotest.speed_level * Test_common.t) list -> unit
  end