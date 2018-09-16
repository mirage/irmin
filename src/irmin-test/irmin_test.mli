module type S =
  Irmin.S with type step = string
    and type key = string list
    and type contents = string
    and type branch = string

val reporter: ?prefix:string -> unit -> Logs.reporter

type t = {
  name  : string;
  init  : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config: Irmin.config;
  store : (module S);
  stats: (unit -> int * int) option;
}

val line: string -> unit
val store: (module Irmin.S_MAKER) -> (module Irmin.Metadata.S) -> (module S)
val testable: 'a Irmin.Type.t -> 'a Alcotest.testable
val check: 'a Irmin.Type.t -> string -> 'a -> 'a -> unit
val checks: 'a Irmin.Type.t -> string -> 'a list -> 'a list -> unit

module Link: sig

  module Hash = Irmin.Hash.SHA1

  module type S = sig
    include Irmin.LINK with type key = Hash.t and type value = Hash.t
    val v: unit -> t Lwt.t
  end

  val test: string -> (module S) -> string * [> `Quick ] * (unit -> unit)
end

module Store: sig

  val run:
    string -> misc:unit Alcotest.test list -> (Alcotest.speed_level * t) list ->
    unit

end
