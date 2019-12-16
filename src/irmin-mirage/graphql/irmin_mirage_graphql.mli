module Server : sig
  module type S = sig
    module Pclock : Mirage_clock.PCLOCK

    module Http : Cohttp_lwt.S.Server

    module Store : Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint

    val start : http:(Http.t -> unit Lwt.t) -> Store.repo -> unit Lwt.t
  end

  module Make
      (Http : Cohttp_lwt.S.Server)
      (Store : Irmin.S with type Private.Sync.endpoint = Git_mirage.endpoint)
      (Pclock : Mirage_clock.PCLOCK) :
    S
      with module Pclock = Pclock
       and module Store = Store
       and module Http = Http
end
