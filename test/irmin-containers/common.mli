val merge_into_exn :
  (module Irmin.S with type t = 's) -> 's -> into:'s -> unit Lwt.t
