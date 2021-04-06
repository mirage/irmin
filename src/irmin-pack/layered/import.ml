include Irmin.Export_for_backends
module Int63 = Optint.Int63

let src = Logs.Src.create "irmin.layers" ~doc:"irmin-pack layered backend"

module Log = (val Logs.src_log src : Logs.LOG)

type int63 = Int63.t

let ( -- ) = Int63.sub
