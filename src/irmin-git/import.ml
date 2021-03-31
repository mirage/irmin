include Irmin.Export_for_backends

let src = Logs.Src.create "irmin.git" ~doc:"Irmin Git-format store"

module Log = (val Logs.src_log src : Logs.LOG)
