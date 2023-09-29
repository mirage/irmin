let src = Logs.Src.create "irmin-client" ~doc:"irmin-client"

module Log = (val Logs.src_log src : Logs.LOG)
