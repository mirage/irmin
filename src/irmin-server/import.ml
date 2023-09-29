let src = Logs.Src.create "irmin-server" ~doc:"irmin-server"

module Log = (val Logs.src_log src : Logs.LOG)
