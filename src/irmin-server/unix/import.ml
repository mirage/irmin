let src = Logs.Src.create "irmin-server.unix" ~doc:"irmin-server.unix"

module Log = (val Logs.src_log src : Logs.LOG)
