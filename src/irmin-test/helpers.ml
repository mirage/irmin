let init_logs () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Common.reporter ())
