let root = "/tmp/irmin/test"

let init () =
  let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
  ()

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

(* Install the FS listener. *)
let () =
  Irmin_unix.install_dir_polling_listener 0.5
