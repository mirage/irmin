let root = "/tmp/irmin/test"

let init () =
  let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
  ()

(* Install the FS listener. *)
let () =
  Irmin_unix.install_dir_polling_listener 0.5
