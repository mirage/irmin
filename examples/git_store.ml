open Lwt

let store = "/tmp/irmin/test"
module Store = (val IrminGit.local ~bare:false store)

let main () =
  Store.create () >>= fun t ->
  Store.update   t ["root";"misc";"1.txt"] "Hello world!" >>= fun () ->
  Store.update   t ["root";"misc";"2.txt"] "Hi!" >>= fun () ->
  Store.update   t ["root";"misc";"3.txt"] "Really ?" >>= fun () ->
  Store.read_exn t ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;
  return_unit

let () =
  Lwt_unix.run (main ())
