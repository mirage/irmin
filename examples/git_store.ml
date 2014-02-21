(*

  Simple example showing how to create and use a Git store.

  $ make                               # Compile
  $ ./git_store                        # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)


open Lwt

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  -> Log.set_log_level Log.DEBUG
  with Not_found -> ()


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
