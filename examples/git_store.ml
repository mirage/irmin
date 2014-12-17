(*

  Simple example showing how to create and use a Git store.

  $ make                               # Compile
  $ ./git_store                        # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)


open Lwt
open Irmin_unix

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

module Store = Irmin.Default(Irmin_git.FS)(Irmin.Contents.String)

let main () =
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true () in
  Store.create config task >>= fun t ->
  Store.update (t "t: Update 1.txt") ["root";"misc";"1.txt"] "Hello world!" >>= fun () ->
  Store.update (t "t: Update 2.txt") ["root";"misc";"2.txt"] "Hi!" >>= fun () ->
  Store.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "How are you ?" >>= fun () ->

  Store.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;

  Store.clone_force (t "x: Cloning 't'") task ["test"] >>= fun x ->

  Store.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "Hohoho" >>= fun () ->
  Store.update (x "x: Update 2.txt") ["root";"misc";"2.txt"] "HELP!"  >>= fun () ->

  Store.merge_exn (t "t: Merge with 'x'") ["test"] >>= fun () ->

  Store.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"]  >>= fun file2 ->
  Store.read_exn (t "t: Read 3.txt") ["root";"misc";"3.txt"]  >>= fun file3 ->
  Printf.printf "I've just read: 2:%s 3:%s\n%!" file2 file3;

  return_unit

let () =
  Lwt_unix.run (main ())
