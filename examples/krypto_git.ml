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

(* ECB JUST FOR TEST ! DON'T USE IT *)
let git_store = Irmin_krypto.Make (Nocrypto.Cipher_block.AES.ECB) (Irimn_git.IO) (Git.FS.Make(FS))

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let main () =
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true () in
  Irmin.create store config task >>= fun t ->

  Irmin.update (t "t: Update 1.txt") ["root";"misc";"1.txt"] "Hello world!" >>= fun () ->
  Irmin.update (t "t: Update 2.txt") ["root";"misc";"2.txt"] "Hi!" >>= fun () ->
  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "How are you ?" >>= fun () ->

  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;

  Irmin.clone_force task (t "x: Cloning 't'") "test" >>= fun x ->

  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "Hohoho" >>= fun () ->
  Irmin.update (x "x: Update 2.txt") ["root";"misc";"2.txt"] "HELP!"  >>= fun () ->

  Irmin.merge_exn "t: Merge with 'x'" x ~into:t >>= fun () ->

  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"]  >>= fun file2 ->
  Irmin.read_exn (t "t: Read 3.txt") ["root";"misc";"3.txt"]  >>= fun file3 ->
  Printf.printf "I've just read: 2:%s 3:%s\n%!" file2 file3;

  return_unit

let () =
  Lwt_unix.run (main ())
