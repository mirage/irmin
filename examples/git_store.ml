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
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let path = "/tmp/irmin/test"
module Git = IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
module Store = (val Git.create ~bare:true ~kind:`Disk ~root:path ())

let main () =
  Store.create () >>= fun t ->
  Store.update   t ["root";"misc";"1.txt"] "Hello world!" >>= fun () ->
  Store.update   t ["root";"misc";"2.txt"] "Hi!" >>= fun () ->
  Store.update   t ["root";"misc";"3.txt"] "How are you ?" >>= fun () ->

  Store.read_exn t ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;

  Store.branch t "refs/heads/test" >>= fun x ->

  let str = Cryptokit.(Random.string (Random.device_rng "/dev/urandom") 1024) in
  Store.update   t ["root";"misc";"3.txt"] "Hohoho" >>= fun () ->
  Store.update   x ["root";"misc";"2.txt"] str >>= fun () ->

  Store.merge_exn t x >>= fun () ->

  Store.read_exn t ["root";"misc";"2.txt"]  >>= fun file2 ->
  Store.read_exn t ["root";"misc";"3.txt"]  >>= fun file3 ->
  Printf.printf "I've just read: 2:%s 3:%s\n%!" file2 file3;

  return_unit

let () =
  Lwt_unix.run (main ())
