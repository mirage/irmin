(* Simple example of reading and writing in a Git repository *)
open Lwt
open Irmin_unix
open Printf

module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let update t k v =
  let msg = sprintf "Updating /%s" (String.concat "/" k) in
  print_endline msg;
  Store.update (t msg) k v

let read_exn t k =
  let msg = sprintf "Reading /%s" (String.concat "/" k) in
  print_endline msg;
  Store.read_exn (t msg) k

let main () =
  Config.init ();
  let config = Irmin_git.config ~root:Config.root ~bare:true () in
  Store.Repo.create config >>= Store.master task >>= fun t ->

  update t ["root";"misc";"1.txt"] "Hello world!" >>= fun () ->
  update t ["root";"misc";"2.txt"] "Hi!" >>= fun () ->
  update t ["root";"misc";"3.txt"] "How are you ?" >>= fun () ->
  read_exn t ["root";"misc";"2.txt"] >>= fun _ ->

  Store.clone_force task (t "x: Cloning 't'") "test" >>= fun x ->
  print_endline "cloning ...";

  update t ["root";"misc";"3.txt"] "Hohoho" >>= fun () ->
  update x ["root";"misc";"2.txt"] "Cool!"  >>= fun () ->

  Store.merge_exn "t: Merge with 'x'" x ~into:t >>= fun () ->
  print_endline "merging ...";

  read_exn t ["root";"misc";"2.txt"]  >>= fun _ ->
  read_exn t ["root";"misc";"3.txt"]  >>= fun _ ->

  return_unit

let () =
  Printf.printf
    "This example creates a Git repository in %s and use it to read \n\
     and write data:\n" Config.root;
  let _ = Sys.command (Printf.sprintf "rm -rf %s" Config.root) in
  Lwt_main.run (main ());
  Printf.printf
    "You can now run `cd %s && tig` to inspect the store.\n" Config.root;
