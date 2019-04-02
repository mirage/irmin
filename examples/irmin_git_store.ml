(* Simple example of reading and writing in a Git repository *)
open Lwt.Infix
open Printf

let info = Irmin_unix.info

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let update t k v =
  let msg = sprintf "Updating /%s" (String.concat "/" k) in
  print_endline msg;
  Store.set_exn t ~info:(info "%s" msg) k v

let read_exn t k =
  let msg = sprintf "Reading /%s" (String.concat "/" k) in
  print_endline msg;
  Store.get t k

let main () =
  Config.init ();
  let config = Irmin_git.config ~bare:true Config.root in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  update t [ "root"; "misc"; "1.txt" ] "Hello world!" >>= fun () ->
  update t [ "root"; "misc"; "2.txt" ] "Hi!" >>= fun () ->
  update t [ "root"; "misc"; "3.txt" ] "How are you ?" >>= fun () ->
  read_exn t [ "root"; "misc"; "2.txt" ] >>= fun _ ->
  Store.clone ~src:t ~dst:"test" >>= fun x ->
  print_endline "cloning ...";
  update t [ "root"; "misc"; "3.txt" ] "Hohoho" >>= fun () ->
  update x [ "root"; "misc"; "2.txt" ] "Cool!" >>= fun () ->
  Store.merge_into ~info:(info "t: Merge with 'x'") x ~into:t >>= function
  | Error _ -> failwith "conflict!"
  | Ok () ->
      print_endline "merging ...";
      read_exn t [ "root"; "misc"; "2.txt" ] >>= fun _ ->
      read_exn t [ "root"; "misc"; "3.txt" ] >>= fun _ -> Lwt.return_unit

let () =
  Printf.printf
    "This example creates a Git repository in %s and use it to read \n\
     and write data:\n"
    Config.root;
  let _ = Sys.command (Printf.sprintf "rm -rf %s" Config.root) in
  Lwt_main.run (main ());
  Printf.printf "You can now run `cd %s && tig` to inspect the store.\n"
    Config.root
