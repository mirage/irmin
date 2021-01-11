(* Simple example of reading and writing in a Git repository *)
open Lwt.Infix
open Printf

let ( let* ) x f = Lwt.bind x f
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
  let* repo = Store.Repo.v config in
  let* t = Store.master repo in
  update t [ "root"; "misc"; "1.txt" ] "Hello world!" >>= fun () ->
  update t [ "root"; "misc"; "2.txt" ] "Hi!" >>= fun () ->
  update t [ "root"; "misc"; "3.txt" ] "How are you ?" >>= fun () ->
  let* _ = read_exn t [ "root"; "misc"; "2.txt" ] in
  let* x = Store.clone ~src:t ~dst:"test" in
  print_endline "cloning ...";
  update t [ "root"; "misc"; "3.txt" ] "Hohoho" >>= fun () ->
  update x [ "root"; "misc"; "2.txt" ] "Cool!" >>= fun () ->
  Store.merge_into ~info:(info "t: Merge with 'x'") x ~into:t >>= function
  | Error _ -> failwith "conflict!"
  | Ok () ->
      print_endline "merging ...";
      let* _ = read_exn t [ "root"; "misc"; "2.txt" ] in
      let* _ = read_exn t [ "root"; "misc"; "3.txt" ] in
      Lwt.return_unit

let () =
  Printf.printf
    "This example creates a Git repository in %s and use it to read \n\
     and write data:\n"
    Config.root;
  let _ = Sys.command (Printf.sprintf "rm -rf %s" Config.root) in
  Lwt_main.run (main ());
  Printf.printf "You can now run `cd %s && tig` to inspect the store.\n"
    Config.root
