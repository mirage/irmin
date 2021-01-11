open Lwt.Infix

let ( let* ) x f = Lwt.bind x f
let info = Irmin_unix.info

let url, user, token =
  if Array.length Sys.argv = 4 then (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3))
  else failwith "usage: push.exe url user token"

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync (Store)

let headers =
  let e = Cohttp.Header.of_list [] in
  Cohttp.Header.add_authorization e (`Basic (user, token))

let remote = Store.remote ~headers url

let test () =
  Config.init ();
  let config = Irmin_git.config Config.root in
  let* repo = Store.Repo.v config in
  let* t = Store.master repo in
  let* _ = Sync.pull_exn t remote `Set in
  let* readme = Store.get t [ "README.md" ] in
  let* tree = Store.get_tree t [] in
  let* tree = Store.Tree.add tree [ "BAR.md" ] "Hoho!" in
  let* tree = Store.Tree.add tree [ "FOO.md" ] "Hihi!" in
  Store.set_tree_exn t ~info:(info "merge") [] tree >>= fun () ->
  Printf.printf "%s\n%!" readme;
  let* bar = Store.get t [ "BAR.md" ] in
  Printf.printf "%s\n%!" bar;
  let* foo = Store.get t [ "FOO.md" ] in
  Printf.printf "%s\n%!" foo;
  Sync.push_exn t remote >|= ignore

let () = Lwt_main.run (test ())
