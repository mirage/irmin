open Lwt.Infix

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
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Sync.pull_exn t remote `Set >>= fun _ ->
  Store.get t [ "README.md" ] >>= fun readme ->
  Store.get_tree t [] >>= fun tree ->
  Store.Tree.add tree [ "BAR.md" ] "Hoho!" >>= fun tree ->
  Store.Tree.add tree [ "FOO.md" ] "Hihi!" >>= fun tree ->
  Store.set_tree_exn t ~info:(info "merge") [] tree >>= fun () ->
  Printf.printf "%s\n%!" readme;
  Store.get t [ "BAR.md" ] >>= fun bar ->
  Printf.printf "%s\n%!" bar;
  Store.get t [ "FOO.md" ] >>= fun foo ->
  Printf.printf "%s\n%!" foo;
  Sync.push_exn t remote >|= ignore

let () = Lwt_main.run (test ())
