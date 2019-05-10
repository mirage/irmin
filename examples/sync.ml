open Lwt.Infix

let info = Irmin_unix.info

let path =
  if Array.length Sys.argv = 2 then Sys.argv.(1)
  else "git://github.com/mirage/ocaml-git.git"

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync (Store)

let upstream = Store.remote path

let test () =
  Config.init ();
  let config = Irmin_git.config Config.root in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Sync.pull_exn t upstream `Set >>= fun _ ->
  Store.get t [ "README.md" ] >>= fun readme ->
  Store.get_tree t [] >>= fun tree ->
  Store.Tree.add tree [ "BAR.md" ] "Hoho!" >>= fun tree ->
  Store.Tree.add tree [ "FOO.md" ] "Hihi!" >>= fun tree ->
  Store.set_tree_exn t ~info:(info "merge") [] tree >|= fun () ->
  Printf.printf "%s\n%!" readme

let () = Lwt_main.run (test ())
