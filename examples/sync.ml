let ( let* ) x f = Lwt.bind x f
let ( let+ ) x f = Lwt.map f x
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
  let* repo = Store.Repo.v config in
  let* t = Store.master repo in
  let* _ = Sync.pull_exn t upstream `Set in
  let* readme = Store.get t [ "README.md" ] in
  let* tree = Store.get_tree t [] in
  let* tree = Store.Tree.add tree [ "BAR.md" ] "Hoho!" in
  let* tree = Store.Tree.add tree [ "FOO.md" ] "Hihi!" in
  let+ () = Store.set_tree_exn t ~info:(info "merge") [] tree in
  Printf.printf "%s\n%!" readme

let () = Lwt_main.run (test ())
