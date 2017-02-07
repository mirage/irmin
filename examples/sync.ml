open Lwt.Infix
open Irmin_unix

let path =
  if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else
    "git://github.com/mirage/ocaml-git.git"

module Store =
  Irmin_git.FS
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)

module Sync = Irmin.Sync(Store)

let upstream = Irmin.remote_uri path

let test () =
  Config.init ();
  let config = Irmin_git.config ~root:Config.root () in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Sync.pull_exn t upstream `Update >>= fun () ->
  Store.get t ["README.md"]>>= fun readme ->
  Store.getv t [] >>= fun view ->
  Store.Tree.add view ["BAR.md"] "Hoho!" >>= fun view ->
  Store.Tree.add view ["FOO.md"] "Hihi!" >>= fun view ->
  Store.setv t (info "merge") [] view >|= fun () ->
  Printf.printf "%s\n%!" readme

let () =
  Lwt_main.run (test ())
