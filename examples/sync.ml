open Lwt
open Irmin_unix

let path =
  if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else
    "git://github.com/mirage/ocaml-git.git"

module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module Sync = Irmin.Sync(Store)
module View = Irmin.View(Store)

let upstream = Irmin.remote_uri path

let test () =
  Config.init ();
  let config = Irmin_git.config ~root:Config.root () in
  Store.Repo.create config >>= Store.master task >>= fun t ->
  Sync.pull_exn (t "Syncing with upstream store") upstream `Update >>= fun () ->
  Store.read_exn (t "get the README") ["README.md"]>>= fun readme ->
  Irmin.with_hrw_view (module View) (t "Updating BAR and FOO") `Merge ~path:[] (fun view ->
      View.update view ["BAR.md"] "Hoho!" >>= fun () ->
      View.update view ["FOO.md"] "Hihi!" >>= fun () ->
      Lwt.return_unit
    ) >>= Irmin.Merge.exn >>= fun () ->
  Printf.printf "%s\n%!" readme;
  return_unit

let () =
  Lwt_main.run (test ())
