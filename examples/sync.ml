open Lwt
open Irmin_unix

let path =
  if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else
    "git://github.com/mirage/ocaml-git.git"

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let upstream = Irmin.remote_uri path

let test () =
  Config.init ();
  let config = Irmin_git.config ~root:Config.root () in
  Irmin.create store config task >>= fun t ->
  Irmin.pull_exn (t "Syncing with upstream store") upstream `Update >>= fun () ->
  Irmin.read_exn (t "get the README") ["README.md"]>>= fun readme ->
  Irmin.with_hrw_view (t "Updating BAR and FOO") `Merge (fun view ->
      Irmin.update view ["BAR.md"] "Hoho!" >>= fun () ->
      Irmin.update view ["FOO.md"] "Hihi!" >>= fun () ->
      Lwt.return_unit
    ) >>= Irmin.Merge.exn >>= fun () ->
  Printf.printf "%s\n%!" readme;
  return_unit

let () =
  Lwt_main.run (test ())
