This database has been generated with `irmin-pack.2.10.2` and the following
program:

```ocaml
open Lwt.Syntax

let data_dir = "output"

let rm_dir () =
  if Sys.file_exists data_dir then (
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ())

module Conf = Irmin_tezos.Conf

module Store =
  Irmin_pack.V1 (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)

let config root =
  let conf = Irmin_pack.config ~readonly:false ~fresh:true root in
  Irmin_pack_layered.config ~conf ~with_lower:true ()

let info = Irmin.Info.empty

(* random numeric string of length 4 *)
let random_content () =
  Random.int 10000 |> string_of_int

(* random path [0-2]/[0-99] *)
let random_key () =
  let d x = Random.int x |> string_of_int in
  [d 3; d 100]

let random_commit repo tree parent =
  let* tree = Store.Tree.add tree (random_key ()) (random_content ()) in
  let hash = Store.Commit.hash parent in
  let* c = Store.Commit.v repo ~info ~parents:[hash] tree in
  Lwt.return (c, tree)

let fill_repo repo =
  let tree = Store.Tree.empty () in
  let* origin = Store.Commit.v repo ~info ~parents:[] tree in
  let rec add_commits n parent tree =
    if n <= 0 then Lwt.return parent
    else
      let* c, tree = random_commit repo tree parent in
      add_commits (n - 1) c tree
  in
  add_commits 10_000 origin tree

let main () =
  Random.self_init ();
  rm_dir ();
  let* repo = Store.Repo.v (config data_dir) in
  let* head = fill_repo repo in
  let* () = Store.Branch.set repo "main" head in
  Store.Repo.close repo

let () = Lwt_main.run (main ())
```
