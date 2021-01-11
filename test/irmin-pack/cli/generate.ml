let ( let* ) x f = Lwt.bind x f
let data_dir = "data/layered_pack_upper"

let rm_dir () =
  if Sys.file_exists data_dir then (
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ())

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Store =
  Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)

let config root =
  let conf = Irmin_pack.config ~readonly:false ~fresh:true root in
  Irmin_pack_layered.config ~conf ~copy_in_upper:true ~with_lower:true ()

let info = Irmin.Info.v ~date:0L ~author:"" ""

let create_store () =
  rm_dir ();
  let* repo = Store.Repo.v (config data_dir) in
  let* _t = Store.master repo in
  let* tree = Store.Tree.add Store.Tree.empty [ "a"; "b"; "c" ] "x1" in
  let* c = Store.Commit.v repo ~info ~parents:[] tree in
  let* () = Store.freeze ~max:[ c ] ~copy_in_upper:false repo in
  let* () = Store.PrivateLayer.wait_for_freeze repo in
  let* tree = Store.Tree.add tree [ "a"; "b"; "d" ] "x2" in
  let hash = Store.Commit.hash c in
  let* c3 = Store.Commit.v repo ~info ~parents:[ hash ] tree in
  let* () = Store.Branch.set repo "master" c3 in
  Store.Repo.close repo

let () = Lwt_main.run (create_store ())
