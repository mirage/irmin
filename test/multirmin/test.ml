open Common

let root = Filename.concat "_build" "test-readonly"

module S = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = S.Info.empty

let test_find repo i =
  let tree =
    repo
    |> S.main
    |> S.Head.get
    |> S.Commit.hash
    |> S.Commit.of_hash repo
    |> Option.get
    |> S.Commit.tree
  in
  let start_value = S.Tree.find tree [ "start" ] in
  assert (start_value = Some "content-start");
  let str_i = string_of_int i in
  let value = S.Tree.find tree [ str_i ] in
  if not (value = Some ("content-" ^ str_i)) then
    Format.printf "Couldn't read correct value from thread %d@." i

let test_add repo i =
  let main = S.main repo in
  let tree =
    main
    |> S.Head.get
    |> S.Commit.hash
    |> S.Commit.of_hash repo
    |> Option.get
    |> S.Commit.tree
  in
  let str_i = string_of_int i in
  let tree' = S.Tree.add tree [ str_i ] ("content-" ^ str_i) in
  S.set_tree_exn ~info main [] tree';
  ()

let repeatedly_do fn arg () =
  for _ = 0 to 100 do
    Sys.opaque_identity (fn arg)
  done

let dispatch repo i () =
  repeatedly_do
    (if i mod 2 = 0 then (* Readers *)
     test_find repo
    else (* Writers *) test_add repo)
    (i / 2) ()

let setup d_mgr =
  rm_dir root;
  let repo = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let main = S.main repo in
  let init = S.Tree.singleton [ "start" ] "content-start" in
  S.set_tree_exn ~parents:[] ~info main [] init;
  S.Repo.close repo;
  let repo = S.Repo.v (config ~readonly:false ~fresh:false root) in
  let fibers =
    List.init 7 (fun i () -> Eio.Domain_manager.run d_mgr (dispatch repo i))
  in
  Eio.Fiber.all fibers;
  S.Repo.close repo

let () =
  Logs.set_level None;
  Eio_main.run @@ fun env -> setup env#domain_mgr
