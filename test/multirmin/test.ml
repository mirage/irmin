open Common

let root = Filename.concat "_build" "test-readonly"

module S = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

let info () = S.Info.empty

let do_the_do ro i =
  let t = S.main ro in
  let c = S.Head.get t in
  match S.Commit.of_hash ro (S.Commit.hash c) with
  | None -> failwith "no hash"
  | Some commit ->
      let tree = S.Commit.tree commit in
      let x = S.Tree.find tree [ "a" ] in
      if x <> Some "x" then failwith "RO find";
      Format.printf "Done: i:%d d:%d@." i (Domain.self () :> int)

let repeatedly_do fn arg () =
  for _ = 0 to 100 do
    Sys.opaque_identity (fn arg)
  done

let open_ro_after_rw_closed env =
  rm_dir root;
  let rw = S.Repo.v (config ~readonly:false ~fresh:true root) in
  let t = S.main rw in
  let tree = S.Tree.singleton [ "a" ] "x" in
  S.set_tree_exn ~parents:[] ~info t [] tree;
  S.Repo.close rw;
  let ro = S.Repo.v (config ~readonly:true ~fresh:false root) in
  let l =
    List.init 7 (fun i () ->
        Eio.Domain_manager.run env (repeatedly_do (do_the_do ro) i))
  in
  Eio.Fiber.all l;
  S.Repo.close ro

let () =
  Logs.set_level None;
  Eio_main.run @@ fun env -> open_ro_after_rw_closed env#domain_mgr
