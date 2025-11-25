open Common
open Tree_model
open STM

module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)

  let config ?(readonly = false) ?(fresh = true) root =
    Irmin_pack.config ~readonly ?index_log_size ~fresh root

  let info : Info.f = fun () -> Info.empty
  let add_and_commit main path contents = set_exn ~info main path contents
  let remove_and_commit main path = remove_exn ~info main path

  let find main path : string option =
    let tree = main |> Head.get |> Commit.tree in
    Tree.find tree path

  let with_tree main path f = with_tree_exn ~info main path f
end

let with_tree_add_irmin path content : Store.tree option -> Store.tree option =
  function
  | _ -> Some (Store.Tree.add (Store.Tree.empty ()) path content)

let with_tree_function_irmin =
  [ ("remove", Common_model.with_tree_remove); ("add", with_tree_add_irmin) ]

module type ENV = sig
  val fs : Eio.Fs.dir_ty Eio.Path.t
end

(* *****************************************)
module Model (Env : ENV) = struct
  include Common_model

  type sut = Store.t

  let init_state = TreeModel.add TreeModel.empty [ "e" ] "not empty"
  let root ~fs = Eio.Path.(fs / "test-irmin-pack-stm-bin")

  let make_store ~fs =
    Eio.Switch.run @@ fun sw ->
    let root = root ~fs in
    rm_dir root;
    let repo = Store.Repo.v (Store.config ~sw ~fs ~fresh:true root) in
    let main = Store.main repo in
    Store.add_and_commit main [ "e" ] "not empty";

    Store.Repo.close repo

  let init_sut () =
    Eio.Switch.run @@ fun sw ->
    let repo =
      Store.Repo.v
        (Store.config ~sw ~fs:Env.fs ~readonly:true ~fresh:false
           (root ~fs:Env.fs))
    in
    Store.main repo

  let cleanup c = Store.Repo.close (Store.repo c)
  let precond _ _ = true

  (* run *)
  let run c d =
    match c with
    | Add (path, contents) ->
        Res
          ( result unit exn,
            protect (fun d -> Store.add_and_commit d path contents) d )
    | Find i -> Res (option string, Store.find d i)
    | Remove path ->
        Res
          (result unit exn, protect (fun d -> Store.remove_and_commit d path) d)
    | With_tree (path, content, f) ->
        let f_irmin = List.assoc f with_tree_function_irmin in
        let f_irmin = f_irmin path content in
        Res
          (result unit exn, protect (fun d -> Store.with_tree d path f_irmin) d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add _, Res ((Result (Unit, Exn), _), _res) -> true
    | Find i, Res ((Option String, _), res) -> res = TreeModel.find s i
    | Remove _, Res ((Result (Unit, Exn), _), _res) -> true
    | With_tree _, Res ((Result (Unit, Exn), _), _res) -> true
    | _, _ -> false
end

let agree_test_eio ~count ~domain_mgr ~fs =
  let module Env : ENV = struct
    let fs = fs
  end in
  let module Model = Model (Env) in
  Model.make_store ~fs;
  let module TT = STM_domain_eio.Make (Model) in
  TT.agree_test_par ~domain_mgr ~count ~name:"Irmin test parallel"

let () =
  let count = 50 in
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let fs = Eio.Stdenv.fs env in
  QCheck_base_runner.run_tests_main [ agree_test_eio ~count ~domain_mgr ~fs ]
