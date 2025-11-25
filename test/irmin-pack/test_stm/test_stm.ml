open Common
open Tree_model
open STM

module Store = struct
  module Maker = Irmin_mem
  include Maker.Make (Schema)

  let config () = Irmin_mem.config ()
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

(* *****************************************)
module Model = struct
  include Common_model

  type sut = Store.t

  let init_sut () =
    let r = Store.Repo.v (Store.config ()) |> Store.main in
    Store.set_tree_exn ~info:Store.info r [] (Store.Tree.empty ());
    r

  let cleanup _c = ()
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

let agree_test_eio ~count ~domain_mgr =
  let module TT = STM_domain_eio.Make (Model) in
  TT.agree_test_par ~domain_mgr ~count ~name:"Irmin test parallel"

let () =
  let count = 500 in
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  QCheck_base_runner.run_tests_main [ agree_test_eio ~count ~domain_mgr ]
