open Common
open QCheck
open STM
open Tree_model

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

type path = string list

let path_to_string path = String.concat "/" path

(*  Paths are randomly built following a restrictive pattern to increase chance of collision: the name of the next step of the path iss built with the characters "a", "b" and "c" only with the following logic:
      - at the root level, the possible names are "a", "b" and "c"
      - at any other level, the possible names are the concatenation of the name of the parent directory with "a", "b" or "c" (chosen randomly)

      The content is simply a string.
  *)
let possible_names = [ "a"; "b"; "c"; "d" ]

let possible_contents =
  [
    "cat";
    "dog";
    "wombat";
    "lizard";
    "hamster";
    "turtle";
    "rabbit";
    "snake";
    "ferret";
    "meerkat";
    "chinchilla";
    "hedgehog";
    "parrot";
    "kangaroo";
    "iguana";
    "otter";
    "pony";
    "goat";
    "raccoon";
  ]

(** Helper functions *)
let build_path_next_step prev =
  match prev with
  | [] -> possible_names
  | d :: _ -> List.map (fun str -> d ^ str) possible_names

(* for with_tree we need a "random" function *)
let with_tree_remove _ _ = function _ -> None

let with_tree_add path content : TreeModel.t option -> TreeModel.t option =
  function
  | _ -> Some (TreeModel.add TreeModel.empty path content)

let with_tree_add_irmin path content : Store.tree option -> Store.tree option =
  function
  | _ -> Some (Store.Tree.add (Store.Tree.empty ()) path content)

let with_tree_function =
  [
    ("remove", (with_tree_remove, with_tree_remove));
    ("add", (with_tree_add, with_tree_add_irmin));
  ]

(* *****************************************)
module Model = struct
  type cmd =
    | Add of (path * string)
    | Find of path
    | Remove of string list
    | With_tree of (path * string * string)

  (* Printer *)
  let show_cmd c =
    match c with
    | Add (path, s) ->
        "Add path(" ^ path_to_string path ^ ") content(" ^ s ^ ")"
    | Find s -> "Find " ^ path_to_string s
    | Remove s -> "Remove " ^ path_to_string s
    | With_tree (path, content, name) -> (
        match name with
        | "remove" ->
            "With_tree: path (" ^ path_to_string path ^ "), function remove"
        | "add" ->
            "With_tree: path ("
            ^ path_to_string path
            ^ "), function add with content ("
            ^ content
            ^ ")"
        | _ -> "With_tree unknown function")

  type state = TreeModel.t
  type sut = Store.t

  (* Generator *)
  let arb_cmd _s =
    let max_tree_depth = 3 in
    let depth_gen =
      Gen.small_nat |> Gen.map (fun n -> 1 + (n mod max_tree_depth))
    in

    let path_gen =
      let open Gen in
      depth_gen >>= fun d ->
      let rec build_path d acc =
        if d = 0 then Gen.return @@ List.rev acc
        else
          let next_steps = build_path_next_step acc in
          Gen.oneof
            (List.map (fun step -> build_path (d - 1) (step :: acc)) next_steps)
      in
      build_path d []
    in

    let contents_gen = Gen.oneofl possible_contents in

    let with_tree_function_gen = Gen.oneofl (List.map fst with_tree_function) in

    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2
             (fun path content -> Add (path, content))
             path_gen contents_gen;
           Gen.map (fun s -> Find s) path_gen;
           Gen.map (fun s -> Remove s) path_gen;
           Gen.map3
             (fun path content func_name ->
               With_tree (path, content, func_name))
             path_gen contents_gen with_tree_function_gen;
         ])

  (* Init functions *)
  let init_state = TreeModel.empty

  let init_sut () =
    let r = Store.Repo.v (Store.config ()) |> Store.main in
    Store.set_tree_exn ~info:Store.info r [] (Store.Tree.empty ());
    r

  let cleanup _c = ()

  (* next_state *)
  let next_state c s =
    match c with
    | Add (path, contents) -> TreeModel.add s path contents
    | Find _ -> s
    | Remove path -> TreeModel.remove s path
    | With_tree (path, contents, fname) ->
        let f, _ = List.assoc fname with_tree_function in
        TreeModel.with_tree s path (f path contents)

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
        let _, f_irmin = List.assoc f with_tree_function in
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
