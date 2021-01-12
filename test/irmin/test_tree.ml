open Lwt.Infix
open Irmin

module Metadata = struct
  type t = Default | Left | Right [@@deriving irmin]

  let merge =
    Merge.v t (fun ~old:_ _ _ -> Merge.conflict "Can't merge metadata")

  let default = Default
end

module Store =
  Irmin_mem.Make (Metadata) (Contents.String) (Path.String_list) (Branch.String)
    (Hash.BLAKE2B)

module Tree = Store.Tree

type diffs = (string list * (Contents.String.t * Metadata.t) Diff.t) list
[@@deriving irmin]

type kind = [ `Contents | `Node ] [@@deriving irmin]

module Alcotest = struct
  include Alcotest

  let gtestable typ = testable (Type.pp_dump typ) Type.(unstage (equal typ))
  let gcheck typ = check (gtestable typ)
  let diffs = gtestable diffs_t
end

let ( >> ) f g x = g (f x)
let ( let* ) = Lwt.bind
let ( let+ ) x f = Lwt.map f x
let c ?(info = Metadata.Default) blob = `Contents (blob, info)

let test_bindings _ () =
  let tree =
    Tree.of_concrete
      (`Tree [ ("aa", c "0"); ("ab", c "1"); ("a", `Tree []); ("b", c "3") ])
  in
  let check_sorted =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" [ "a"; "aa"; "ab"; "b" ]
  in
  (* [Tree.list] returns all keys in lexicographic order *)
  Tree.list tree [] >|= (List.map fst >> check_sorted)

let test_paginated_bindings _ () =
  let tree =
    Tree.of_concrete
      (`Tree
        [
          ("aa", c "0");
          ("a", `Tree []);
          ("bbb", c "3");
          ("b", c "3");
          ("aaa", `Tree []);
        ])
  in
  let check_sorted expected =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" expected
  in
  Tree.list ~offset:0 ~length:2 tree []
  >|= (List.map fst >> check_sorted [ "a"; "aa" ])
  >>= fun () ->
  Tree.list ~offset:2 ~length:3 tree []
  >|= (List.map fst >> check_sorted [ "aaa"; "b"; "bbb" ])
  >>= fun () ->
  Tree.list ~offset:1 ~length:1 tree []
  >|= (List.map fst >> check_sorted [ "aa" ])
  >>= fun () ->
  Tree.list ~offset:4 ~length:2 tree []
  >|= (List.map fst >> check_sorted [ "bbb" ])
  >>= fun () ->
  Tree.list ~offset:5 ~length:2 tree [] >|= (List.map fst >> check_sorted [])

(** Basic tests of the [Tree.diff] operation. *)
let test_diff _ () =
  let tree bs = Tree.of_concrete (`Tree bs) in
  let empty = tree [] in
  let single = tree [ ("k", c "v") ] in

  (* Adding a single key *)
  Tree.diff empty single
  >|= Alcotest.(check diffs)
        "Added [k → v]"
        [ ([ "k" ], `Added ("v", Default)) ]
  >>= fun () ->
  (* Removing a single key *)
  Tree.diff single empty
  >|= Alcotest.(check diffs)
        "Removed [k → v]"
        [ ([ "k" ], `Removed ("v", Default)) ]
  >>= fun () ->
  (* Changing metadata *)
  Tree.diff
    (tree [ ("k", c ~info:Left "v") ])
    (tree [ ("k", c ~info:Right "v") ])
  >|= Alcotest.(check diffs)
        "Changed metadata"
        [ ([ "k" ], `Updated (("v", Left), ("v", Right))) ]

(* Correct stats for a completely lazy tree *)
let lazy_stats = Tree.{ nodes = 0; leafs = 0; skips = 1; depth = 0; width = 0 }

let test_clear _ () =
  let size = 830829 in
  let* large_tree =
    List.init size string_of_int
    |> Lwt_list.fold_left_s (fun acc i -> Tree.add acc [ i ] i) Tree.empty
  in
  let* () =
    Tree.stats ~force:false large_tree
    >|= Alcotest.(gcheck Tree.stats_t)
          "Before clear, root node is eagerly evaluated"
          { nodes = 1; leafs = size; skips = 0; depth = 1; width = size }
  in
  Tree.clear large_tree;
  let* () =
    Tree.stats ~force:false large_tree
    >|= Alcotest.(gcheck Tree.stats_t)
          "After clear, root node is no longer cached" lazy_stats
  in
  Lwt.return_unit

let with_binding k v t = Tree.add_tree t k v

let clear_and_assert_lazy tree =
  Tree.clear tree;
  Tree.stats ~force:false tree
  >|= Alcotest.(gcheck Tree.stats_t)
        "Initially the tree is entirely lazy" lazy_stats

let test_fold_force _ () =
  let* invalid_tree =
    let+ repo = Store.Repo.v (Irmin_mem.config ()) in
    let hash = Store.Hash.hash (fun f -> f "") in
    Tree.shallow repo hash
  in

  (* Ensure that [fold] doesn't force a lazy tree when [~force:(`False f)],
     and that [f] is called the correct number of times. *)
  let* () =
    let* tree =
      Lwt.return Tree.empty
      >>= with_binding [ "existing"; "subtree" ] (Tree.of_contents "value")
      >>= with_binding [ "dangling"; "subtree"; "hash" ] invalid_tree
      >>= with_binding [ "other"; "lazy"; "path" ] invalid_tree
    in
    let force = `False (Lwt.wrap2 List.cons) in
    Tree.fold ~force tree []
    >|= Alcotest.(check (slist (list string) Stdlib.compare))
          "Unforced paths"
          [ [ "dangling"; "subtree"; "hash" ]; [ "other"; "lazy"; "path" ] ]
  in
  let sample_tree =
    Tree.of_concrete
      (`Tree
        [
          ("a", `Tree [ ("aa", c "v-aa"); ("ab", c "v-ab"); ("ac", c "v-ac") ]);
          ("b", c "v-b");
          ("c", c "v-c");
        ])
  in
  let eager_stats =
    Tree.{ nodes = 2; leafs = 5; skips = 0; depth = 2; width = 3 }
  in

  (* Ensure that [fold ~force:`True] forces all lazy trees. *)
  let* () =
    let* () = clear_and_assert_lazy sample_tree in
    let* () = Tree.fold ~force:`True sample_tree () in
    Tree.stats ~force:false sample_tree
    >|= Alcotest.(gcheck Tree.stats_t)
          "After folding, the tree is eagerly evaluated" eager_stats
  in

  (* Ensure that [fold ~force:`And_clear] visits all children and does not
     leave them cached. *)
  let* () =
    let* () = clear_and_assert_lazy sample_tree in
    let* contents =
      Tree.fold ~force:`And_clear
        ~contents:(fun _ -> Lwt.wrap2 List.cons)
        sample_tree []
    in
    let+ () =
      Tree.stats ~force:false sample_tree
      >|= Alcotest.(gcheck Tree.stats_t)
            "After folding, the tree is cleared" lazy_stats
    in
    Alcotest.(check (slist string compare))
      "During forced fold, all contents were traversed"
      [ "v-aa"; "v-ab"; "v-ac"; "v-b"; "v-c" ]
      contents
  in

  Lwt.return_unit

let test_kind_empty_path _ () =
  let cont = c "c" |> Tree.of_concrete in
  let tree = `Tree [ ("k", c "c") ] |> Tree.of_concrete in
  let* k = Tree.kind cont [] in
  Alcotest.(check (option (gtestable kind_t)))
    "Kind of empty path in content"
    (Some `Contents)
    k;
  let* k = Tree.kind tree [] in
  Alcotest.(check (option (gtestable kind_t)))
    "Kind of empty path in tree"
    (Some `Node)
    k;
  Lwt.return_unit

let suite =
  [
    Alcotest_lwt.test_case "bindings" `Quick test_bindings;
    Alcotest_lwt.test_case "paginated bindings" `Quick test_paginated_bindings;
    Alcotest_lwt.test_case "diff" `Quick test_diff;
    Alcotest_lwt.test_case "clear" `Quick test_clear;
    Alcotest_lwt.test_case "fold" `Quick test_fold_force;
    Alcotest_lwt.test_case "kind of empty path" `Quick test_kind_empty_path;
  ]
