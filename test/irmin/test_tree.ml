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
  let assert_ msg b = check bool msg true b

  let check_tree_lwt =
    let concrete_tree = gtestable Tree.concrete_t in
    fun msg ~expected b_lwt ->
      b_lwt >>= Tree.to_concrete >|= Alcotest.check concrete_tree msg expected
end

let ( >> ) f g x = g (f x)
let ( let* ) = Lwt.bind
let ( let+ ) x f = Lwt.map f x
let c ?(info = Metadata.Default) blob = `Contents (blob, info)

let invalid_tree () =
  let+ repo = Store.Repo.v (Irmin_mem.config ()) in
  let hash = Store.Hash.hash (fun f -> f "") in
  Tree.shallow repo hash

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
  let* () =
    Tree.list ~offset:0 ~length:2 tree []
    >|= (List.map fst >> check_sorted [ "a"; "aa" ])
  in
  let* () =
    Tree.list ~offset:2 ~length:3 tree []
    >|= (List.map fst >> check_sorted [ "aaa"; "b"; "bbb" ])
  in
  let* () =
    Tree.list ~offset:1 ~length:1 tree []
    >|= (List.map fst >> check_sorted [ "aa" ])
  in
  let* () =
    Tree.list ~offset:4 ~length:2 tree []
    >|= (List.map fst >> check_sorted [ "bbb" ])
  in
  let* () =
    Tree.list ~offset:5 ~length:2 tree [] >|= (List.map fst >> check_sorted [])
  in
  Lwt.return_unit

let tree bs = Tree.of_concrete (`Tree bs)

(** Basic tests of the [Tree.diff] operation. *)
let test_diff _ () =
  let empty = tree [] in
  let single = tree [ ("k", c "v") ] in

  (* Adding a single key *)
  let* () =
    Tree.diff empty single
    >|= Alcotest.(check diffs)
          "Added [k \226\134\146 v]"
          [ ([ "k" ], `Added ("v", Default)) ]
  in
  (* Removing a single key *)
  let* () =
    Tree.diff single empty
    >|= Alcotest.(check diffs)
          "Removed [k \226\134\146 v]"
          [ ([ "k" ], `Removed ("v", Default)) ]
  in
  (* Changing metadata *)
  Tree.diff
    (tree [ ("k", c ~info:Left "v") ])
    (tree [ ("k", c ~info:Right "v") ])
  >|= Alcotest.(check diffs)
        "Changed metadata"
        [ ([ "k" ], `Updated (("v", Left), ("v", Right))) ]

let test_add _ () =
  let sample_tree ?(ab = "ab_v") ?ac () : Tree.concrete =
    let ac = match ac with Some ac -> [ ("ac", ac) ] | None -> [] in
    `Tree [ ("a", `Tree ([ ("aa", c "0"); ("ab", c ab) ] @ ac)); ("b", c "3") ]
  in

  let* () =
    Alcotest.check_tree_lwt "Adding a root value to an empty tree"
      ~expected:(c "1")
      (Tree.add Tree.empty [] "1")
  in

  let* () =
    let t = Tree.of_concrete (sample_tree ()) in
    let expected = sample_tree ~ab:"new_value" () in
    Alcotest.check_tree_lwt "Replacing an existing value in a tree" ~expected
      (Tree.add t [ "a"; "ab" ] "new_value")
  in

  let* () =
    let t = Tree.of_concrete (sample_tree ()) in
    let expected = sample_tree ~ac:(`Tree [ ("aca", c "new_value") ]) () in
    Alcotest.check_tree_lwt
      "Adding at a non-existent path in a tree creates necessary intermediate \
       nodes"
      ~expected
      (Tree.add t [ "a"; "ac"; "aca" ] "new_value")
  in

  let* () =
    let t = Tree.of_concrete (c "1") in
    let+ t' = Tree.add t [] "1" in
    Alcotest.assert_ "Re-adding a root value preserves physical equality"
      (t == t')
  in

  let* () =
    let t = tree [ ("a", `Tree [ ("b", c "1") ]) ] in
    let+ t' = Tree.add t [ "a"; "b" ] "1" in
    Alcotest.assert_ "Re-adding a non-root value preserves physical equality"
      (t == t')
  in

  Lwt.return_unit

let test_remove _ () =
  let tree =
    Tree.of_concrete
      (`Tree [ ("a", `Tree [ ("aa", c "0"); ("ab", c "1") ]); ("b", c "3") ])
  in

  let* () =
    let t = Tree.empty in
    let+ t' = Tree.remove t [] in
    Alcotest.assert_ "Removing in an empty tree preserves physical equality"
      (t == t')
  in

  let* () =
    let+ tree' = Tree.remove tree [ "a"; "non"; "existent"; "path" ] in
    Alcotest.assert_
      "Removing at a non-existent path in a non-empty tree preserves physical \
       equality"
      (tree == tree')
  in

  let* () =
    let tree = Tree.of_concrete (c "1") in
    let+ tree' = Tree.remove tree [ "a"; "non"; "existent"; "path" ] in
    Alcotest.assert_
      "Removing at a non-existent path in a root contents value preserves \
       physical equality"
      (tree == tree')
  in

  let* () =
    Alcotest.check_tree_lwt
      "Removing a root contents value results in an empty root node."
      ~expected:(`Tree [])
      (Tree.remove (Tree.of_concrete (c "1")) [])
  in

  Lwt.return_unit

(* Build a function that requires a given input, always returns a given output,
   and can be called at most once. *)
let transform_once : type a b. a Type.t -> a -> b -> a -> b =
 fun typ ->
  let equal = Type.(unstage (equal typ)) in
  let pp = Type.pp_dump typ in
  fun source target ->
    let called = ref false in
    fun x ->
      if !called then Alcotest.failf "Transformation called more than once";
      called := true;
      if equal source x then target
      else Alcotest.failf "Expected %a but got %a" pp source pp x

let test_update _ () =
  let unrelated_binding = ("a_unrelated", c "<>") in
  let abc ?info v =
    `Tree
      [ ("a", `Tree [ ("b", `Tree [ ("c", c ?info v) ]) ]); unrelated_binding ]
  in
  let abc1 = Tree.of_concrete (abc "1") in
  let ( --> ) = transform_once [%typ: string option] in

  let* () =
    Alcotest.check_tree_lwt
      "Changing the value of a root contents node results in a new contents \
       node."
      ~expected:(c "2")
      (Tree.update (Tree.of_concrete (c "1")) [] (Some "1" --> Some "2"))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Removing a root contents node results in an empty root node."
      ~expected:(`Tree [])
      (Tree.update (Tree.of_concrete (c "1")) [] (Some "1" --> None))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Updating a root node to a contents value removes all bindings and sets \
       the correct metadata."
      ~expected:(c ~info:Metadata.Right "2")
      (Tree.update ~metadata:Metadata.Right abc1 [] (None --> Some "2"))
  in

  let* () =
    (* Replacing a root node with a dangling hash does not raise an
       exception. *)
    let* invalid_tree = invalid_tree () in
    Tree.update_tree abc1 [] (function
      | Some _ -> Some invalid_tree
      | None -> assert false)
    >|= ignore
  in

  let* () =
    Alcotest.check_tree_lwt
      "Updating at an existing contents path changes the contents value \
       appropriately."
      ~expected:(abc "2")
      (Tree.update abc1 [ "a"; "b"; "c" ] (Some "1" --> Some "2"))
  in

  let* () =
    let s = "1" and s' = "1" ^ "" in
    assert (s != s');
    let+ abc1' = Tree.update abc1 [ "a"; "b"; "c" ] (Some s --> Some s') in
    Alcotest.assert_
      "Performing a no-op change to tree contents preserves physical equality"
      (abc1 == abc1')
  in

  let* () =
    let+ abc1' =
      Tree.update_tree abc1 [ "a"; "b" ] (function
        | Some t -> Some t
        | None -> assert false)
    in
    Alcotest.assert_
      "Replacing a subtree node with a physically-equal one preserves physical \
       equality"
      (abc1 == abc1')
  in

  let* () =
    Alcotest.check_tree_lwt
      "Changing the metadata of an existing contents value updates the tree."
      ~expected:(abc ~info:Metadata.Left "1")
      (Tree.update ~metadata:Metadata.Left abc1 [ "a"; "b"; "c" ]
         (Some "1" --> Some "1"))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Removing a siblingless contents value causes newly-empty directories to \
       be pruned."
      ~expected:(`Tree [ unrelated_binding ])
      (Tree.update abc1 [ "a"; "b"; "c" ] (Some "1" --> None))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Removing a siblingless node causes newly-empty directories to be pruned"
      ~expected:(`Tree [ unrelated_binding ])
      (Tree.update_tree abc1 [ "a"; "b" ] (function
        | Some _ -> None
        | None -> assert false))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Updating at a non-existent contents path adds a new directory entry."
      ~expected:
        (`Tree
          [
            ("a", `Tree [ ("b", `Tree [ ("c", c "1"); ("c'", c "new_value") ]) ]);
            unrelated_binding;
          ])
      (Tree.update abc1 [ "a"; "b"; "c'" ] (None --> Some "new_value"))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Updating at an existing node path replaces the subtree with the given \
       element."
      ~expected:
        (`Tree [ ("a", `Tree [ ("b", c "new_value") ]); unrelated_binding ])
      (Tree.update abc1 [ "a"; "b" ] (None --> Some "new_value"))
  in

  let* () =
    Alcotest.check_tree_lwt
      "Updating at a path in an empty tree creates the necessary intermediate \
       nodes with the new contents."
      ~expected:(`Tree [ ("a", `Tree [ ("b", `Tree [ ("c", c "1") ]) ]) ])
      (Tree.update Tree.empty [ "a"; "b"; "c" ] (None --> Some "1"))
  in

  let* () =
    let+ abc1' = Tree.update abc1 [ "a"; "b"; "c"; "d"; "e" ] (None --> None) in
    Alcotest.assert_
      "Removing at a non-existent path in a non-empty tree preserves physical \
       equality."
      (abc1 == abc1')
  in

  let* () =
    let t = Tree.empty in
    let+ t' = Tree.update t [] (None --> None) in
    Alcotest.assert_ "Removing from an empty tree preserves physical equality"
      (t == t')
  in

  Lwt.return_unit

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
    clear_and_assert_lazy sample_tree >>= fun () ->
    Tree.fold ~force:`True sample_tree () >>= fun () ->
    Tree.stats ~force:false sample_tree
    >|= Alcotest.(gcheck Tree.stats_t)
          "After folding, the tree is eagerly evaluated" eager_stats
  in

  (* Ensure that [fold ~force:`And_clear] visits all children and does not
     leave them cached. *)
  let* () =
    clear_and_assert_lazy sample_tree >>= fun () ->
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
    Alcotest_lwt.test_case "add" `Quick test_add;
    Alcotest_lwt.test_case "remove" `Quick test_remove;
    Alcotest_lwt.test_case "update" `Quick test_update;
    Alcotest_lwt.test_case "clear" `Quick test_clear;
    Alcotest_lwt.test_case "fold" `Quick test_fold_force;
    Alcotest_lwt.test_case "kind of empty path" `Quick test_kind_empty_path;
  ]
