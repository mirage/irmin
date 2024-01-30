(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Irmin.Export_for_backends
open Irmin

module Metadata = struct
  type t = Default | Left | Right [@@deriving irmin]

  let merge =
    Merge.v t (fun ~old:_ _ _ -> Merge.conflict "Can't merge metadata")

  let default = Default
end

module Schema = struct
  module Metadata = Metadata
  module Contents = Contents.String
  module Path = Path.String_list
  module Branch = Branch.String
  module Hash = Hash.BLAKE2B
  module Node = Node.Generic_key.Make (Hash) (Path) (Metadata)
  module Commit = Commit.Make (Hash)
  module Info = Info.Default
end

module Store = Irmin_mem.Make (Schema)
module Tree = Store.Tree
open Schema

type diffs = (string list * (Contents.t * Metadata.t) Diff.t) list
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
    fun ?__POS__:pos msg ~expected b_lwt ->
      b_lwt
      |> Tree.to_concrete
      |> Alcotest.check ?pos concrete_tree msg expected

  let inspect =
    Alcotest.testable
      (fun ppf -> function
        | `Contents -> Fmt.string ppf "contents"
        | `Node `Key -> Fmt.string ppf "key"
        | `Node `Map -> Fmt.string ppf "map"
        | `Node `Value -> Fmt.string ppf "value"
        | `Node `Portable_dirty -> Fmt.string ppf "portable_dirty"
        | `Node `Pruned -> Fmt.string ppf "pruned")
      ( = )
end

let check_exn_lwt ~exn_type pos f =
  try
    let _ = f () in
    Alcotest.failf ~pos
      "Expected a `%s` exception, but no exception was raised."
      (match exn_type with
      | `Dangling_hash -> "Dangling_hash"
      | `Pruned_hash -> "Pruned_hash")
  with exn -> (
    match (exn_type, exn) with
    | `Dangling_hash, Tree.Dangling_hash _ -> ()
    | `Pruned_hash, Tree.Pruned_hash _ -> ()
    | _ -> raise exn)

(* Let-syntax for testing all possible combinations of a set of choices: *)
let ( let&* ) x f = List.iter f x
and ( and&* ) l m = List.concat_map (fun a -> List.map (fun b -> (a, b)) m) l

let ( >> ) f g x = g (f x)
let c ?(info = Metadata.default) blob = `Contents (blob, info)

let invalid_tree ~sw =
  let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
  let hash = Store.Hash.hash (fun f -> f "") in
  Tree.shallow repo (`Node hash)

let test_bindings () =
  let tree =
    Tree.of_concrete
      (`Tree [ ("aa", c "0"); ("ab", c "1"); ("a", c "2"); ("b", c "3") ])
  in
  let check_sorted =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" [ "a"; "aa"; "ab"; "b" ]
  in
  (* [Tree.list] returns all keys in lexicographic order *)
  Tree.list tree [] |> (List.map fst >> check_sorted)

let test_paginated_bindings () =
  let tree =
    Tree.of_concrete
      (`Tree
        [
          ("aa", c "0");
          ("a", c "1");
          ("bbb", c "3");
          ("b", c "3");
          ("aaa", c "2");
        ])
  in
  let check_sorted expected =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" expected
  in
  let () =
    Tree.list ~offset:0 ~length:2 tree []
    |> (List.map fst >> check_sorted [ "a"; "aa" ])
  in
  let () =
    Tree.list ~offset:2 ~length:3 tree []
    |> (List.map fst >> check_sorted [ "aaa"; "b"; "bbb" ])
  in
  let () =
    Tree.list ~offset:1 ~length:1 tree []
    |> (List.map fst >> check_sorted [ "aa" ])
  in
  let () =
    Tree.list ~offset:4 ~length:2 tree []
    |> (List.map fst >> check_sorted [ "bbb" ])
  in
  let () =
    Tree.list ~offset:5 ~length:2 tree [] |> (List.map fst >> check_sorted [])
  in
  ()

let tree bs = Tree.of_concrete (`Tree bs)

(** Basic tests of the [Tree.diff] operation. *)
let test_diff () =
  let empty = tree [] in
  let single = tree [ ("k", c "v") ] in

  (* Adding a single key *)
  let () =
    Tree.diff empty single
    |> Alcotest.(check diffs)
         "Added [k \226\134\146 v]"
         [ ([ "k" ], `Added ("v", Default)) ]
  in
  (* Removing a single key *)
  let () =
    Tree.diff single empty
    |> Alcotest.(check diffs)
         "Removed [k \226\134\146 v]"
         [ ([ "k" ], `Removed ("v", Default)) ]
  in
  (* Changing metadata *)
  Tree.diff
    (tree [ ("k", c ~info:Left "v") ])
    (tree [ ("k", c ~info:Right "v") ])
  |> Alcotest.(check diffs)
       "Changed metadata"
       [ ([ "k" ], `Updated (("v", Left), ("v", Right))) ]

let test_empty () =
  Eio.Switch.run @@ fun sw ->
  let () =
    Alcotest.check_tree_lwt "The empty tree is empty" ~expected:(`Tree [])
      (Tree.empty ())
  in

  (* Ensure that different [empty] values have disjoint cache state.

     This is a regression test for a bug in which all [Tree.empty] values had
     shared cache state and any keys obtained from [export] were discarded (to
     avoid sharing keys from different repositories). *)
  let () =
    let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
    let empty_exported = Tree.empty () and empty_not_exported = Tree.empty () in
    let () =
      Store.Backend.Repo.batch repo (fun c n _ ->
          Store.save_tree repo c n empty_exported |> ignore)
    in
    Alcotest.(check inspect)
      "The exported empty tree is now in Key form" (`Node `Key)
      (Tree.inspect empty_exported);
    Alcotest.(check inspect)
      "The non-exported empty tree should still be represented as a Map"
      (`Node `Map)
      (Tree.inspect empty_not_exported)
  in
  ()

let test_add () =
  let sample_tree ?(ab = "ab_v") ?ac () : Tree.concrete =
    let ac = match ac with Some ac -> [ ("ac", ac) ] | None -> [] in
    `Tree [ ("a", `Tree ([ ("aa", c "0"); ("ab", c ab) ] @ ac)); ("b", c "3") ]
  in

  let () =
    Alcotest.check_tree_lwt "Adding a root value to an empty tree"
      ~expected:(c "1")
      (Tree.add (Tree.empty ()) [] "1")
  in

  let () =
    let t = Tree.of_concrete (sample_tree ()) in
    let expected = sample_tree ~ab:"new_value" () in
    Alcotest.check_tree_lwt "Replacing an existing value in a tree" ~expected
      (Tree.add t [ "a"; "ab" ] "new_value")
  in

  let () =
    let t = Tree.of_concrete (sample_tree ()) in
    let expected = sample_tree ~ac:(`Tree [ ("aca", c "new_value") ]) () in
    Alcotest.check_tree_lwt
      "Adding at a non-existent path in a tree creates necessary intermediate \
       nodes"
      ~expected
      (Tree.add t [ "a"; "ac"; "aca" ] "new_value")
  in

  let () =
    let t = Tree.of_concrete (c "1") in
    let t' = Tree.add t [] "1" in
    Alcotest.assert_ "Re-adding a root value preserves physical equality"
      (t == t')
  in

  let () =
    let t = tree [ ("a", `Tree [ ("b", c "1") ]) ] in
    let t' = Tree.add t [ "a"; "b" ] "1" in
    Alcotest.assert_ "Re-adding a non-root value preserves physical equality"
      (t == t')
  in

  ()

let test_remove () =
  let tree =
    Tree.of_concrete
      (`Tree [ ("a", `Tree [ ("aa", c "0"); ("ab", c "1") ]); ("b", c "3") ])
  in

  let () =
    let t = Tree.empty () in
    let t' = Tree.remove t [] in
    Alcotest.assert_ "Removing in an empty tree preserves physical equality"
      (t == t')
  in

  let () =
    let tree' = Tree.remove tree [ "a"; "non"; "existent"; "path" ] in
    Alcotest.assert_
      "Removing at a non-existent path in a non-empty tree preserves physical \
       equality"
      (tree == tree')
  in

  let () =
    let tree = Tree.of_concrete (c "1") in
    let tree' = Tree.remove tree [ "a"; "non"; "existent"; "path" ] in
    Alcotest.assert_
      "Removing at a non-existent path in a root contents value preserves \
       physical equality"
      (tree == tree')
  in

  let () =
    Alcotest.check_tree_lwt
      "Removing a root contents value results in an empty root node."
      ~expected:(`Tree [])
      (Tree.remove (Tree.of_concrete (c "1")) [])
  in

  ()

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

let test_update () =
  Eio.Switch.run @@ fun sw ->
  let unrelated_binding = ("a_unrelated", c "<>") in
  let abc ?info v =
    `Tree
      [ ("a", `Tree [ ("b", `Tree [ ("c", c ?info v) ]) ]); unrelated_binding ]
  in
  let abc1 = Tree.of_concrete (abc "1") in
  let ( --> ) = transform_once [%typ: string option] in

  let () =
    Alcotest.check_tree_lwt
      "Changing the value of a root contents node results in a new contents \
       node."
      ~expected:(c "2")
      (Tree.update (Tree.of_concrete (c "1")) [] (Some "1" --> Some "2"))
  in

  let () =
    Alcotest.check_tree_lwt
      "Removing a root contents node results in an empty root node."
      ~expected:(`Tree [])
      (Tree.update (Tree.of_concrete (c "1")) [] (Some "1" --> None))
  in

  let () =
    Alcotest.check_tree_lwt
      "Updating a root node to a contents value removes all bindings and sets \
       the correct metadata."
      ~expected:(c ~info:Metadata.Right "2")
      (Tree.update ~metadata:Metadata.Right abc1 [] (None --> Some "2"))
  in

  let () =
    (* Replacing a root node with a dangling hash does not raise an
       exception. *)
    let invalid_tree = invalid_tree ~sw in
    Tree.update_tree abc1 [] (function
      | Some _ -> Some invalid_tree
      | None -> assert false)
    |> ignore
  in

  let () =
    Alcotest.check_tree_lwt
      "Updating at an existing contents path changes the contents value \
       appropriately."
      ~expected:(abc "2")
      (Tree.update abc1 [ "a"; "b"; "c" ] (Some "1" --> Some "2"))
  in

  let () =
    let s = "1" and s' = "1" ^ "" in
    assert (s != s');
    let abc1' = Tree.update abc1 [ "a"; "b"; "c" ] (Some s --> Some s') in
    Alcotest.assert_
      "Performing a no-op change to tree contents preserves physical equality"
      (abc1 == abc1')
  in

  let () =
    let abc1' =
      Tree.update_tree abc1 [ "a"; "b" ] (function
        | Some t -> Some t
        | None -> assert false)
    in
    Alcotest.assert_
      "Replacing a subtree node with a physically-equal one preserves physical \
       equality"
      (abc1 == abc1')
  in

  let () =
    Alcotest.check_tree_lwt
      "Changing the metadata of an existing contents value updates the tree."
      ~expected:(abc ~info:Metadata.Left "1")
      (Tree.update ~metadata:Metadata.Left abc1 [ "a"; "b"; "c" ]
         (Some "1" --> Some "1"))
  in

  let () =
    Alcotest.check_tree_lwt
      "Removing a siblingless contents value causes newly-empty directories to \
       be pruned."
      ~expected:(`Tree [ unrelated_binding ])
      (Tree.update abc1 [ "a"; "b"; "c" ] (Some "1" --> None))
  in

  let () =
    Alcotest.check_tree_lwt
      "Removing a siblingless node causes newly-empty directories to be pruned"
      ~expected:(`Tree [ unrelated_binding ])
      (Tree.update_tree abc1 [ "a"; "b" ] (function
        | Some _ -> None
        | None -> assert false))
  in

  let () =
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

  let () =
    Alcotest.check_tree_lwt
      "Updating at an existing node path replaces the subtree with the given \
       element."
      ~expected:
        (`Tree [ ("a", `Tree [ ("b", c "new_value") ]); unrelated_binding ])
      (Tree.update abc1 [ "a"; "b" ] (None --> Some "new_value"))
  in

  let () =
    Alcotest.check_tree_lwt
      "Updating at a path in an empty tree creates the necessary intermediate \
       nodes with the new contents."
      ~expected:(`Tree [ ("a", `Tree [ ("b", `Tree [ ("c", c "1") ]) ]) ])
      (Tree.update (Tree.empty ()) [ "a"; "b"; "c" ] (None --> Some "1"))
  in

  let () =
    let abc1' = Tree.update abc1 [ "a"; "b"; "c"; "d"; "e" ] (None --> None) in
    Alcotest.assert_
      "Removing at a non-existent path in a non-empty tree preserves physical \
       equality."
      (abc1 == abc1')
  in

  let () =
    let t = Tree.empty () in
    let t' = Tree.update t [] (None --> None) in
    Alcotest.assert_ "Removing from an empty tree preserves physical equality"
      (t == t')
  in

  let () =
    let abc1' =
      Tree.update_tree abc1 [ "a"; "b"; "d" ] (function
        | None -> Some (Tree.empty ())
        | Some _ -> assert false)
    in
    Alcotest.assert_
      "Adding an empty tree at an empty location preserves physical equality"
      (abc1 == abc1')
  in

  ()

(* Correct stats for a completely lazy tree *)
let lazy_stats = Tree.{ nodes = 0; leafs = 0; skips = 1; depth = 0; width = 0 }

(* Take a tree and persist it to some underlying store, making it lazy. *)
let persist_tree ?clear : Store.tree -> Store.tree =
 fun tree ->
  Fmt.pr "persist_tree@.";
  Eio.Switch.run @@ fun sw ->
  let store = Store.Repo.v ~sw (Irmin_mem.config ()) |> Store.empty in
  let () = Store.set_tree_exn ?clear ~info:Store.Info.none store [] tree in
  Store.tree store

type path = Store.Path.t [@@deriving irmin ~pp ~equal]

let test_clear () =
  (* 1. Build a tree *)
  let size = 830829 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> Tree.add acc [ i ] i) (Tree.empty ())
  in
  (* Check the state of the root and root/42 *)
  Alcotest.(check inspect) "Before clear, root" (`Node `Map) (Tree.inspect t);
  let () =
    Tree.stats ~force:false t
    |> Alcotest.(gcheck Tree.stats_t)
         "Before clear, root node is eagerly evaluated"
         { nodes = 1; leafs = size; skips = 0; depth = 1; width = size }
  in
  let entry42 = Tree.find_tree t [ "42" ] |> Option.get in
  Alcotest.(check inspect)
    "Before clear, root/42" `Contents (Tree.inspect entry42);
  let () =
    let dont_skip k = Alcotest.failf "should not have skipped %a" pp_path k in
    Tree.fold ~force:(`False dont_skip) entry42 ()
  in
  (* 2. Clear on non-persisted *)
  Tree.clear t;
  (* The state of the tree shouldn't have changed after this clear *)
  Alcotest.(check inspect) "Before persist" (`Node `Map) (Tree.inspect t);
  let () =
    Tree.stats ~force:false t
    |> Alcotest.(gcheck Tree.stats_t)
         "Before persist, root node is eagerly evaluated"
         { nodes = 1; leafs = size; skips = 0; depth = 1; width = size }
  in
  let entry42 = Tree.find_tree t [ "42" ] |> Option.get in
  Alcotest.(check inspect) "Before persist" `Contents (Tree.inspect entry42);
  let () =
    let dont_skip k = Alcotest.failf "should not have skipped %a" pp_path k in
    Tree.fold ~force:(`False dont_skip) entry42 ()
  in
  (* 3. Persist (and implicitly clear) *)
  let _ = persist_tree t in
  (* Check the state of the root *)
  Alcotest.(check inspect) "After persist+clear" (`Node `Key) (Tree.inspect t);
  let () =
    Tree.stats ~force:false t
    |> Alcotest.(gcheck Tree.stats_t)
         "After persist+clear, root node is no longer cached" lazy_stats
  in
  ()

let test_minimal_reads () =
  let persist_tree = persist_tree in
  (* 1. Build a tree *)
  let size = 10 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> Tree.add acc [ i ] i) (Tree.empty ())
  in

  (* Persist with no clear *)
  Tree.reset_counters ();
  let _ = persist_tree ~clear:false t in
  let _ = Tree.find t [ "0" ] in
  let cnt = Tree.counters () in
  Alcotest.(check int) "no reads" 0 cnt.node_find;

  (* Persist with clear *)
  Tree.reset_counters ();
  let _ = persist_tree ~clear:true t in
  Fmt.pr "Hello@.";
  let _ = Tree.find_tree t [ "0" ] in
  Fmt.pr "Hello@.";
  let cnt = Tree.counters () in
  Alcotest.(check int) "reads" 1 cnt.node_find

let with_binding k v t = Tree.add_tree t k v

let clear_and_assert_lazy tree =
  let _ = persist_tree tree in
  Tree.clear tree;
  Tree.stats ~force:false tree
  |> Alcotest.(gcheck Tree.stats_t)
       "Initially the tree is entirely lazy" lazy_stats

let test_fold_force () =
  Eio.Switch.run @@ fun sw ->
  let invalid_tree ~sw =
    let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
    let hash = Store.Hash.hash (fun f -> f "") in
    Tree.shallow repo (`Node hash)
  in

  (* Ensure that [fold] doesn't force a lazy tree when [~force:(`False f)],
     and that [f] is called the correct number of times. *)
  let () =
    let tree =
      Tree.singleton [ "existing"; "subtree" ] "value"
      |> with_binding [ "dangling"; "subtree"; "hash" ] (invalid_tree ~sw)
      |> with_binding [ "other"; "lazy"; "path" ] (invalid_tree ~sw)
    in
    let force = `False List.cons in
    Tree.fold ~force tree []
    |> Alcotest.(check (slist (list string) Stdlib.compare))
         "Unforced paths"
         [ [ "dangling"; "subtree"; "hash" ]; [ "other"; "lazy"; "path" ] ]
  in
  let create_sample_tree () =
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

  (* Ensure that [fold ~force:`True ~cache:true] forces all lazy trees. *)
  let () =
    let sample_tree = create_sample_tree () in
    let () = clear_and_assert_lazy sample_tree in
    Tree.fold ~force:`True ~cache:true sample_tree ();
    Tree.stats ~force:false sample_tree
    |> Alcotest.(gcheck Tree.stats_t)
         "After folding, the tree is eagerly evaluated" eager_stats
  in

  (* Ensure that [fold ~force:`True ~cache:false] visits all children and does
     not leave them cached. *)
  let () =
    let sample_tree = create_sample_tree () in
    clear_and_assert_lazy sample_tree;
    let contents =
      Tree.fold ~force:`True ~cache:false
        ~contents:(fun _ -> List.cons)
        sample_tree []
    in
    let () =
      Tree.stats ~force:false sample_tree
      |> Alcotest.(gcheck Tree.stats_t)
           "After folding, the tree is cleared" lazy_stats
    in
    Alcotest.(check (slist string compare))
      "During forced fold, all contents were traversed"
      [ "v-aa"; "v-ab"; "v-ac"; "v-b"; "v-c" ]
      contents
  in

  (* Ensure that [fold ~force:`True ~cache:false] visits newly added values and
     updated values only once and does not visit removed values. *)
  let () =
    let sample_tree = create_sample_tree () in
    let () = clear_and_assert_lazy sample_tree in
    Tree.remove sample_tree [ "a"; "ab" ] |> fun updated_tree ->
    Tree.add updated_tree [ "a"; "ad" ] "v-ad" |> fun updated_tree ->
    Tree.add updated_tree [ "a"; "ac" ] "v-acc" |> fun updated_tree ->
    let visited = ref [] in
    let contents k v () =
      if equal_path k [ "a"; "ab" ] then
        Alcotest.failf
          "Removed contents at %a should not be visited during fold" pp_path k;
      if equal_path k [ "a"; "ac" ] then
        if not (String.equal v "v-acc") then
          Alcotest.failf "Outdated contents at %a visited during fold" pp_path k;
      if List.mem ~equal:equal_path k !visited then
        Alcotest.failf "Visited node at %a twice during fold" pp_path k
      else visited := k :: !visited;
      ()
    in
    Tree.fold ~force:`True ~cache:false ~contents updated_tree ();
    Alcotest.(check bool)
      "Newly added contents visited"
      (List.mem ~equal:equal_path [ "a"; "ad" ] !visited)
      true
  in

  ()

(* Tests of "broken" trees: trees that can't be dereferenced. Tree currently
   supports two varieties of broken tree:

   - shallow trees containing [(repo, key)] pairs for which [repo] doesn't
     contain [key]. Attempted dereferences should raise [Dangling_hash].

   - pruned trees (hash-only tree nodes, with no underlying repository).
     Attempted dereferences should raise [Pruned_hash]. *)
module Broken = struct
  let shallow_of_ptr kinded_key =
    Eio.Switch.run @@ fun sw ->
    let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
    Tree.shallow repo kinded_key

  let pruned_of_ptr kinded_hash = Tree.pruned kinded_hash
  let random_string32 = Irmin.Type.(unstage (random (string_of (`Fixed 32))))

  let random_contents () =
    let value = Tree.of_concrete (c (random_string32 ())) in
    let value_ptr = `Contents (Tree.hash value, Metadata.default) in
    (value, value_ptr)

  let random_node () =
    let value = tree [ ("k", c (random_string32 ())) ] in
    let value_ptr = `Node (Tree.hash value) in
    (value, value_ptr)

  let test_hashes () =
    let&* leaf_type, (leaf, leaf_ptr) =
      [ ("contents", random_contents ()); ("node", random_node ()) ]
    and&* operation_name, operation =
      [ ("shallow", shallow_of_ptr); ("pruned", pruned_of_ptr) ]
    and&* path = [ []; [ "k" ] ] in
    let leaf_broken = operation leaf_ptr in
    let hash_actual = Tree.(add_tree (empty ())) path leaf |> Tree.hash in
    let hash_expected =
      Tree.(add_tree (empty ())) path leaf_broken |> Tree.hash
    in
    Alcotest.(gcheck Store.Hash.t)
      (Fmt.str
         "Hashing a %s %s value at path %a is equivalent to hashing the \
          non-broken %s"
         operation_name leaf_type
         Fmt.Dump.(list string)
         path leaf_type)
      hash_expected hash_actual

  let test_trees () =
    let run_tests ~exn_type ~broken_contents ~broken_node ~path =
      [%logs.app
        "Testing operations on a tree with a broken position at %a" pp_path path];
      let broken_leaf = Tree.(add_tree (empty ())) path broken_contents in
      let broken_node = Tree.(add_tree (empty ())) path broken_node in
      let beneath = path @ [ "a"; "b"; "c" ] in
      let blob = "v" and node = tree [ ("k", c "v") ] in
      let add_blob_or_node path =
        [
          (fun tr -> Tree.(add tr path blob));
          (fun tr -> Tree.(add_tree tr path node));
        ]
      in

      (* [add] on broken nodes/contents replaces the broken position. *)
      let () =
        let&* broken = [ broken_leaf; broken_node ]
        and&* add = add_blob_or_node path in
        let expected = add (Tree.empty ()) |> Tree.to_concrete in
        Alcotest.check_tree_lwt ~__POS__ "" ~expected (add broken)
      in

      (* [add] _beneath_ a broken contents value also works fine, but on broken
         nodes an exception is raised. (We can't know what the node's contents are,
         so there's no valid return tree.) *)
      let () =
        let&* add_beneath = add_blob_or_node beneath in
        let expected = add_beneath (Tree.empty ()) |> Tree.to_concrete in
        Alcotest.check_tree_lwt ~__POS__ "" ~expected (add_beneath broken_leaf)
      in
      let () =
        let&* add_beneath = add_blob_or_node beneath in
        check_exn_lwt ~exn_type __POS__ (fun () -> add_beneath broken_node)
      in

      (* [find] on broken contents raises an exception (can't recover contents),
         but _beneath_ broken contents it returns [None] (mismatched type). (The
         behaviour is reversed for broken nodes.) *)
      let () =
        check_exn_lwt ~exn_type __POS__ (fun () -> Tree.find broken_leaf path)
      in
      let () =
        check_exn_lwt ~exn_type __POS__ (fun () ->
            Tree.find broken_node beneath)
      in
      let () =
        Tree.find broken_leaf beneath
        |> Alcotest.(check ~pos:__POS__ (option reject)) "" None
      in
      let () =
        Store.Tree.find broken_node path
        |> Alcotest.(check ~pos:__POS__ (option reject)) "" None
      in

      (* [list] on (or beneath) broken contents returns the empty list, but on
         (or beneath) broken nodes an exception is raised. *)
      let () =
        let&* path = [ path; beneath ] in
        Tree.list broken_leaf path
        |> Alcotest.(check ~pos:__POS__ (list reject)) "" []
      in
      let () =
        let&* path = [ path; beneath ] in
        check_exn_lwt ~exn_type __POS__ (fun () -> Tree.list broken_node path)
      in
      ()
    in
    let&* path = [ []; [ "k" ] ]
    and&* exn_type, tree_of_ptr =
      [ (`Dangling_hash, shallow_of_ptr); (`Pruned_hash, pruned_of_ptr) ]
    in
    let broken_contents = tree_of_ptr (snd (random_contents ())) in
    let broken_node = tree_of_ptr (snd (random_node ())) in
    run_tests ~exn_type ~broken_contents ~broken_node ~path

  let test_pruned_fold () =
    Eio.Switch.run @@ fun sw ->
    let&* _, ptr = [ random_contents (); random_node () ]
    and&* path = [ []; [ "k" ] ] in
    let tree = Tree.(add_tree (empty ())) path (Tree.pruned ptr) in

    (* Folding over a pruned tree with [force:`True] should fail: *)
    let () =
      check_exn_lwt ~exn_type:`Pruned_hash __POS__ (fun () ->
          Tree.fold ~force:`True tree ())
    in

    (* But folding with [force:`False] should not: *)
    let () = Tree.fold ~force:(`False (fun _ -> Fun.id)) tree () in

    (* Similarly, attempting to export a pruned tree should fail: *)
    let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
    check_exn_lwt ~exn_type:`Pruned_hash __POS__ (fun () ->
        Store.Backend.Repo.batch repo (fun c n _ ->
            Store.save_tree repo c n tree |> ignore))
end

let test_kind_empty_path () =
  let cont = c "c" |> Tree.of_concrete in
  let tree = `Tree [ ("k", c "c") ] |> Tree.of_concrete in
  let k = Tree.kind cont [] in
  Alcotest.(check (option (gtestable kind_t)))
    "Kind of empty path in content" (Some `Contents) k;
  let k = Tree.kind tree [] in
  Alcotest.(check (option (gtestable kind_t)))
    "Kind of empty path in tree" (Some `Node) k

let test_generic_equality () =
  (* Regression test for a bug in which the equality derived from [tree_t] did
     not respect equivalences between in-memory trees and lazy trees. *)
  let tree = persist_tree (tree [ ("k", c "v") ]) in
  let should_be_empty = Tree.remove tree [ "k" ] in
  Alcotest.(gcheck Store.tree_t)
    "Modified empty tree is equal to [(Tree.empty ())]" (Tree.empty ())
    should_be_empty

let test_is_empty () =
  (* Test for equivalence against an [is_equal] derived from generic equality,
     for backwards compatibility. *)
  let is_empty =
    let equal = Type.unstage (Type.equal Store.tree_t) in
    fun t ->
      let reference = equal t (Tree.empty ()) in
      let candidate = Tree.is_empty t in
      Alcotest.(check bool)
        "`equal (Tree.empty ())` agrees with `is_empty`" reference candidate;
      candidate
  in
  let kv = tree [ ("k", c "v") ] in
  let () = Alcotest.(check bool) "empty tree" true (is_empty (Tree.empty ())) in
  let () = Alcotest.(check bool) "non-empty tree" false (is_empty kv) in
  let () =
    let tree = Tree.remove kv [ "k" ] in
    Alcotest.(check bool) "emptied tree" true (is_empty tree)
  in
  Eio.Switch.run @@ fun sw ->
  let repo = Store.Repo.v ~sw (Irmin_mem.config ()) in
  let () =
    let shallow_empty = Tree.(shallow repo (`Node (hash (empty ())))) in
    Alcotest.(check bool) "shallow empty tree" true (is_empty shallow_empty)
  in
  let () =
    let shallow_empty = Tree.(shallow repo (`Node (hash kv))) in
    Alcotest.(check bool)
      "shallow non-empty tree" false (is_empty shallow_empty)
  in
  ()

let test_of_concrete () =
  let () =
    let aa = ("aa", c "aa-v") in
    let ac = ("ac", c "ac-v") in
    let input = tree [ ("a", `Tree [ aa; ("ab", `Tree []); ac ]) ] in
    let pruned = `Tree [ ("a", `Tree [ aa; ac ]) ] in
    Alcotest.check_tree_lwt "Empty subtrees are pruned" ~expected:pruned
      (Tree.to_concrete input |> Tree.of_concrete)
  in

  let () =
    Alcotest.check_raises "Tree with duplicate bindings is rejected"
      (Invalid_argument "of_concrete: duplicate bindings for step `k`")
      (fun () ->
        ignore (Tree.of_concrete (`Tree [ ("k", c "v1"); ("k", c "v2") ])))
  in

  ()

let suite =
  [
    Alcotest.test_case "bindings" `Quick test_bindings;
    Alcotest.test_case "paginated bindings" `Quick test_paginated_bindings;
    Alcotest.test_case "diff" `Quick test_diff;
    Alcotest.test_case "empty" `Quick test_empty;
    Alcotest.test_case "add" `Quick test_add;
    Alcotest.test_case "remove" `Quick test_remove;
    Alcotest.test_case "update" `Quick test_update;
    Alcotest.test_case "clear" `Quick test_clear;
    Alcotest.test_case "minimal_reads" `Quick test_minimal_reads;
    Alcotest.test_case "fold" `Quick test_fold_force;
    Alcotest.test_case "Broken.hashes" `Quick Broken.test_hashes;
    Alcotest.test_case "Broken.trees" `Quick Broken.test_trees;
    Alcotest.test_case "Broken.pruned_fold" `Quick Broken.test_pruned_fold;
    Alcotest.test_case "kind of empty path" `Quick test_kind_empty_path;
    Alcotest.test_case "generic equality" `Quick test_generic_equality;
    Alcotest.test_case "is_empty" `Quick test_is_empty;
    Alcotest.test_case "of_concrete" `Quick test_of_concrete;
  ]
