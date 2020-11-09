open Lwt.Infix
open Lwt.Syntax
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

module Alcotest = struct
  include Alcotest

  let diffs = testable (Type.pp diffs_t) (Type.equal diffs_t)

  let check_lwt typ msg expect = Lwt.map (Alcotest.check typ msg expect)
end

(* Generators for random test data *)
module Faker = struct
  let () = Random.self_init ()

  let string () =
    let rand_char _ =
      match Random.int 16 with
      | n when n < 10 -> Char.chr (n + 48)
      | n -> Char.chr (n + 65)
    in
    String.init 8 rand_char

  let contents () = string ()

  let rec tree ?(depth = 3) ?(branching_factor = 4) () =
    match depth with
    | 0 -> Lwt.return (Tree.of_contents (contents ()))
    | _ ->
        let rec aux = function
          | 0 -> Lwt.return Tree.empty
          | n ->
              let* children = aux (n - 1) in
              let* new_child = tree ~depth:(depth - 1) ~branching_factor () in
              Tree.add_tree children [ string () ] new_child
        in
        aux branching_factor
end

let ( >> ) f g x = g (f x)

let get_ok = function
  | Ok x -> x
  | Error (`Dangling_hash _) -> Alcotest.fail "Unexpected dangling hash"

let c ?(info = Metadata.Default) blob = `Contents (blob, info)

let n x = `Tree x

let test_bindings _ () =
  let tree =
    Tree.of_concrete
      (`Tree [ ("aa", c "0"); ("ab", c "1"); ("a", n []); ("b", c "3") ])
  in
  let check_sorted =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" [ "a"; "aa"; "ab"; "b" ]
  in
  (* [Tree.list] returns all keys in lexicographic order *)
  Tree.list tree [] >|= (List.map fst >> check_sorted) >>= fun () ->
  (* [Tree.Node.bindings] returns all bindings in lexicographic order *)
  Tree.destruct tree |> function
  | `Contents _ -> Alcotest.fail "Received `Contents but expected `Node"
  | `Node n -> Tree.Node.bindings n >|= (get_ok >> List.map fst >> check_sorted)

(** Basic tests of the [Tree.diff] operation. *)
let test_diff _ () =
  let tree = n >> Tree.of_concrete in
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

type kind = [ `Contents of Store.metadata | `Node ] [@@deriving irmin]

type binding = kind * Store.Hash.t [@@deriving irmin]

type error =
  [ `Dangling_hash of Store.Hash.t
  | `Invalid_path
  | `Different_hashes of Store.Tree.Proof.path_diff ]
[@@deriving irmin]

let ok x = Alcotest.(result x reject)

let proof_result =
  Alcotest.(
    result reject
      (testable (Irmin.Type.pp_dump error_t) (Irmin.Type.equal error_t)))

let verify_result = Irmin.Type.(result unit) error_t

module Proof = struct
  let tree = n >> Tree.of_concrete

  let test_non_existent_path _ () =
    let t = tree [ ("a", c "1") ] in
    let* () =
      Alcotest.(check_lwt proof_result)
        "cannot construct proof for a non-existent path"
        (Error `Invalid_path)
        (Tree.Proof.of_path t [ "a"; "not_present" ])
    in
    Lwt.return ()

  let test_verify_path _ () =
    Alcotest.(check_lwt (ok unit))
      "Empty proof is valid for the empty tree" (Ok ())
      Tree.(Proof.(verify_on_path empty) empty [])

  let test_basic _ () =
    let t1, t1', t2 =
      let f dist =
        tree
          [
            ("alpha", c "1");
            ("gamma", `Tree []);
            ("delta", c "3");
            ("differing", n [ ("path", n [ (dist, c "0") ]) ]);
          ]
      in
      (f "one", f "one", f "two")
    in
    let* () =
      Alcotest.(check_lwt (ok unit))
        "Empty proof is valid for the empty tree" (Ok ())
        Tree.(Proof.(verify empty) empty)
    in
    let* () =
      let* tree = Faker.tree () in
      Alcotest.(check_lwt (ok unit))
        "Empty proof is valid for a random tree" (Ok ())
        Tree.(Proof.(verify empty tree))
    in
    let* proof_alpha = Tree.Proof.of_path t1 [ "alpha" ] >|= Result.get_ok in
    let* () =
      Alcotest.(check_lwt (ok unit))
        "Content proof is verifiable on the tree from which it was constructed"
        (Ok ())
        (Tree.Proof.verify proof_alpha t1)
    in
    let* () =
      Alcotest.(check_lwt (ok unit))
        "Content proof is verifiable on a tree that is equal to the one from \
         which it was constructed"
        (Ok ())
        (Tree.Proof.verify proof_alpha t1')
    in
    let* () =
      Tree.Proof.verify proof_alpha t2 >|= function
      | Error
          (`Different_hashes
            {
              Tree.Proof.path = [ "differing" ];
              computed = `Node, h1;
              required = `Node, h2;
            })
        when not (Irmin.Type.equal Store.Hash.t h1 h2) ->
          ()
      | r ->
          Alcotest.failf
            "Content proof is not verifiable on a different tree containing \
             the same path: %a"
            (Irmin.Type.pp_dump verify_result)
            r
    in
    Lwt.return ()
end

let suite =
  let tc n f = Alcotest_lwt.test_case n `Quick f in
  [
    tc "bindings" test_bindings;
    tc "diff" test_diff;
    tc "Proof.test_basic" Proof.test_basic;
    tc "Proof.test_non_existent_path" Proof.test_non_existent_path;
    tc "Proof.test_verify_path" Proof.test_verify_path;
  ]
