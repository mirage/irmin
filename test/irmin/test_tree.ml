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

module Alcotest = struct
  include Alcotest

  let diffs = testable (Type.pp diffs_t) (Type.equal diffs_t)
end

let ( >> ) f g x = g (f x)

let get_ok = function
  | Ok x -> x
  | Error (`Dangling_hash _) -> Alcotest.fail "Unexpected dangling hash"

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
  Tree.list tree [] >|= (List.map fst >> check_sorted) >>= fun () ->
  (* [Tree.Node.bindings] returns all bindings in lexicographic order *)
  Tree.destruct tree |> function
  | `Contents _ -> Alcotest.fail "Received `Contents but expected `Node"
  | `Node n -> Tree.Node.bindings n >|= (get_ok >> List.map fst >> check_sorted)

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

let suite =
  [
    Alcotest_lwt.test_case "bindings" `Quick test_bindings;
    Alcotest_lwt.test_case "diff" `Quick test_diff;
  ]
