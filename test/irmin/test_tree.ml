open Lwt.Infix
module Store = Irmin_mem.KV (Irmin.Contents.String)
module Tree = Store.Tree

let ( >> ) f g x = g (f x)

let test_bindings _ () =
  let tree =
    Tree.of_concrete
      (`Tree
        [
          ("aa", `Contents ("0", ()));
          ("ab", `Contents ("1", ()));
          ("a", `Tree []);
          ("b", `Contents ("3", ()));
        ])
  in
  let check_sorted =
    Alcotest.(check (list string))
      "Bindings are reported in lexicographic order" [ "a"; "aa"; "ab"; "b" ]
  in
  (* [Tree.list] *)
  Tree.list tree [] >|= (List.map fst >> check_sorted) >>= fun () ->
  (* [Tree.Node.bindings] *)
  Tree.destruct tree |> function
  | `Contents _ -> Alcotest.fail "Received `Contents but expected `Node"
  | `Node n -> Tree.Node.bindings n >|= (List.map fst >> check_sorted)

let suite = [ Alcotest_lwt.test_case "bindings" `Quick test_bindings ]
