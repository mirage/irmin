open Lwt.Infix
open Common

module Make (S : S) = struct
  include Common.Make_helpers (S)

  let test_iter x () =
    let test repo =
      let n = n repo in
      let check_key = check P.Node.Key.t in
      let check_val = check Graph.value_t in
      let foo = normal (P.Contents.Key.hash "foo") in
      let v = P.Node.Val.add P.Node.Val.empty "b" foo in
      with_node repo (fun n -> P.Node.add n v) >>= fun k1 ->
      with_node repo (fun g -> Graph.v g [ ("a", `Node k1) ]) >>= fun k2 ->
      with_node repo (fun g -> Graph.v g [ ("c", `Node k1) ]) >>= fun k3 ->
      let visited = ref [] in
      let check_mem order_test step h1 h2 v1 v2 =
        check_key "find key" h1 h2;
        check_val "find value" v1 v2;
        if List.mem step !visited then
          Alcotest.failf "node %a visited twice" (Irmin.Type.pp P.Hash.t) h1;
        order_test step;
        visited := step :: !visited
      in
      let mem hash value check_node =
        match P.Node.Val.find value "a" with
        | Some k -> check_node "a" hash k2 k (`Node k1)
        | None -> (
            match P.Node.Val.find value "c" with
            | Some k -> check_node "c" hash k3 k (`Node k1)
            | None -> (
                match P.Node.Val.find value "b" with
                | Some k -> check_node "b" hash k1 k foo
                | None -> Alcotest.fail "unexpected node"))
      in
      let rev_order step =
        if !visited = [] && step <> "b" then
          Alcotest.fail "traversal should start with oldest node"
      in
      let node k =
        P.Node.find n k >|= fun t -> mem k (Option.get t) (check_mem rev_order)
      in
      Graph.iter (g repo) ~min:[] ~max:[ k2; k3 ] ~node ~rev:true ()
      >>= fun () ->
      let inorder step =
        if !visited = [] && step = "b" then
          Alcotest.fail "traversal should start with newest nodes"
      in
      let node k =
        P.Node.find n k >|= fun t -> mem k (Option.get t) (check_mem inorder)
      in
      let skipped = ref [] in
      let check_skip step h1 _ _ _ =
        if List.mem step !skipped then
          Alcotest.failf "node %a skipped twice" (Irmin.Type.pp P.Hash.t) h1;
        skipped := step :: !skipped
      in
      let skip_node k =
        P.Node.find n k >|= fun t ->
        mem k (Option.get t) check_skip;
        if List.mem "b" !skipped then true else false
      in
      visited := [];
      Graph.iter (g repo) ~min:[] ~max:[ k2; k3 ] ~node ~skip_node ~rev:false ()
      >>= fun () ->
      if List.mem "b" !visited then Alcotest.fail "b should be skipped";
      visited := [];
      let node k =
        P.Node.find n k >|= fun t -> mem k (Option.get t) (check_mem inorder)
      in
      Graph.iter (g repo) ~min:[ k1 ] ~max:[ k2 ] ~node ~rev:false ()
      >>= fun () ->
      if not (List.mem "b" !visited) then
        Alcotest.fail "k1 should have been visited";
      if List.mem "c" !visited then
        Alcotest.fail "k3 shouldn't have been visited";
      visited := [];
      Graph.iter (g repo) ~min:[ k2; k3 ] ~max:[ k2; k3 ] ~node ~rev:false ()
      >>= fun () ->
      if (not (List.mem "a" !visited)) || not (List.mem "c" !visited) then
        Alcotest.fail "k1, k2 should have been visited";
      P.Repo.close repo
    in
    run x test

  let tests = [ ("Iter", test_iter) ]
end
