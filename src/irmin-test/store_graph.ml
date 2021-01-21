open! Import
open Common

module Make (S : S) = struct
  include Common.Make_helpers (S)

  let test_iter x () =
    let test repo =
      let eq = Irmin.Type.(unstage (equal P.Hash.t)) in
      let mem k ls = List.exists (fun k' -> eq k k') ls in
      let visited = ref [] in
      let skipped = ref [] in
      let rev_order oldest k =
        if !visited = [] && not (eq k oldest) then
          Alcotest.fail "traversal should start with oldest node"
      in
      let in_order oldest k =
        if !visited = [] && eq k oldest then
          Alcotest.fail "traversal shouldn't start with oldest node"
      in
      let node k =
        if mem k !visited then
          Alcotest.failf "node %a visited twice" (Irmin.Type.pp P.Hash.t) k;
        visited := k :: !visited;
        Lwt.return_unit
      in
      let contents ?order k =
        if mem k !visited then
          Alcotest.failf "contents %a visited twice" (Irmin.Type.pp P.Hash.t) k;
        (match order with None -> () | Some f -> f k);
        visited := k :: !visited;
        Lwt.return_unit
      in
      let test_rev_order ~nodes ~max =
        let oldest = List.hd nodes in
        let contents = contents ~order:(rev_order oldest) in
        let+ () =
          Graph.iter (g repo) ~min:[] ~max ~node ~contents ~rev:true ()
        in
        List.iter
          (fun k ->
            if not (mem k !visited) then
              Alcotest.failf "%a should be visited" (Irmin.Type.pp P.Hash.t) k)
          nodes
      in
      let test_in_order ~nodes ~max =
        let oldest = List.hd nodes in
        let contents = contents ~order:(in_order oldest) in
        let+ () =
          Graph.iter (g repo) ~min:[] ~max ~node ~contents ~rev:false ()
        in
        List.iter
          (fun k ->
            if not (mem k !visited) then
              Alcotest.failf "%a should be visited" (Irmin.Type.pp P.Hash.t) k)
          nodes
      in
      let test_skip ~max ~to_skip ~not_visited =
        let skip_node k =
          if mem k to_skip then (
            skipped := k :: !skipped;
            Lwt.return_true)
          else Lwt.return_false
        in
        let+ () =
          Graph.iter (g repo) ~min:[] ~max ~node ~contents ~skip_node ~rev:false
            ()
        in
        List.iter
          (fun k ->
            if mem k !visited || not (mem k !skipped) then
              Alcotest.failf "%a should be skipped" (Irmin.Type.pp P.Hash.t) k)
          to_skip;
        List.iter
          (fun k ->
            if mem k !visited || mem k !skipped then
              Alcotest.failf "%a should not be skipped nor visited"
                (Irmin.Type.pp P.Hash.t) k)
          not_visited
      in
      let test_min_max ~nodes ~min ~max ~not_visited =
        Graph.iter (g repo) ~min ~max ~node ~contents ~rev:false ()
        >|= fun () ->
        List.iter
          (fun k ->
            if mem k not_visited && mem k !visited then
              Alcotest.failf "%a should not be visited" (Irmin.Type.pp P.Hash.t)
                k;
            if (not (mem k not_visited)) && not (mem k !visited) then
              Alcotest.failf "%a should not be visited" (Irmin.Type.pp P.Hash.t)
                k)
          nodes
      in
      let test1 () =
        let foo = P.Contents.Key.hash "foo" in
        let* k1 = with_node repo (fun g -> Graph.v g [ ("b", normal foo) ]) in
        let* k2 = with_node repo (fun g -> Graph.v g [ ("a", `Node k1) ]) in
        let* k3 = with_node repo (fun g -> Graph.v g [ ("c", `Node k1) ]) in
        let nodes = [ foo; k1; k2; k3 ] in
        visited := [];
        test_rev_order ~nodes ~max:[ k2; k3 ] >>= fun () ->
        visited := [];
        test_in_order ~nodes ~max:[ k2; k3 ] >>= fun () ->
        visited := [];
        skipped := [];
        test_skip ~max:[ k2; k3 ] ~to_skip:[ k1 ] ~not_visited:[] >>= fun () ->
        visited := [];
        let* () =
          test_min_max ~nodes ~min:[ k1 ] ~max:[ k2 ] ~not_visited:[ foo; k3 ]
        in
        visited := [];
        test_min_max ~nodes ~min:[ k2; k3 ] ~max:[ k2; k3 ]
          ~not_visited:[ foo; k1 ]
      in
      let test2 () =
        (* Graph.iter requires a node as max, we cannot test a graph with only
           contents. *)
        let foo = P.Contents.Key.hash "foo" in
        let* k1 = with_node repo (fun g -> Graph.v g [ ("b", normal foo) ]) in
        visited := [];
        test_rev_order ~nodes:[ foo; k1 ] ~max:[ k1 ] >>= fun () ->
        visited := [];
        skipped := [];
        test_skip ~max:[ k1 ] ~to_skip:[ k1 ] ~not_visited:[ foo ]
      in
      let test3 () =
        let foo = P.Contents.Key.hash "foo" in
        let* kb1 = with_node repo (fun g -> Graph.v g [ ("b1", normal foo) ]) in
        let* ka1 = with_node repo (fun g -> Graph.v g [ ("a1", `Node kb1) ]) in
        let* ka2 = with_node repo (fun g -> Graph.v g [ ("a2", `Node kb1) ]) in
        let* kb2 = with_node repo (fun g -> Graph.v g [ ("b2", normal foo) ]) in
        let* kc =
          with_node repo (fun g ->
              Graph.v g
                [ ("c1", `Node ka1); ("c2", `Node ka2); ("c3", `Node kb2) ])
        in
        let nodes = [ foo; kb1; ka1; ka2; kb2; kc ] in
        visited := [];
        test_rev_order ~nodes ~max:[ kc ] >>= fun () ->
        visited := [];
        test_in_order ~nodes ~max:[ kc ] >>= fun () ->
        visited := [];
        skipped := [];
        let* () =
          test_skip ~max:[ kc ] ~to_skip:[ ka1; ka2 ] ~not_visited:[ kb1 ]
        in
        visited := [];
        skipped := [];
        let* () =
          test_skip ~max:[ kc ] ~to_skip:[ ka1; ka2; kb2 ]
            ~not_visited:[ kb1; foo ]
        in
        visited := [];
        let* () =
          test_min_max ~nodes ~min:[ kb1 ] ~max:[ ka1 ]
            ~not_visited:[ foo; ka2; kb2; kc ]
        in
        visited := [];
        test_min_max ~nodes ~min:[ kc ] ~max:[ kc ]
          ~not_visited:[ foo; kb1; ka1; ka2; kb2 ]
      in
      test1 () >>= fun () ->
      test2 () >>= fun () ->
      test3 () >>= fun () -> P.Repo.close repo
    in
    run x test

  let tests = [ ("Iter", test_iter) ]
end
