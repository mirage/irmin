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

open! Import
open Common

module Make (S : Generic_key) = struct
  include Common.Make_helpers (S)

  let test_iter x () =
    let test repo =
      let pp_id = Irmin.Type.pp S.Tree.kinded_key_t in
      let eq_id = Irmin.Type.(unstage (equal S.Tree.kinded_key_t)) in
      let mem k ls = List.exists (fun k' -> eq_id k k') ls in
      let visited = ref [] in
      let skipped = ref [] in
      let rev_order oldest k =
        if !visited = [] && not (eq_id k oldest) then
          Alcotest.fail "traversal should start with oldest node"
      in
      let in_order oldest k =
        if !visited = [] && eq_id k oldest then
          Alcotest.fail "traversal shouldn't start with oldest node"
      in
      let node k =
        if mem (`Node k) !visited then
          Alcotest.failf "node %a visited twice" (Irmin.Type.pp B.Node.Key.t) k;
        visited := `Node k :: !visited;
        Lwt.return_unit
      in
      let contents ?order k =
        let e = `Contents (k, S.Metadata.default) in
        if mem e !visited then
          Alcotest.failf "contents %a visited twice"
            (Irmin.Type.pp B.Contents.Key.t)
            k;
        (match order with None -> () | Some f -> f e);
        visited := e :: !visited;
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
              Alcotest.failf "%a should be visited"
                (Irmin.Type.pp S.Tree.kinded_key_t)
                k)
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
              Alcotest.failf "%a should be visited" pp_id k)
          nodes
      in
      let test_skip ~max ~to_skip ~not_visited =
        let skip_node k =
          if mem (`Node k) to_skip then (
            skipped := `Node k :: !skipped;
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
              Alcotest.failf "%a should be skipped" pp_id k)
          to_skip;
        List.iter
          (fun k ->
            if mem k !visited || mem k !skipped then
              Alcotest.failf "%a should not be skipped nor visited" pp_id k)
          not_visited
      in
      let test_min_max ~nodes ~min ~max ~not_visited =
        Graph.iter (g repo) ~min ~max ~node ~contents ~rev:false ()
        >|= fun () ->
        List.iter
          (fun k ->
            if mem k not_visited && mem k !visited then
              Alcotest.failf "%a should not be visited" pp_id k;
            if (not (mem k not_visited)) && not (mem k !visited) then
              Alcotest.failf "%a should not be visited" pp_id k)
          nodes
      in
      let test1 () =
        let* foo = with_contents repo (fun c -> B.Contents.add c "foo") in
        let foo_k = (foo, S.Metadata.default) in
        let* k1 = with_node repo (fun g -> Graph.v g [ ("b", normal foo) ]) in
        let* k2 = with_node repo (fun g -> Graph.v g [ ("a", `Node k1) ]) in
        let* k3 = with_node repo (fun g -> Graph.v g [ ("c", `Node k1) ]) in
        let nodes = [ `Contents foo_k; `Node k1; `Node k2; `Node k3 ] in
        visited := [];
        test_rev_order ~nodes ~max:[ k2; k3 ] >>= fun () ->
        visited := [];
        test_in_order ~nodes ~max:[ k2; k3 ] >>= fun () ->
        visited := [];
        skipped := [];
        test_skip ~max:[ k2; k3 ] ~to_skip:[ `Node k1 ] ~not_visited:[]
        >>= fun () ->
        visited := [];
        let* () =
          test_min_max ~nodes ~min:[ k1 ] ~max:[ k2 ]
            ~not_visited:[ `Contents foo_k; `Node k3 ]
        in
        visited := [];
        test_min_max ~nodes ~min:[ k2; k3 ] ~max:[ k2; k3 ]
          ~not_visited:[ `Contents foo_k; `Node k1 ]
      in
      let test2 () =
        (* Graph.iter requires a node as max, we cannot test a graph with only
           contents. *)
        let* foo = with_contents repo (fun c -> B.Contents.add c "foo") in
        let foo_k = (foo, S.Metadata.default) in
        let* k1 = with_node repo (fun g -> Graph.v g [ ("b", normal foo) ]) in
        visited := [];
        test_rev_order ~nodes:[ `Contents foo_k; `Node k1 ] ~max:[ k1 ]
        >>= fun () ->
        visited := [];
        skipped := [];
        test_skip ~max:[ k1 ]
          ~to_skip:[ `Node k1 ]
          ~not_visited:[ `Contents foo_k ]
      in
      let test3 () =
        let* foo = with_contents repo (fun c -> B.Contents.add c "foo") in
        let foo_k = (foo, S.Metadata.default) in
        let* kb1 = with_node repo (fun g -> Graph.v g [ ("b1", normal foo) ]) in
        let* ka1 = with_node repo (fun g -> Graph.v g [ ("a1", `Node kb1) ]) in
        let* ka2 = with_node repo (fun g -> Graph.v g [ ("a2", `Node kb1) ]) in
        let* kb2 = with_node repo (fun g -> Graph.v g [ ("b2", normal foo) ]) in
        let* kc =
          with_node repo (fun g ->
              Graph.v g
                [ ("c1", `Node ka1); ("c2", `Node ka2); ("c3", `Node kb2) ])
        in
        let nodes =
          [
            `Contents foo_k;
            `Node kb1;
            `Node ka1;
            `Node ka2;
            `Node kb2;
            `Node kc;
          ]
        in
        visited := [];
        test_rev_order ~nodes ~max:[ kc ] >>= fun () ->
        visited := [];
        test_in_order ~nodes ~max:[ kc ] >>= fun () ->
        visited := [];
        skipped := [];
        let* () =
          test_skip ~max:[ kc ]
            ~to_skip:[ `Node ka1; `Node ka2 ]
            ~not_visited:[ `Node kb1 ]
        in
        visited := [];
        skipped := [];
        let* () =
          test_skip ~max:[ kc ]
            ~to_skip:[ `Node ka1; `Node ka2; `Node kb2 ]
            ~not_visited:[ `Node kb1; `Contents foo_k ]
        in
        visited := [];
        let* () =
          test_min_max ~nodes ~min:[ kb1 ] ~max:[ ka1 ]
            ~not_visited:[ `Contents foo_k; `Node ka2; `Node kb2; `Node kc ]
        in
        visited := [];
        test_min_max ~nodes ~min:[ kc ] ~max:[ kc ]
          ~not_visited:
            [ `Contents foo_k; `Node kb1; `Node ka1; `Node ka2; `Node kb2 ]
      in
      test1 () >>= fun () ->
      test2 () >>= fun () ->
      test3 () >>= fun () -> B.Repo.close repo
    in
    run x test

  let tests = [ ("Iter", test_iter) ]
end
