open Lwt.Infix
open Common

let src = Logs.Src.create "test" ~doc:"Irmin layered tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Make_Layered (S : LAYERED_STORE) = struct
  module P = S.Private
  module Graph = Irmin.Private.Node.Graph (P.Node)
  module History = Irmin.Private.Commit.History (P.Commit)

  let v1 = "X1"

  let v2 = "X2"

  let v3 = "X3"

  let b1 = "foo"

  let b2 = "bar/toto"

  let with_contents repo f = P.Repo.batch repo (fun t _ _ -> f t)

  let with_node repo f = P.Repo.batch repo (fun _ t _ -> f t)

  let with_commit repo f = P.Repo.batch repo (fun _ _ t -> f t)

  let with_info repo n f = with_commit repo (fun h -> f h ~info:(info n))

  let normal x = `Contents (x, S.Metadata.default)

  let h repo = P.Repo.commit_t repo

  let n repo = P.Repo.node_t repo

  let n1 ~repo =
    with_contents repo (fun t -> P.Contents.add t v1) >>= fun kv1 ->
    with_node repo (fun t -> Graph.v t [ ("x", normal kv1) ]) >>= fun kn1 ->
    with_node repo (fun t -> Graph.v t [ ("b", `Node kn1) ])

  let r1 ~repo =
    n1 ~repo >>= fun kn2 ->
    S.Tree.of_hash repo kn2 >>= function
    | None -> Alcotest.fail "r1"
    | Some tree -> S.Commit.v repo ~info:(info "r1") ~parents:[] (tree :> S.tree)

  let r2 ~repo =
    n1 ~repo >>= fun kn2 ->
    with_node repo (fun t -> Graph.v t [ ("a", `Node kn2) ]) >>= fun kn3 ->
    r1 ~repo >>= fun kr1 ->
    S.Tree.of_hash repo kn3 >>= function
    | None -> Alcotest.fail "r2"
    | Some t3 ->
        S.Commit.v repo ~info:(info "r2") ~parents:[ S.Commit.hash kr1 ]
          (t3 :> S.tree)

  let run x test =
    try
      Lwt_main.run
        ( x.init () >>= fun () ->
          S.Repo.v x.config >>= fun repo -> test repo >>= x.clean )
    with e ->
      Lwt_main.run (x.clean ());
      raise e

  let fail_with_none f msg =
    f >>= function None -> Alcotest.fail msg | Some c -> Lwt.return c

  (* add nodes (from Private.Graph), commits (from Private.History)
           commits:kr2 ---> kr1
                    |        |
            nodes :kt3 -b-> kt2 -a-> kt1 -x-> kv1
     freeze kr1; find nodes and commits; check that kr2 is deleted;
     reconstruct node kt3 and commit again kr2; freeze kr2 and check again*)
  let test_graph_and_history x () =
    let test repo =
      with_contents repo (fun t -> P.Contents.add t v1) >>= fun kv1 ->
      let check_val = check (T.option P.Commit.Val.t) in
      let check_key = check P.Commit.Key.t in
      let check_keys = checks P.Commit.Key.t in
      with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) >>= fun kt1 ->
      with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) >>= fun kt2 ->
      with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) >>= fun kt3 ->
      with_info repo "commit kt2" (History.v ~node:kt2 ~parents:[])
      >>= fun (kr1, _) ->
      with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[ kr1 ])
      >>= fun (kr2, _) ->
      P.Commit.find (h repo) kr1 >>= fun t1 ->
      fail_with_none (S.Commit.of_hash repo kr1) "of_hash commit"
      >>= fun commit ->
      S.freeze repo ~max:[ commit ] >>= fun () ->
      P.Commit.find (h repo) kr1 >>= fun t1' ->
      check_val "value of kr1 before and after freeze" t1 t1';
      Graph.find (n repo) kt2 [ "a" ] >>= fun kt1' ->
      check (T.option Graph.value_t) "kt2 -a-> kt1 before and after freeze"
        (Some (`Node kt1))
        kt1';
      Graph.find (n repo) kt3 [ "b" ] >>= fun kt2' ->
      if not (S.async_freeze ()) then
        check (T.option Graph.value_t) "kt3 -b-> kt2 deleted by freeze" None
          kt2';
      with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) >>= fun kt3 ->
      with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[ kr1 ])
      >>= fun (kr2', _) ->
      check_key "commit kr2 after freeze" kr2 kr2';
      History.closure (h repo) ~min:[] ~max:[ kr1 ] >>= fun kr1s ->
      check_keys "closure over lower" [ kr1 ] kr1s;
      History.closure (h repo) ~min:[] ~max:[ kr2 ] >>= fun kr2s ->
      check_keys "closure over upper and lower" [ kr1; kr2 ] kr2s;
      P.Commit.find (h repo) kr2' >>= fun t2 ->
      fail_with_none (S.Commit.of_hash repo kr2') "of_hash commit"
      >>= fun commit ->
      S.freeze repo ~max:[ commit ] >>= fun () ->
      P.Commit.find (h repo) kr1 >>= fun t1' ->
      check_val "value of kr1 before and after snd freeze" t1 t1';
      P.Commit.find (h repo) kr2 >>= fun t2' ->
      check_val "value of kr2 before and after snd freeze" t2 t2';
      Graph.find (n repo) kt2 [ "a" ] >>= fun kt1' ->
      check (T.option Graph.value_t) "kt2 -a-> kt1 before and after snd freeze"
        (Some (`Node kt1))
        kt1';
      Graph.find (n repo) kt3 [ "b" ] >>= fun kt2' ->
      check (T.option Graph.value_t) "kt3 -b-> kt2 before and after snd freeze"
        (Some (`Node kt2))
        kt2';
      S.Repo.close repo
    in
    run x test

  (* test trees, trees from commits, temporary stores from commits *)
  let test_gc x () =
    let info = info "gc" in
    let check_val = check (T.option P.Commit.Val.t) in
    (*
      -> c0 -> c1 -> c2 |freeze|
                 \-> c3
    *)
    let tree1 repo =
      let tree = S.Tree.empty in
      S.Tree.add tree [ "c"; "b"; "a" ] "x" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[] tree >>= fun c0 ->
      S.Tree.add tree [ "c"; "b"; "a1" ] "x1" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c0 ] tree >>= fun c1 ->
      S.Tree.add tree [ "c"; "b"; "a2" ] "x2" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c1 ] tree >>= fun c2 ->
      S.Tree.add tree [ "c"; "b"; "a3" ] "x3" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c1 ] tree >>= fun c3 ->
      Lwt.return (c2, c3)
    in
    (*
     \->  c5 -> c6 -> c7 |freeze|
     \->  c4      \-> c8
    *)
    let tree2 repo =
      let tree = S.Tree.empty in
      S.Tree.add tree [ "c"; "b1" ] "x4" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[] tree >>= fun c4 ->
      S.Tree.add tree [ "c"; "b2" ] "x5" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[] tree >>= fun c5 ->
      S.Tree.add tree [ "c"; "b1"; "a" ] "x6" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c5 ] tree >>= fun c6 ->
      S.Tree.add tree [ "c"; "e" ] "x7" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c6 ] tree >>= fun c7 ->
      S.Tree.add tree [ "c"; "d" ] "x8" >>= fun tree ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c6 ] tree >>= fun c8 ->
      Lwt.return (c4, c5, c7, c8)
    in
    let test repo =
      tree1 repo >>= fun (c2, c3) ->
      P.Commit.find (h repo) (S.Commit.hash c2) >>= fun t2 ->
      S.freeze repo ~max:[ c2 ] >>= fun () ->
      P.Commit.find (h repo) (S.Commit.hash c2) >>= fun t2' ->
      check_val "c2" t2 t2';
      let tree = S.Commit.tree c2 in
      S.Tree.find tree [ "c"; "b"; "a1" ] >>= fun x1 ->
      Alcotest.(check (option string)) "x1" (Some "x1") x1;
      S.of_commit c2 >>= fun a ->
      S.tree a >>= fun tree_a ->
      S.Tree.find tree_a [ "c"; "b"; "a2" ] >>= fun x2 ->
      Alcotest.(check (option string)) "x2" (Some "x2") x2;
      ( if not (S.async_freeze ()) then (
        P.Commit.find (h repo) (S.Commit.hash c3) >>= fun t3 ->
        check_val "c3" None t3;
        S.Tree.find tree [ "c"; "b"; "a3" ] >>= fun x3 ->
        Alcotest.(check (option string)) "x3" None x3;
        S.Tree.find tree_a [ "c"; "b"; "a3" ] >|= fun x3 ->
        Alcotest.(check (option string)) "x3" None x3 )
      else Lwt.return_unit )
      >>= fun () ->
      tree2 repo >>= fun (c4, c5, c7, c8) ->
      P.Commit.find (h repo) (S.Commit.hash c5) >>= fun t5 ->
      P.Commit.find (h repo) (S.Commit.hash c7) >>= fun t7 ->
      S.freeze repo ~max:[ c7 ] >>= fun () ->
      P.Commit.find (h repo) (S.Commit.hash c5) >>= fun t5' ->
      check_val "c5" t5 t5';
      P.Commit.find (h repo) (S.Commit.hash c7) >>= fun t7' ->
      check_val "c7" t7 t7';
      fail_with_none (S.Commit.of_hash repo (S.Commit.hash c7)) "of_hash commit"
      >>= fun c7 ->
      let tree = S.Commit.tree c7 in
      S.Tree.find tree [ "c"; "e" ] >>= fun x7 ->
      Alcotest.(check (option string)) "x7" (Some "x7") x7;
      ( if not (S.async_freeze ()) then (
        P.Commit.find (h repo) (S.Commit.hash c4) >>= fun t4 ->
        check_val "c4" None t4;
        P.Commit.find (h repo) (S.Commit.hash c8) >>= fun t8 ->
        check_val "c8" None t8;
        S.Commit.of_hash repo (S.Commit.hash c8) >>= function
        | None -> Lwt.return_unit
        | Some _ -> Alcotest.fail "should not find c8" )
      else Lwt.return_unit )
      >>= fun () -> S.Repo.close repo
    in
    run x test

  (* branches that point to deleted commits are deleted as well *)
  let test_fail_branch x () =
    let check_val repo = check (T.option @@ S.commit_t repo) in
    let test repo =
      r1 ~repo >>= fun kv1 ->
      r2 ~repo >>= fun kv2 ->
      S.Branch.set repo b1 kv1 >>= fun () ->
      S.Branch.set repo b2 kv2 >>= fun () ->
      S.freeze repo ~max:[ kv1 ] >>= fun () ->
      S.Branch.find repo b1 >>= fun k1' ->
      check_val repo "r1 after freeze" (Some kv1) k1';
      S.Branch.find repo b2 >>= fun k2' ->
      if not (S.async_freeze ()) then
        check_val repo "r2 deleted by freeze" None k2';
      S.Repo.close repo
    in
    run x test

  let test_set x () =
    let check_list = checks T.(pair S.Key.step_t S.kind_t) in
    let check_parents = checks S.Hash.t in
    let test repo =
      S.master repo >>= fun t ->
      S.set_exn t [ "a"; "b"; "c" ] v1 ~info:(infof "commit 1") >>= fun () ->
      S.Head.get t >>= fun c1 ->
      S.freeze repo ~max:[ c1 ] >>= fun () ->
      S.set_exn t [ "a"; "d" ] v2 ~info:(infof "commit 2") >>= fun () ->
      S.list t [ "a" ] >>= fun ks ->
      (* list sees a merged tree from lower and upper layers *)
      check_list "path" [ ("d", `Contents); ("b", `Node) ] ks;
      ( if not (S.async_freeze ()) then
        S.layer_id repo (S.Commit_t (S.Commit.hash c1)) >|= fun s ->
        Alcotest.(check string) "layer id of commit 1" s "lower"
      else Lwt.return_unit )
      >>= fun () ->
      S.Head.get t >>= fun c2 ->
      S.layer_id repo (S.Commit_t (S.Commit.hash c2)) >>= fun s ->
      Alcotest.(check string) "layer_id commit 2" s "upper0";
      let parents = S.Commit.parents c2 in
      check_parents "parents of c2" [ S.Commit.hash c1 ] parents;
      S.get t [ "a"; "b"; "c" ] >>= fun s ->
      (* read value from lower layers *)
      Alcotest.(check string) "commit 1" s v1;
      S.set_exn t [ "a"; "b"; "c" ] "Hello" ~info:(infof "commit 3")
      >>= fun () ->
      S.get t [ "a"; "b"; "c" ] >>= fun s ->
      (* updated value on upper layers, hides lower layer *)
      Alcotest.(check string) "commit 3" s "Hello";
      S.Repo.close repo
    in
    run x test

  (* test set tree, add and remove to a tree; test get head from lower*)
  let test_set_tree x () =
    let fail_with_some f msg =
      f >>= function None -> Lwt.return_unit | Some _ -> Alcotest.fail msg
    in
    let test repo =
      S.master repo >>= fun t ->
      S.Tree.add S.Tree.empty [ "a"; "d" ] v1 >>= fun t1 ->
      S.Tree.add t1 [ "a"; "b"; "c" ] v2 >>= fun t1 ->
      S.set_tree_exn ~info:(infof "commit 1") ~parents:[] t [] t1 >>= fun () ->
      S.freeze repo >>= fun () ->
      (* get head from lower *)
      S.Head.get t >>= fun c1 ->
      let t1 = S.Commit.tree c1 in
      fail_with_none (S.Tree.find t1 [ "a"; "d" ]) "find in t1" >>= fun v ->
      Alcotest.(check string) "t1" v v1;
      fail_with_none (S.Tree.find t1 [ "a"; "b"; "c" ]) "find in t1"
      >>= fun v ->
      Alcotest.(check string) "t1" v v2;
      S.Tree.remove t1 [ "a"; "b"; "c" ] >>= fun t2 ->
      S.set_tree_exn ~info:(infof "commit 2") ~parents:[ c1 ] t [] t2
      >>= fun () ->
      S.get_tree t [] >>= fun t3 ->
      fail_with_some (S.Tree.find t3 [ "a"; "b"; "c" ]) "found after remove"
      >>= fun () ->
      fail_with_none (S.Tree.find t3 [ "a"; "d" ]) "find in t3" >>= fun v ->
      Alcotest.(check string) "t3" v v1;
      S.Repo.branches repo >>= Lwt_list.iter_p (S.Branch.remove repo)
      >>= fun () -> S.Repo.close repo
    in
    run x test

  (* use branch deleted by freeze in merges *)
  let test_merge_unrelated x () =
    let test1 repo =
      S.of_branch repo "foo" >>= fun foo ->
      S.of_branch repo "bar" >>= fun bar ->
      S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1 >>= fun () ->
      S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1 >>= fun () ->
      S.Head.get foo >>= fun c ->
      S.freeze repo ~max:[ c ] >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      Lwt.catch
        (fun () ->
          S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo
          >|= fun _ -> Alcotest.fail "bar should point to a deleted commit")
        (function
          | Invalid_argument msg ->
              if msg = "merge_with_branch: bar is not a valid branch ID" then
                P.Repo.close repo
              else Lwt.fail (Failure msg)
          | exn -> Lwt.fail exn)
    in
    let test2 repo =
      S.of_branch repo "foo" >>= fun foo ->
      S.of_branch repo "bar" >>= fun bar ->
      S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1 >>= fun () ->
      S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1 >>= fun () ->
      S.Head.get bar >>= fun c ->
      S.freeze repo ~max:[ c ] >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo
      >>= merge_exn "merge unrelated"
      >>= fun () ->
      S.get bar [ "b" ] >>= fun v1' ->
      check S.contents_t "v1" v1 v1';
      Lwt.catch
        (fun () ->
          S.get foo [ "a" ] >|= fun _ ->
          Alcotest.fail "foo should point to a deleted commit")
        (function
          | Invalid_argument msg ->
              if msg = "Irmin.Tree.get: /a not found" then P.Repo.close repo
              else Lwt.fail (Failure msg)
          | exn -> Lwt.fail exn)
    in
    run x test1;
    run x test2

  (* TODO check size of the store before and after freeze *)
  let test_squash x () =
    let check_val = check T.(option S.contents_t) in
    let test repo =
      S.master repo >>= fun t ->
      S.Tree.reset_counters ();
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] v1 >>= fun () ->
      S.Head.get t >>= fun c ->
      S.get_tree t [ "x" ] >>= fun tree ->
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree >>= fun () ->
      S.Head.get t >>= fun c1 ->
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] v2 >>= fun () ->
      S.Head.get t >>= fun c2 ->
      S.Tree.add tree [ "x"; "z" ] v1 >>= fun tree3 ->
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree3 >>= fun () ->
      S.Head.get t >>= fun c3 ->
      S.tree t >>= fun tree3 ->
      S.Tree.stats ~force:true tree3 >>= fun s ->
      Log.debug (fun l ->
          l
            "stats before freeze nodes = %d, leafs = %d, depth = %d, width = \
             %d, skips = %d"
            s.nodes s.leafs s.depth s.width s.skips);
      Log.debug (fun l -> l "counters = %a" S.Tree.dump_counters ());
      S.freeze repo ~squash:true >>= fun () ->
      S.tree t >>= fun tree ->
      S.set_tree_exn t ~info:(infof "update") [] tree >>= fun () ->
      S.Tree.stats ~force:true tree >>= fun s ->
      Log.debug (fun l ->
          l
            "stats after freeze nodes = %d, leafs = %d, depth = %d, width = \
             %d, skips = %d"
            s.nodes s.leafs s.depth s.width s.skips);
      Log.debug (fun l -> l "counters = %a" S.Tree.dump_counters ());
      ( if not (S.async_freeze ()) then
        (S.Commit.of_hash repo (S.Commit.hash c) >>= function
         | None -> Lwt.return_unit
         | Some _ -> Alcotest.fail "should not find c")
        >>= fun () ->
        (S.Commit.of_hash repo (S.Commit.hash c1) >>= function
         | None -> Lwt.return_unit
         | Some _ -> Alcotest.fail "should not find c1")
        >>= fun () ->
        (S.Commit.of_hash repo (S.Commit.hash c2) >>= function
         | None -> Lwt.return_unit
         | Some _ -> Alcotest.fail "should not find c2")
        >>= fun () ->
        S.Commit.of_hash repo (S.Commit.hash c3) >>= function
        | None -> Alcotest.fail "should not find c3"
        | Some _ -> Lwt.return_unit
      else Lwt.return_unit )
      >>= fun () ->
      S.find t [ "x"; "y"; "z" ] >>= fun v1' ->
      check_val "x/y/z after merge" (Some v1) v1';
      S.find t [ "u"; "x"; "z" ] >>= fun v1' ->
      check_val "u/x/z after merge" (Some v1) v1';
      S.find t [ "u"; "y"; "z" ] >>= fun v1' ->
      check_val "u/y/z after merge" (Some v1) v1';
      ( if not (S.async_freeze ()) then
        S.find t [ "u"; "x"; "y" ] >|= fun v2' ->
        check_val "vy after merge" None v2'
      else Lwt.return_unit )
      >>= fun () -> P.Repo.close repo
    in
    run x test

  let test_branch_squash x () =
    let check_val = check T.(option S.contents_t) in
    let setup repo =
      S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v1 >>= fun tree1 ->
      S.Tree.add S.Tree.empty [ "a"; "b"; "d" ] v2 >>= fun tree2 ->
      S.of_branch repo "foo" >>= fun foo ->
      S.set_tree_exn ~parents:[] ~info:(infof "tree1") foo [] tree1
      >>= fun () ->
      S.Head.get foo >>= fun c1 ->
      S.master repo >>= fun t ->
      S.set_tree_exn ~parents:[] ~info:(infof "tree2") t [] tree2 >>= fun () ->
      S.Head.get t >|= fun c2 -> (c1, c2)
    in
    let test repo =
      setup repo >>= fun (c1, c2) ->
      S.freeze repo ~squash:true >>= fun () ->
      (P.Commit.mem (P.Repo.commit_t repo) (S.Commit.hash c1) >>= function
       | true ->
           if not (S.async_freeze ()) then
             S.layer_id repo (S.Commit_t (S.Commit.hash c1)) >|= fun s ->
             Alcotest.(check string) "layer_id commit 1" s "lower"
           else Lwt.return_unit
       | false ->
           Alcotest.failf "did not copy commit %a to dst" S.Commit.pp_hash c1)
      >>= fun () ->
      (P.Commit.mem (P.Repo.commit_t repo) (S.Commit.hash c2) >>= function
       | true ->
           if not (S.async_freeze ()) then
             S.layer_id repo (S.Commit_t (S.Commit.hash c2)) >|= fun s ->
             Alcotest.(check string) "layer_id commit 2" s "lower"
           else Lwt.return_unit
       | false ->
           Alcotest.failf "did not copy commit %a to dst" S.Commit.pp_hash c2)
      >>= fun () ->
      S.of_branch repo "foo" >>= fun foo ->
      S.find foo [ "a"; "b"; "c" ] >>= fun v1' ->
      check_val "copy v1 to dst" (Some v1) v1';
      S.master repo >>= fun t ->
      S.find t [ "a"; "b"; "d" ] >>= fun v2' ->
      check_val "copy v2 to dst" (Some v2) v2';
      P.Repo.close repo
    in
    let test_squash repo =
      setup repo >>= fun (c1, c2) ->
      S.freeze repo ~squash:true ~max:[ c2 ] >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      (P.Commit.mem (P.Repo.commit_t repo) (S.Commit.hash c1) >|= function
       | true ->
           Alcotest.failf "should not copy commit %a to dst" S.Commit.pp_hash c1
       | false -> ())
      >>= fun () ->
      (P.Commit.mem (P.Repo.commit_t repo) (S.Commit.hash c2) >>= function
       | true ->
           S.layer_id repo (S.Commit_t (S.Commit.hash c2)) >|= fun s ->
           Alcotest.(check string) "layer_id commit 2" s "lower"
       | false ->
           Alcotest.failf "should copy commit %a to dst" S.Commit.pp_hash c2)
      >>= fun () ->
      S.master repo >>= fun t ->
      S.find t [ "a"; "b"; "d" ] >>= fun v2' ->
      check_val "copy v2 to dst" (Some v2) v2';
      ( S.find t [ "a"; "b"; "c" ] >>= fun v1' ->
        check_val "copy v1 to dst" None v1';
        S.Branch.find repo "foo" >|= function
        | None -> ()
        | Some _ -> Alcotest.failf "should not find branch foo in dst" )
      >>= fun () -> P.Repo.close repo
    in
    run x test;
    run x test_squash

  let test_consecutive_freeze x () =
    let test repo =
      let info = info "freezes" in
      let check_val repo = check (T.option @@ S.commit_t repo) in
      let tree = S.Tree.empty in
      let rec commits tree acc i =
        if i = 10 then Lwt.return acc
        else
          let a = "a" ^ string_of_int i in
          let y = "y" ^ string_of_int i in
          let b = "b" ^ string_of_int i in
          S.Tree.add tree [ "c"; "b"; a ] y >>= fun tree ->
          Log.debug (fun l -> l "commit %s" y);
          S.Commit.v repo ~info ~parents:[] tree >>= fun c ->
          S.Branch.set repo b c >>= fun () ->
          S.freeze repo ~max:[ c ] >>= fun () ->
          S.PrivateLayer.wait_for_freeze () >>= fun () ->
          commits tree (c :: acc) (i + 1)
      in
      let tests c i =
        let tree = S.Commit.tree c in
        let a = "a" ^ string_of_int i in
        let y = "y" ^ string_of_int i in
        let b = "b" ^ string_of_int i in
        Log.debug (fun l -> l "test %s" y);
        S.Tree.find tree [ "c"; "b"; a ] >>= fun y' ->
        Alcotest.(check (option string)) "commit" (Some y) y';
        ( if not (S.async_freeze ()) then
          S.layer_id repo (S.Commit_t (S.Commit.hash c)) >|= fun s ->
          Alcotest.(check string) "layer id of commit" s "lower"
        else Lwt.return_unit )
        >>= fun () ->
        S.Branch.find repo b >|= fun c' -> check_val repo "branch" (Some c) c'
      in
      commits tree [] 0 >>= fun commits ->
      Lwt_list.iteri_s (fun i c -> tests c i) (List.rev commits) >>= fun () ->
      S.Repo.close repo
    in
    run x test

  let test_keep_max x () =
    let test repo =
      with_contents repo (fun t -> P.Contents.add t v1) >>= fun kv1 ->
      with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) >>= fun kt1 ->
      with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) >>= fun kt2 ->
      with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) >>= fun kt3 ->
      with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[])
      >>= fun (kr1, _) ->
      let test_commit1 name_commit name =
        S.layer_id repo (S.Commit_t kr1) >>= fun s ->
        Alcotest.(check string) "layer id of commit 1" s name_commit;
        S.layer_id repo (S.Node_t kt1) >>= fun s ->
        Alcotest.(check string) "layer id of node kt1" s name;
        S.layer_id repo (S.Node_t kt2) >>= fun s ->
        Alcotest.(check string) "layer id of node kt2" s name;
        S.layer_id repo (S.Node_t kt3) >>= fun s ->
        Alcotest.(check string) "layer id of node kt3" s name;
        S.layer_id repo (S.Content_t kv1) >|= fun s ->
        Alcotest.(check string) "layer id of node kv1" s name
      in
      (* Test that commit c1 and all its objects are preserved in upper after a
         freeze. *)
      fail_with_none (S.Commit.of_hash repo kr1) "of_hash commit" >>= fun c1 ->
      S.freeze repo ~max:[ c1 ] ~keep_max:true >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      test_commit1 "upper0" "upper0" >>= fun () ->
      with_contents repo (fun t -> P.Contents.add t v2) >>= fun kv2 ->
      with_node repo (fun g -> Graph.v g [ ("d", normal kv2) ]) >>= fun kt1' ->
      with_node repo (fun g -> Graph.v g [ ("a", `Node kt1') ]) >>= fun kt2' ->
      with_info repo "commit kt2" (History.v ~node:kt2' ~parents:[ kr1 ])
      >>= fun (kr2, _) ->
      let test_commit2 name =
        S.layer_id repo (S.Commit_t kr2) >>= fun s ->
        Alcotest.(check string) "layer id of commit 2" s name;
        S.layer_id repo (S.Node_t kt1') >>= fun s ->
        Alcotest.(check string) "layer id of node kt1" s name;
        S.layer_id repo (S.Node_t kt2') >>= fun s ->
        Alcotest.(check string) "layer id of node kt2" s name;
        S.layer_id repo (S.Content_t kv2) >|= fun s ->
        Alcotest.(check string) "layer id of node kv2" s name
      in
      (* Test that commit c2 and all its objects are preserved in upper after a
         freeze. *)
      fail_with_none (S.Commit.of_hash repo kr2) "of_hash commit" >>= fun c2 ->
      S.freeze repo ~max:[ c2 ] ~keep_max:true >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      test_commit1 "lower" "lower" >>= fun () ->
      test_commit2 "upper1" >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_keep_max_set x () =
    let check_layer msg s t =
      let s = if s = "upper1" || s = "upper1" then "upper" else s in
      Alcotest.(check string) msg s t
    in
    let test repo =
      S.of_branch repo "foo" >>= fun foo ->
      S.set_exn foo [ "a"; "b"; "c" ] v1 ~info:(infof "commit 1") >>= fun () ->
      S.Head.get foo >>= fun c1 ->
      (* Test that contents of commit c1 is preserved in upper during and after
         a freeze. *)
      S.freeze repo ~max:[ c1 ] ~keep_max:true >>= fun () ->
      fail_with_none (S.hash foo [ "a"; "b"; "c" ]) "hash of v1'" >>= fun hv1 ->
      S.layer_id repo (S.Content_t hv1) >>= fun s ->
      check_layer "layer id of v1" s "upper";
      S.set_exn foo [ "a"; "d" ] v2 ~info:(infof "commit 2") >>= fun () ->
      S.Head.get foo >>= fun c2 ->
      let hc2 = S.Commit.hash c2 in
      S.layer_id repo (S.Commit_t hc2) >>= fun s ->
      check_layer "layer_id commit 2" s "upper";
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      S.layer_id repo (S.Content_t hv1) >>= fun s ->
      check_layer "layer id of v1" s "upper";
      (* Test that commit c2 and all its objects are preserved in upper during
         and after a freeze. *)
      S.freeze repo ~max:[ c2 ] ~keep_max:true >>= fun () ->
      S.layer_id repo (S.Commit_t hc2) >>= fun s ->
      check_layer "layer_id commit 2" s "upper";
      S.layer_id repo (S.Content_t hv1) >>= fun s ->
      check_layer "layer id of v1" s "upper";
      fail_with_none (S.hash foo [ "a"; "d" ]) "hash of v2'" >>= fun hv2 ->
      S.layer_id repo (S.Content_t hv2) >>= fun s ->
      check_layer "layer id of v2" s "upper";
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      let hc1 = S.Commit.hash c1 in
      S.layer_id repo (S.Commit_t hc1) >>= fun s ->
      check_layer "layer_id commit 1" s "lower";
      S.layer_id repo (S.Commit_t hc2) >>= fun s ->
      check_layer "layer_id commit 2" s "upper";
      S.layer_id repo (S.Content_t hv1) >>= fun s ->
      check_layer "layer id of v1" s "upper";
      S.layer_id repo (S.Content_t hv2) >>= fun s ->
      check_layer "layer id of v2" s "upper";
      S.Repo.close repo
    in
    run x test

  (* c0 <- c1 <- c2 "b2"
              \- c3 <- c4
                    \- c5 "b5"
                    \- c6 "b6" *)
  let test_keep_heads x () =
    let check_layer msg s t =
      let s = if s = "upper1" || s = "upper1" then "upper" else s in
      Alcotest.(check string) msg s t
    in
    let info = info "keep_heads" in
    let check_commit = check (T.option P.Commit.Val.t) in
    let check_branch repo = check (T.option @@ S.commit_t repo) in
    let test repo =
      let tree = S.Tree.empty in
      let rec setup i tree commits contents =
        if i > 6 then Lwt.return (commits, List.rev contents)
        else
          let c = "c" ^ string_of_int i in
          let x = "x" ^ string_of_int i in
          let b = "b" ^ string_of_int i in
          with_contents repo (fun t -> P.Contents.add t x) >>= fun hx ->
          let contents = hx :: contents in
          S.Tree.add tree [ "a"; "b"; c ] x >>= fun tree' ->
          match i with
          | 0 ->
              S.Commit.v repo ~info ~parents:[] tree' >>= fun c0 ->
              setup (i + 1) tree' [ c0 ] contents
          | 1 ->
              S.Commit.v repo ~info
                ~parents:[ S.Commit.hash (List.nth commits 0) ]
                tree'
              >>= fun c1 -> setup (i + 1) tree' (commits @ [ c1 ]) contents
          | 2 ->
              S.Commit.v repo ~info
                ~parents:[ S.Commit.hash (List.nth commits 1) ]
                tree'
              >>= fun c ->
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree (commits @ [ c ]) contents
          | 3 ->
              S.Commit.v repo ~info
                ~parents:[ S.Commit.hash (List.nth commits 1) ]
                tree'
              >>= fun c ->
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree' (commits @ [ c ]) contents
          | 4 ->
              S.Commit.v repo ~info
                ~parents:[ S.Commit.hash (List.nth commits 3) ]
                tree'
              >>= fun c -> setup (i + 1) tree (commits @ [ c ]) contents
          | 5 | 6 ->
              S.Commit.v repo ~info
                ~parents:[ S.Commit.hash (List.nth commits 3) ]
                tree'
              >>= fun c ->
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree (commits @ [ c ]) contents
          | _ -> assert false
      in
      setup 0 tree [] [] >>= fun (commits, contents) ->
      S.freeze repo ~min:[ List.nth commits 1 ] ~max:[ List.nth commits 3 ]
        ~keep_max:true
      >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      S.freeze repo ~min:[ List.nth commits 1 ] ~max:[ List.nth commits 3 ]
        ~keep_max:true
      >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      Lwt_list.iter_p
        (fun i ->
          P.Commit.find (h repo) (S.Commit.hash (List.nth commits i))
          >|= fun t -> check_commit "deleted commit" None t)
        [ 0; 2; 4 ]
      >>= fun () ->
      S.Branch.find repo "b2" >>= fun c2' ->
      check_branch repo "b2" None c2';
      S.Branch.find repo "b3" >>= fun c3' ->
      check_branch repo "b3" (Some (List.nth commits 3)) c3';
      S.Branch.find repo "b5" >>= fun c5' ->
      check_branch repo "b5" (Some (List.nth commits 5)) c5';
      S.Branch.find repo "b6" >>= fun c6' ->
      check_branch repo "b6" (Some (List.nth commits 6)) c6';
      S.layer_id repo (S.Commit_t (S.Commit.hash (List.nth commits 1)))
      >>= fun s ->
      check_layer "layer id of c1" s "lower";
      Lwt_list.iter_p
        (fun i ->
          S.layer_id repo (S.Commit_t (S.Commit.hash (List.nth commits i)))
          >|= fun s -> check_layer ("layer id of c" ^ string_of_int i) s "upper")
        [ 3; 5; 6 ]
      >>= fun () ->
      S.of_branch repo "b6" >>= fun t ->
      S.get_tree t [] >>= fun tree ->
      fail_with_none (S.Tree.find tree [ "a"; "b"; "c0" ]) "find x0"
      >>= fun s ->
      Alcotest.(check string) "x0" s "x0";
      fail_with_none (S.Tree.find tree [ "a"; "b"; "c1" ]) "find x1"
      >>= fun s ->
      Alcotest.(check string) "x1" s "x1";
      Lwt_list.iter_p
        (fun i ->
          S.layer_id repo (S.Content_t (List.nth contents i)) >|= fun s ->
          check_layer ("layer id of x" ^ string_of_int i) s "upper")
        [ 0; 1; 3 ]
      >>= fun () ->
      Lwt.catch
        (fun () ->
          S.layer_id repo (S.Content_t (List.nth contents 2)) >|= fun _ ->
          Alcotest.fail "Should not find x2")
        (function Not_found -> Lwt.return_unit | exn -> Lwt.fail exn)
      >>= fun () ->
      Lwt.catch
        (fun () ->
          S.layer_id repo (S.Content_t (List.nth contents 4)) >|= fun _ ->
          Alcotest.fail "Should not found x4")
        (function Not_found -> Lwt.return_unit | exn -> Lwt.fail exn)
      >>= fun () ->
      Lwt_list.iter_p
        (fun i ->
          S.layer_id repo (S.Content_t (List.nth contents i)) >|= fun s ->
          check_layer ("layer id of x" ^ string_of_int i) s "upper")
        [ 3; 5; 6 ]
      >>= fun () -> S.Repo.close repo
    in
    run x test

  module Hook = S.PrivateLayer.Hook

  let after_copy f = Hook.v (function `After_Copy -> f | _ -> Lwt.return_unit)

  let after_postcopy f =
    Hook.v (function `After_PostCopy -> f | _ -> Lwt.return_unit)

  let after f =
    Hook.v (function
      | `After_PostCopy | `After_Copy -> f
      | _ -> Lwt.return_unit)

  let before f = Hook.v (function `Before -> f | _ -> Lwt.return_unit)

  let test_find_during_freeze x () =
    let info = info "hooks" in
    let test repo =
      let find_commit c v =
        S.Commit.of_hash repo (S.Commit.hash c) >>= function
        | None -> Alcotest.fail "no hash found in repo"
        | Some commit ->
            let tree = S.Commit.tree commit in
            S.Tree.find tree [ "a"; "b"; "c" ] >|= fun v' ->
            Alcotest.(check (option string)) "v1" v' (Some v)
      in
      S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v1 >>= fun tree1 ->
      S.Commit.v repo ~info ~parents:[] tree1 >>= fun c1 ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(after (find_commit c1 v1))
        ~max:[ c1 ] ~keep_max:true repo
      >>= fun () ->
      find_commit c1 v1 >>= fun () ->
      S.Tree.add tree1 [ "a"; "b"; "c" ] v2 >>= fun tree2 ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c1 ] tree2 >>= fun c2 ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(before (find_commit c2 v2))
        ~max:[ c2 ] ~keep_max:true repo
      >>= fun () ->
      find_commit c2 v2 >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      S.Tree.add tree1 [ "a"; "b"; "c" ] v3 >>= fun tree3 ->
      S.Commit.v repo ~info ~parents:[ S.Commit.hash c2 ] tree3 >>= fun c3 ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(after (find_commit c3 v3))
        ~max:[ c3 ] ~keep_max:true repo
      >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_add_during_freeze x () =
    let test repo =
      let find_commit t v =
        S.Head.get t >>= fun c ->
        S.Commit.of_hash repo (S.Commit.hash c) >>= function
        | None -> Alcotest.fail "no hash found in repo"
        | Some commit ->
            let tree = S.Commit.tree commit in
            S.Tree.find tree [ "a"; "b"; "c" ] >|= fun v' ->
            Alcotest.(check (option string)) "v" v' (Some v)
      in
      let add_commit t v =
        S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v >>= fun tree ->
        S.set_tree_exn ~parents:[] ~info:(infof "tree1") t [] tree
      in
      S.master repo >>= fun t ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(before (add_commit t v1))
        ~keep_max:true repo
      >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      find_commit t v1 >>= fun () ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(after (add_commit t v2))
        ~keep_max:true repo
      >>= fun () ->
      S.PrivateLayer.wait_for_freeze () >>= fun () ->
      find_commit t v2 >>= fun () ->
      add_commit t v3 >>= fun () ->
      S.Head.get t >>= fun c ->
      S.PrivateLayer.freeze_with_hook
        ~hook:(before (find_commit t v3))
        ~max:[ c ] ~keep_max:true repo
      >>= fun () -> S.Repo.close repo
    in
    run x test
end
