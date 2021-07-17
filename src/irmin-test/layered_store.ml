(*
 * Copyright (c) 2019-2021 Ioana Cristescu <ioana@tarides.com>
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

let src = Logs.Src.create "test" ~doc:"Irmin layered tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Make_Layered (S : Layered_store) = struct
  module P = S.Private
  module Graph = Irmin.Node.Graph (P.Node)
  module History = Irmin.Commit.History (P.Commit)
  open Common.Make_helpers (S)

  let info message =
    let date = Int64.of_float 0. in
    let author = Printf.sprintf "TESTS" in
    S.Info.v ~author ~message date

  let infof fmt = Fmt.kstrf (fun str () -> info str) fmt
  let v1 = "X1"
  let v2 = "X2"
  let v3 = "X3"
  let v4 = "X4"
  let v5 = "X5"
  let v6 = "X6"
  let b1 = "foo"
  let b2 = "bar/toto"

  let n1 ~repo =
    let* kv1 = with_contents repo (fun t -> P.Contents.add t v1) in
    let* kn1 = with_node repo (fun t -> Graph.v t [ ("x", normal kv1) ]) in
    with_node repo (fun t -> Graph.v t [ ("b", `Node kn1) ])

  let r1 ~repo =
    let* kn2 = n1 ~repo in
    S.Tree.of_key repo (`Node kn2) >>= function
    | None -> Alcotest.fail "r1"
    | Some tree ->
        S.Commit.v repo ~info:(info "r1") ~parents:[] (tree :> S.tree)

  let r2 ~repo =
    let* kn2 = n1 ~repo in
    let* kn3 = with_node repo (fun t -> Graph.v t [ ("a", `Node kn2) ]) in
    let* kr1 = r1 ~repo in
    S.Tree.of_key repo (`Node kn3) >>= function
    | None -> Alcotest.fail "r2"
    | Some t3 ->
        S.Commit.v repo ~info:(info "r2") ~parents:[ S.Commit.key kr1 ]
          (t3 :> S.tree)

  let fail_with_none f msg =
    f >>= function None -> Alcotest.fail msg | Some c -> Lwt.return c

  let fail_with_some f msg =
    f >>= function None -> Lwt.return_unit | Some _ -> Alcotest.fail msg

  (* Add nodes (from Private.Graph), commits (from Private.History)
           commits:kr2 ---> kr1
                    |        |
            nodes :kt3 -b-> kt2 -a-> kt1 -x-> kv1
     freeze kr1; find nodes and commits; check that kr2 is deleted;
     reconstruct node kt3 and commit again kr2; freeze kr2 and check again*)
  let test_graph_and_history x () =
    let test repo =
      let* kv1 = with_contents repo (fun t -> P.Contents.add t v1) in
      let check_val = check (T.option P.Commit.Val.t) in
      let check_key = check P.Commit.Key.t in
      let check_keys = checks P.Commit.Key.t in
      let* kt1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let* kt2 = with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) in
      let* kt3 = with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) in
      let* kr1, _ =
        with_info repo "commit kt2" (History.v ~node:kt2 ~parents:[])
      in
      let* kr2, _ =
        with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[ kr1 ])
      in
      let* t1 = P.Commit.find (h repo) kr1 in
      let* commit =
        fail_with_none (S.Commit.of_key repo kr1) "of_hash commit"
      in
      S.freeze repo ~max_lower:[ commit ] ~max_upper:[] >>= fun () ->
      let* t1' = P.Commit.find (h repo) kr1 in
      check_val "value of kr1 before and after freeze" t1 t1';
      let* kt1' = Graph.find (n repo) kt2 [ "a" ] in
      check (T.option Graph.value_t) "kt2 -a-> kt1 before and after freeze"
        (Some (`Node kt1))
        kt1';
      let* kt2' = Graph.find (n repo) kt3 [ "b" ] in
      if not (S.async_freeze repo) then
        check (T.option Graph.value_t) "kt3 -b-> kt2 deleted by freeze" None
          kt2';
      let* kt3 = with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) in
      let* kr2', _ =
        with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[ kr1 ])
      in
      check_key "commit kr2 after freeze" kr2 kr2';
      let* kr1s = History.closure (h repo) ~min:[] ~max:[ kr1 ] in
      check_keys "closure over lower" [ kr1 ] kr1s;
      let* kr2s = History.closure (h repo) ~min:[] ~max:[ kr2 ] in
      check_keys "closure over upper and lower" [ kr1; kr2 ] kr2s;
      let* t2 = P.Commit.find (h repo) kr2' in
      let* commit =
        fail_with_none (S.Commit.of_key repo kr2') "of_key commit"
      in
      S.freeze repo ~max_lower:[ commit ] ~max_upper:[] >>= fun () ->
      let* t1' = P.Commit.find (h repo) kr1 in
      check_val "value of kr1 before and after snd freeze" t1 t1';
      let* t2' = P.Commit.find (h repo) kr2 in
      check_val "value of kr2 before and after snd freeze" t2 t2';
      let* kt1' = Graph.find (n repo) kt2 [ "a" ] in
      check (T.option Graph.value_t) "kt2 -a-> kt1 before and after snd freeze"
        (Some (`Node kt1))
        kt1';
      let* kt2' = Graph.find (n repo) kt3 [ "b" ] in
      check (T.option Graph.value_t) "kt3 -b-> kt2 before and after snd freeze"
        (Some (`Node kt2))
        kt2';
      S.Repo.close repo
    in
    run x test

  (* Test freezes over several commits; temporary stores from commits *)
  let test_gc x () =
    let info = info "gc" in
    let check_val = check (T.option P.Commit.Val.t) in
    (*
      -> c0 -> c1 -> c2 |freeze|
                 \-> c3
    *)
    let tree1 repo =
      let tree = S.Tree.empty in
      let* tree = S.Tree.add tree [ "c"; "b"; "a" ] "x" in
      let* c0 = S.Commit.v repo ~info ~parents:[] tree in
      let* tree = S.Tree.add tree [ "c"; "b"; "a1" ] "x1" in
      let* c1 = S.Commit.v repo ~info ~parents:[ S.Commit.key c0 ] tree in
      let* tree = S.Tree.add tree [ "c"; "b"; "a2" ] "x2" in
      let* c2 = S.Commit.v repo ~info ~parents:[ S.Commit.key c1 ] tree in
      let* tree = S.Tree.add tree [ "c"; "b"; "a3" ] "x3" in
      let* c3 = S.Commit.v repo ~info ~parents:[ S.Commit.key c1 ] tree in
      Lwt.return (c2, c3)
    in
    (*
     \->  c5 -> c6 -> c7 |freeze|
     \->  c4      \-> c8
    *)
    let tree2 repo =
      let tree = S.Tree.empty in
      let* tree = S.Tree.add tree [ "c"; "b1" ] "x4" in
      let* c4 = S.Commit.v repo ~info ~parents:[] tree in
      let* tree = S.Tree.add tree [ "c"; "b2" ] "x5" in
      let* c5 = S.Commit.v repo ~info ~parents:[] tree in
      let* tree = S.Tree.add tree [ "c"; "b1"; "a" ] "x6" in
      let* c6 = S.Commit.v repo ~info ~parents:[ S.Commit.key c5 ] tree in
      let* tree = S.Tree.add tree [ "c"; "e" ] "x7" in
      let* c7 = S.Commit.v repo ~info ~parents:[ S.Commit.key c6 ] tree in
      let* tree = S.Tree.add tree [ "c"; "d" ] "x8" in
      let* c8 = S.Commit.v repo ~info ~parents:[ S.Commit.key c6 ] tree in
      Lwt.return (c4, c5, c7, c8)
    in
    let test repo =
      let* c2, c3 = tree1 repo in
      let* t2 = P.Commit.find (h repo) (S.Commit.key c2) in
      S.freeze repo ~max_lower:[ c2 ] ~max_upper:[] >>= fun () ->
      let* t2' = P.Commit.find (h repo) (S.Commit.key c2) in
      check_val "c2" t2 t2';
      let tree = S.Commit.tree c2 in
      let* x1 = S.Tree.find tree [ "c"; "b"; "a1" ] in
      Alcotest.(check (option string)) "x1" (Some "x1") x1;
      let* a = S.of_commit c2 in
      let* tree_a = S.tree a in
      let* x2 = S.Tree.find tree_a [ "c"; "b"; "a2" ] in
      Alcotest.(check (option string)) "x2" (Some "x2") x2;
      let* () =
        if not (S.async_freeze repo) then (
          let* t3 = P.Commit.find (h repo) (S.Commit.key c3) in
          check_val "c3" None t3;
          let* x3 = S.Tree.find tree [ "c"; "b"; "a3" ] in
          Alcotest.(check (option string)) "x3" None x3;
          let+ x3 = S.Tree.find tree_a [ "c"; "b"; "a3" ] in
          Alcotest.(check (option string)) "x3" None x3)
        else Lwt.return_unit
      in
      let* c4, c5, c7, c8 = tree2 repo in
      let* t5 = P.Commit.find (h repo) (S.Commit.key c5) in
      let* t7 = P.Commit.find (h repo) (S.Commit.key c7) in
      S.freeze repo ~max_lower:[ c7 ] ~max_upper:[] >>= fun () ->
      let* t5' = P.Commit.find (h repo) (S.Commit.key c5) in
      check_val "c5" t5 t5';
      let* t7' = P.Commit.find (h repo) (S.Commit.key c7) in
      check_val "c7" t7 t7';
      let* c7 =
        fail_with_none (S.Commit.of_key repo (S.Commit.key c7)) "of_key commit"
      in
      let tree = S.Commit.tree c7 in
      let* x7 = S.Tree.find tree [ "c"; "e" ] in
      Alcotest.(check (option string)) "x7" (Some "x7") x7;
      let* () =
        if not (S.async_freeze repo) then (
          let* t4 = P.Commit.find (h repo) (S.Commit.key c4) in
          check_val "c4" None t4;
          let* t8 = P.Commit.find (h repo) (S.Commit.key c8) in
          check_val "c8" None t8;
          fail_with_some
            (S.Commit.of_key repo (S.Commit.key c8))
            "should not find c8")
        else Lwt.return_unit
      in
      S.Repo.close repo
    in
    run x test

  (* Branches that point to deleted commits are deleted as well *)
  let test_fail_branch x () =
    let check_val repo = check (T.option @@ S.commit_t repo) in
    let test repo =
      let* kv1 = r1 ~repo in
      let* kv2 = r2 ~repo in
      S.Branch.set repo b1 kv1 >>= fun () ->
      S.Branch.set repo b2 kv2 >>= fun () ->
      S.freeze repo ~max_lower:[ kv1 ] ~max_upper:[] >>= fun () ->
      let* k1' = S.Branch.find repo b1 in
      check_val repo "r1 after freeze" (Some kv1) k1';
      let* k2' = S.Branch.find repo b2 in
      if not (S.async_freeze repo) then
        check_val repo "r2 deleted by freeze" None k2';
      S.Repo.close repo
    in
    run x test

  let check_layer repo handler msg exp =
    S.layer_id repo handler >|= check Irmin_layers.Layer_id.t msg exp

  let check_layer_for_commits repo commit msg exp =
    check_layer repo (S.Commit_t (S.Commit.key commit)) msg exp

  let test_set x () =
    let check_list = checks T.(pair S.Key.step_t S.tree_t) in
    let check_parents = checks P.Commit.Key.t in
    let contents v = S.Tree.v (`Contents (v, S.Metadata.default)) in
    let test repo =
      let* t = S.master repo in
      S.set_exn t [ "a"; "b"; "c" ] v1 ~info:(infof "commit 1") >>= fun () ->
      let* c1 = S.Head.get t in
      S.freeze repo ~max_lower:[ c1 ] ~max_upper:[] >>= fun () ->
      S.set_exn t [ "a"; "d" ] v2 ~info:(infof "commit 2") >>= fun () ->
      let* ks = S.list t [ "a" ] in
      let* b = S.get_tree t [ "a"; "b" ] in
      (* list sees a merged tree from lower and upper layers *)
      check_list "path" [ ("d", contents v2); ("b", b) ] ks;
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      check_layer_for_commits repo c1 "layer id of commit 1" `Lower
      >>= fun () ->
      let* c2 = S.Head.get t in
      check_layer_for_commits repo c2 "layer id of commit 2" `Upper0
      >>= fun () ->
      let parents = S.Commit.parents c2 in
      check_parents "parents of c2" [ S.Commit.key c1 ] parents;
      let* s = S.get t [ "a"; "b"; "c" ] in
      (* read value from lower layers *)
      Alcotest.(check string) "commit 1" s v1;
      let* () =
        S.set_exn t [ "a"; "b"; "c" ] "Hello" ~info:(infof "commit 3")
      in
      let* s = S.get t [ "a"; "b"; "c" ] in
      (* updated value on upper layers, hides lower layer *)
      Alcotest.(check string) "commit 3" s "Hello";
      S.Repo.close repo
    in
    run x test

  (* Set tree, add and remove to a tree; get head from lower. *)
  let test_set_tree x () =
    let test repo =
      let* t = S.master repo in
      let* t1 = S.Tree.add S.Tree.empty [ "a"; "d" ] v1 in
      let* t1 = S.Tree.add t1 [ "a"; "b"; "c" ] v2 in
      S.set_tree_exn ~info:(infof "commit 1") ~parents:[] t [] t1 >>= fun () ->
      S.freeze repo ~max_upper:[] >>= fun () ->
      let* c1 = S.Head.get t in
      let t1 = S.Commit.tree c1 in
      let* v = fail_with_none (S.Tree.find t1 [ "a"; "d" ]) "find in t1" in
      Alcotest.(check string) "t1" v v1;
      let* v = fail_with_none (S.Tree.find t1 [ "a"; "b"; "c" ]) "find in t1" in
      Alcotest.(check string) "t1" v v2;
      let* t2 = S.Tree.remove t1 [ "a"; "b"; "c" ] in
      let* () =
        S.set_tree_exn ~info:(infof "commit 2") ~parents:[ c1 ] t [] t2
      in
      let* t3 = S.get_tree t [] in
      let* () =
        fail_with_some (S.Tree.find t3 [ "a"; "b"; "c" ]) "found after remove"
      in
      let* v = fail_with_none (S.Tree.find t3 [ "a"; "d" ]) "find in t3" in
      Alcotest.(check string) "t3" v v1;
      let* () =
        S.Repo.branches repo >>= Lwt_list.iter_p (S.Branch.remove repo)
      in
      S.Repo.close repo
    in
    run x test

  (* Use branch deleted by freeze in a merge. *)
  let test_merge_into_deleted_branch x () =
    let test repo =
      let* foo = S.of_branch repo "foo" in
      let* bar = S.of_branch repo "bar" in
      S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1 >>= fun () ->
      S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1 >>= fun () ->
      let* c = S.Head.get foo in
      S.freeze repo ~max_lower:[ c ] ~max_upper:[] >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let* () =
        check_raises_lwt "Branch bar should point to a deleted commit"
          (Invalid_argument "merge_with_branch: bar is not a valid branch ID")
          (fun () ->
            S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo)
      in
      S.Repo.close repo
    in
    run x test

  (* As above, but the merging branch is deleted. *)
  let test_merge_with_deleted_branch x () =
    let test repo =
      let* foo = S.of_branch repo "foo" in
      let* bar = S.of_branch repo "bar" in
      S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1 >>= fun () ->
      S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1 >>= fun () ->
      let* c = S.Head.get bar in
      S.freeze repo ~max_lower:[ c ] ~max_upper:[] >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let* () =
        S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo
        >>= merge_exn "merge unrelated"
      in
      let* v1' = S.get bar [ "b" ] in
      check S.contents_t "v1" v1 v1';
      let* () =
        check_raises_lwt "Branch foo should point to a deleted commit"
          (Invalid_argument "Irmin.Tree.get: /a not found") (fun () ->
            S.get foo [ "a" ])
      in
      S.Repo.close repo
    in
    run x test

  let log_stats () = Logs.debug (fun l -> l "%t" Irmin_layers.Stats.pp_latest)

  let test_squash x () =
    let check_val = check T.(option S.contents_t) in
    let test repo =
      let* t = S.master repo in
      Irmin_layers.Stats.reset_stats ();
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] v1 >>= fun () ->
      let* c = S.Head.get t in
      let* tree = S.get_tree t [ "x" ] in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree >>= fun () ->
      let* c1 = S.Head.get t in
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] v2 >>= fun () ->
      let* c2 = S.Head.get t in
      let* tree3 = S.Tree.add tree [ "x"; "z" ] v1 in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree3 >>= fun () ->
      let* c3 = S.Head.get t in
      log_stats ();
      let () =
        let open Irmin_layers.Stats in
        Alcotest.(check int) "zero freeze" (get_freeze_count ()) 0;
        Alcotest.(check int) "zero commit" (get_copied_commits_count ()) 0
      in
      let* () =
        let* heads = S.Repo.heads repo in
        S.freeze repo ~min_lower:heads ~max_lower:heads ~max_upper:[]
      in
      let* tree = S.tree t in
      S.set_tree_exn t ~info:(infof "update") [] tree >>= fun () ->
      let* () =
        if not (S.async_freeze repo) then
          let* () =
            fail_with_some
              (S.Commit.of_key repo (S.Commit.key c))
              "should not find c"
          in
          let* () =
            fail_with_some
              (S.Commit.of_key repo (S.Commit.key c1))
              "should not find c1"
          in
          let* () =
            fail_with_some
              (S.Commit.of_key repo (S.Commit.key c2))
              "should not find c2"
          in
          let* _ =
            fail_with_none
              (S.Commit.of_key repo (S.Commit.key c3))
              "should find c3"
          in
          Lwt.return_unit
        else Lwt.return_unit
      in
      let* v1' = S.find t [ "x"; "y"; "z" ] in
      check_val "x/y/z after merge" (Some v1) v1';
      let* v1' = S.find t [ "u"; "x"; "z" ] in
      check_val "u/x/z after merge" (Some v1) v1';
      let* v1' = S.find t [ "u"; "y"; "z" ] in
      check_val "u/y/z after merge" (Some v1) v1';
      let* () =
        if not (S.async_freeze repo) then
          let+ v2' = S.find t [ "u"; "x"; "y" ] in
          check_val "vy after merge" None v2'
        else Lwt.return_unit
      in
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      log_stats ();
      let () =
        let open Irmin_layers.Stats in
        Alcotest.(check int) "one freeze" (get_freeze_count ()) 1;
        Alcotest.(check int) "one commit" (get_copied_commits_count ()) 1
      in
      P.Repo.close repo
    in
    run x test

  let test_branch_squash x () =
    let check_val = check T.(option S.contents_t) in
    let setup repo =
      let* tree1 = S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v1 in
      let* tree2 = S.Tree.add S.Tree.empty [ "a"; "b"; "d" ] v2 in
      let* foo = S.of_branch repo "foo" in
      S.set_tree_exn ~parents:[] ~info:(infof "tree1") foo [] tree1
      >>= fun () ->
      let* c1 = S.Head.get foo in
      let* t = S.master repo in
      S.set_tree_exn ~parents:[] ~info:(infof "tree2") t [] tree2 >>= fun () ->
      let+ c2 = S.Head.get t in
      (c1, c2)
    in
    let test repo =
      let* c1, c2 = setup repo in
      let* () =
        let* heads = S.Repo.heads repo in
        S.freeze repo ~min_lower:heads ~max_lower:heads ~max_upper:[]
      in
      let* () =
        P.Commit.mem (P.Repo.commit_t repo) (S.Commit.key c1) >>= function
        | true ->
            if not (S.async_freeze repo) then
              check_layer_for_commits repo c1 "layer id commit 1" `Lower
            else Lwt.return_unit
        | false ->
            Alcotest.failf "did not copy commit %a to dst" S.Commit.pp_hash c1
      in
      let* () =
        P.Commit.mem (P.Repo.commit_t repo) (S.Commit.key c2) >>= function
        | true ->
            if not (S.async_freeze repo) then
              check_layer_for_commits repo c2 "layer id commit 2" `Lower
            else Lwt.return_unit
        | false ->
            Alcotest.failf "did not copy commit %a to dst" S.Commit.pp_hash c2
      in
      let* foo = S.of_branch repo "foo" in
      let* v1' = S.find foo [ "a"; "b"; "c" ] in
      check_val "copy v1 to dst" (Some v1) v1';
      let* t = S.master repo in
      let* v2' = S.find t [ "a"; "b"; "d" ] in
      check_val "copy v2 to dst" (Some v2) v2';
      P.Repo.close repo
    in
    let test_squash repo =
      let* c1, c2 = setup repo in
      Irmin_layers.Stats.reset_stats ();
      S.freeze repo ~min_lower:[ c2 ] ~max_lower:[ c2 ] ~max_upper:[]
      >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let* () =
        P.Commit.mem (P.Repo.commit_t repo) (S.Commit.key c1) >|= function
        | true ->
            Alcotest.failf "should not copy commit %a to dst" S.Commit.pp_hash
              c1
        | false -> ()
      in
      let* () =
        P.Commit.mem (P.Repo.commit_t repo) (S.Commit.key c2) >>= function
        | true -> check_layer_for_commits repo c2 "layer id commit 2" `Lower
        | false ->
            Alcotest.failf "should copy commit %a to dst" S.Commit.pp_hash c2
      in
      let* t = S.master repo in
      let* v2' = S.find t [ "a"; "b"; "d" ] in
      check_val "copy v2 to dst" (Some v2) v2';
      let* () =
        let* v1' = S.find t [ "a"; "b"; "c" ] in
        check_val "copy v1 to dst" None v1';
        fail_with_some (S.Branch.find repo "foo")
          "should not find branch foo in dst"
      in
      log_stats ();
      let () =
        let open Irmin_layers.Stats in
        Alcotest.(check int) "one freeze" (get_freeze_count ()) 1;
        Alcotest.(check int) "one commit" (get_copied_commits_count ()) 1;
        Alcotest.(check int) "one commit" (get_copied_commits_count ()) 1;
        Alcotest.(check int) "one branch" (get_copied_branches_count ()) 1;
        Alcotest.(check int) "one content" (get_copied_contents_count ()) 1;
        Alcotest.(check int) "several nodes" (get_copied_nodes_count ()) 3
      in
      P.Repo.close repo
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
          let* tree = S.Tree.add tree [ "c"; "b"; a ] y in
          Log.debug (fun l -> l "commit %s" y);
          let* c = S.Commit.v repo ~info ~parents:[] tree in
          S.Branch.set repo b c >>= fun () ->
          S.freeze repo ~max_lower:[ c ] ~max_upper:[] >>= fun () ->
          S.Private_layer.wait_for_freeze repo >>= fun () ->
          commits tree (c :: acc) (i + 1)
      in
      let tests c i =
        let tree = S.Commit.tree c in
        let a = "a" ^ string_of_int i in
        let y = "y" ^ string_of_int i in
        let b = "b" ^ string_of_int i in
        Log.debug (fun l -> l "test %s" y);
        let* y' = S.Tree.find tree [ "c"; "b"; a ] in
        Alcotest.(check (option string)) "commit" (Some y) y';

        let* () =
          if not (S.async_freeze repo) then
            check_layer_for_commits repo c "layer id commit" `Lower
          else Lwt.return_unit
        in

        let+ c' = S.Branch.find repo b in
        check_val repo "branch" (Some c) c'
      in
      let* commits = commits tree [] 0 in
      Lwt_list.iteri_s (fun i c -> tests c i) (List.rev commits) >>= fun () ->
      S.Repo.close repo
    in
    run x test

  let test_freeze_tree x () =
    let info = info "two" in
    let test repo =
      let find4 tree =
        Log.debug (fun l -> l "find4");
        let+ x = S.Tree.find tree [ "4" ] in
        Alcotest.(check (option string)) "x4" (Some "x4") x
      in
      let find5 tree () =
        Log.debug (fun l -> l "find5");
        let+ x = S.Tree.find tree [ "5" ] in
        Alcotest.(check (option string)) "x5" (Some "x5") x
      in
      let tree = S.Tree.empty in
      let* tree = S.Tree.add tree [ "1"; "2"; "3" ] "x1" in
      let* tree = S.Tree.add tree [ "4" ] "x4" in
      let* tree = S.Tree.add tree [ "5" ] "x5" in
      Log.debug (fun l -> l "commit.v");
      let* h = S.Commit.v repo ~info ~parents:[] tree in
      S.Tree.clear tree;
      let* commit =
        fail_with_none
          (S.Commit.of_key repo (S.Commit.key h))
          "commit not found"
      in
      let tree = S.Commit.tree commit in
      (* copy in upper too *)
      S.freeze ~max_lower:[ h ] repo >>= fun () ->
      find4 tree >>= fun () ->
      (* copy in upper too *)
      S.freeze ~max_lower:[ h ] repo >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let tree = S.Commit.tree commit in
      find5 tree () >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_copy_in_upper x () =
    let test repo =
      let* kv1 = with_contents repo (fun t -> P.Contents.add t v1) in
      let* kt1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let* kt2 = with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) in
      let* kt3 = with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) in
      let* kr1, _ =
        with_info repo "commit kt3" (History.v ~node:kt3 ~parents:[])
      in
      let test_commit1 name_commit name =
        let* () =
          check_layer repo (S.Commit_t kr1) "layer id of commit 1" name_commit
        in
        check_layer repo (S.Node_t kt1) "layer id of node kt1" name
        >>= fun () ->
        check_layer repo (S.Node_t kt2) "layer id of node kt2" name
        >>= fun () ->
        check_layer repo (S.Node_t kt3) "layer id of node kt3" name
        >>= fun () ->
        check_layer repo (S.Content_t kv1) "layer id of content kv1" name
      in
      (* Test that commit c1 and all its objects are preserved in upper after a
         freeze. *)
      let* c1 = fail_with_none (S.Commit.of_key repo kr1) "of_key commit" in
      (* copy in upper too *)
      S.freeze repo ~max_lower:[ c1 ] >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      test_commit1 `Upper0 `Upper0 >>= fun () ->
      let* kv2 = with_contents repo (fun t -> P.Contents.add t v2) in
      let* kt1' = with_node repo (fun g -> Graph.v g [ ("d", normal kv2) ]) in
      let* kt2' = with_node repo (fun g -> Graph.v g [ ("a", `Node kt1') ]) in
      let* kr2, _ =
        with_info repo "commit kt2" (History.v ~node:kt2' ~parents:[ kr1 ])
      in
      let test_commit2 name =
        let* () =
          check_layer repo (S.Commit_t kr2) "layer id of commit 2" name
        in
        let* () =
          check_layer repo (S.Node_t kt1') "layer id of node kt1" name
        in
        let* () =
          check_layer repo (S.Node_t kt2') "layer id of node kt2" name
        in

        check_layer repo (S.Content_t kv2) "layer id of node kv2" name
      in
      (* Test that commits c1 and c2 are preserved in upper during and after a
         freeze. *)
      let* c2 = fail_with_none (S.Commit.of_key repo kr2) "of_key commit" in
      let* () =
        (* copy in upper too *)
        S.freeze repo ~min_upper:[ c1 ] ~max_lower:[ c2 ]
      in
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      test_commit1 `Upper1 `Upper1 >>= fun () ->
      test_commit2 `Upper1 >>= fun () -> S.Repo.close repo
    in

    run x test

  let test_copy_in_upper_set x () =
    let check_layer_id repo handler msg exp =
      let+ s = S.layer_id repo handler in
      if (s = `Upper1 || s = `Upper0) && exp = `Upper then ()
      else if s = `Lower && exp = `Lower then ()
      else Alcotest.fail msg
    in
    let test repo =
      let* foo = S.of_branch repo "foo" in
      S.set_exn foo [ "a"; "b"; "c" ] v1 ~info:(infof "commit 1") >>= fun () ->
      let* c1 = S.Head.get foo in
      (* Test that contents of commit c1 is preserved in upper during and after
         a freeze. *)
      (* copy in upper too *)
      S.freeze repo ~max_lower:[ c1 ] >>= fun () ->
      let* hv1 = fail_with_none (S.key foo [ "a"; "b"; "c" ]) "hash of v1'" in
      let hv1 = get_contents_key hv1 in
      check_layer_id repo (S.Content_t hv1) "layer id of v1" `Upper
      >>= fun () ->
      S.set_exn foo [ "a"; "d" ] v2 ~info:(infof "commit 2") >>= fun () ->
      let* c2 = S.Head.get foo in
      let hc2 = S.Commit.key c2 in
      check_layer_id repo (S.Commit_t hc2) "layer_id commit 2" `Upper
      >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      check_layer_id repo (S.Content_t hv1) "layer id of v1" `Upper
      >>= fun () ->
      (* Test that commit c2 and all its objects are preserved in upper during
         and after a freeze. *)
      (* copy in upper too *)
      S.freeze repo ~max_lower:[ c2 ] >>= fun () ->
      check_layer_id repo (S.Commit_t hc2) "layer_id commit 2" `Upper
      >>= fun () ->
      check_layer_id repo (S.Content_t hv1) "layer id of v1" `Upper
      >>= fun () ->
      let* hv2 = fail_with_none (S.key foo [ "a"; "d" ]) "hash of v2'" in
      let hv2 = get_contents_key hv2 in
      check_layer_id repo (S.Content_t hv2) "layer id of v2" `Upper
      >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let hc1 = S.Commit.key c1 in
      check_layer_id repo (S.Commit_t hc1) "layer_id commit 1" `Lower
      >>= fun () ->
      check_layer_id repo (S.Commit_t hc2) "layer_id commit 2" `Upper
      >>= fun () ->
      check_layer_id repo (S.Content_t hv1) "layer id of v1" `Upper
      >>= fun () ->
      check_layer_id repo (S.Content_t hv2) "layer id of v2" `Upper
      >>= fun () -> S.Repo.close repo
    in
    run x test

  (* c0 <- c1 <- c2 "b2"
              \- c3 <- c4
                    \- c5 "b5"
                    \- c6 "b6" *)
  let test_keep_heads x () =
    let check_layer_id repo handler msg exp =
      let+ s = S.layer_id repo handler in
      if (s = `Upper1 || s = `Upper0) && exp = `Upper then ()
      else if s = `Lower && exp = `Lower then ()
      else Alcotest.fail msg
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
          let* hx = with_contents repo (fun t -> P.Contents.add t x) in
          let contents = hx :: contents in
          let* tree' = S.Tree.add tree [ "a"; "b"; c ] x in
          match i with
          | 0 ->
              let* c0 = S.Commit.v repo ~info ~parents:[] tree' in
              setup (i + 1) tree' [ c0 ] contents
          | 1 ->
              let* c1 =
                S.Commit.v repo ~info
                  ~parents:[ S.Commit.key (List.nth commits 0) ]
                  tree'
              in
              setup (i + 1) tree' (commits @ [ c1 ]) contents
          | 2 ->
              let* c =
                S.Commit.v repo ~info
                  ~parents:[ S.Commit.key (List.nth commits 1) ]
                  tree'
              in
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree (commits @ [ c ]) contents
          | 3 ->
              let* c =
                S.Commit.v repo ~info
                  ~parents:[ S.Commit.key (List.nth commits 1) ]
                  tree'
              in
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree' (commits @ [ c ]) contents
          | 4 ->
              let* c =
                S.Commit.v repo ~info
                  ~parents:[ S.Commit.key (List.nth commits 3) ]
                  tree'
              in
              setup (i + 1) tree (commits @ [ c ]) contents
          | 5 | 6 ->
              let* c =
                S.Commit.v repo ~info
                  ~parents:[ S.Commit.key (List.nth commits 3) ]
                  tree'
              in
              S.Branch.set repo b c >>= fun () ->
              setup (i + 1) tree (commits @ [ c ]) contents
          | _ -> assert false
      in
      let* commits, contents = setup 0 tree [] [] in
      let* () =
        S.freeze repo ~min_lower:[ List.nth commits 1 ]
          ~min_upper:[ List.nth commits 3 ]
          ~max_lower:[ List.nth commits 5; List.nth commits 6 ]
      in
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let* () =
        S.freeze repo ~min_lower:[ List.nth commits 1 ]
          ~min_upper:[ List.nth commits 3 ]
          ~max_lower:[ List.nth commits 5; List.nth commits 6 ]
      in
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      let* () =
        Lwt_list.iter_p
          (fun i ->
            let+ t =
              P.Commit.find (h repo) (S.Commit.key (List.nth commits i))
            in
            check_commit "deleted commit" None t)
          [ 0; 2; 4 ]
      in
      let* c2' = S.Branch.find repo "b2" in
      check_branch repo "b2" None c2';
      let* c3' = S.Branch.find repo "b3" in
      check_branch repo "b3" (Some (List.nth commits 3)) c3';
      let* c5' = S.Branch.find repo "b5" in
      check_branch repo "b5" (Some (List.nth commits 5)) c5';
      let* c6' = S.Branch.find repo "b6" in
      check_branch repo "b6" (Some (List.nth commits 6)) c6';

      let* () =
        check_layer_id repo
          (S.Commit_t (S.Commit.key (List.nth commits 1)))
          "layer id of c1" `Lower
      in
      let* () =
        Lwt_list.iter_p
          (fun i ->
            check_layer_id repo
              (S.Commit_t (S.Commit.key (List.nth commits i)))
              ("layer id of c" ^ string_of_int i)
              `Upper)
          [ 3; 5; 6 ]
      in

      let* t = S.of_branch repo "b6" in
      let* tree = S.get_tree t [] in
      let* s = fail_with_none (S.Tree.find tree [ "a"; "b"; "c0" ]) "find x0" in
      Alcotest.(check string) "x0" s "x0";
      let* s = fail_with_none (S.Tree.find tree [ "a"; "b"; "c1" ]) "find x1" in
      Alcotest.(check string) "x1" s "x1";

      let* () =
        Lwt_list.iter_p
          (fun i ->
            check_layer_id repo
              (S.Content_t (List.nth contents i))
              ("layer id of x" ^ string_of_int i)
              `Upper)
          [ 0; 1; 3 ]
      in
      let* () =
        check_raises_lwt "Should not find x2" Not_found (fun () ->
            S.layer_id repo (S.Content_t (List.nth contents 2)))
      in
      let* () =
        check_raises_lwt "Should not found x4" Not_found (fun () ->
            S.layer_id repo (S.Content_t (List.nth contents 4)))
      in
      let* () =
        Lwt_list.iter_p
          (fun i ->
            check_layer_id repo
              (S.Content_t (List.nth contents i))
              ("layer id of x" ^ string_of_int i)
              `Upper)
          [ 3; 5; 6 ]
      in

      S.Repo.close repo
    in
    run x test

  module Hook = S.Private_layer.Hook

  let before_copy f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `Before_Copy ->
            Log.debug (fun l -> l "test hook -- before_copy");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let before_copy_newies f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `Before_Copy_Newies ->
            Log.debug (fun l -> l "test hook -- before_copy_newies");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let before_copy_last_newies f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `Before_Copy_Last_Newies ->
            Log.debug (fun l -> l "test hook -- before_copy_last_newies");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let before_flip f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `Before_Flip ->
            Log.debug (fun l -> l "test hook -- before_flip");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let before_clear f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `Before_Clear ->
            Log.debug (fun l -> l "test hook -- before_clear");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let after_clear f =
    let p, r = Lwt.wait () in
    let hook =
      Hook.v (function
        | `After_Clear ->
            Log.debug (fun l -> l "test hook -- after_clear");
            Lwt.async (fun () -> f () >|= fun () -> Lwt.wakeup r ());
            Lwt.return_unit
        | _ -> Lwt.return_unit)
    in
    (hook, p)

  let test_find_during_freeze x () =
    let info = info "hooks" in
    let test repo =
      let find_commit c v () =
        let* commit =
          fail_with_none
            (S.Commit.of_key repo (S.Commit.key c))
            "no hash found in repo"
        in
        let tree = S.Commit.tree commit in
        let+ v' = S.Tree.find tree [ "a"; "b"; "c" ] in
        Alcotest.(check (option string)) "v1" v' (Some v)
      in
      let add_and_find_commit ~hook v =
        let* tree = S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v in
        let* c = S.Commit.v repo ~info ~parents:[] tree in
        let hook, p = hook (find_commit c v) in
        let* () =
          (* copy in upper too *)
          S.Private_layer.freeze' ~hook ~max_lower:[ c ] repo
        in
        p >>= fun () -> S.Private_layer.wait_for_freeze repo
      in
      add_and_find_commit ~hook:before_copy v1 >>= fun () ->
      add_and_find_commit ~hook:before_copy_newies v2 >>= fun () ->
      add_and_find_commit ~hook:before_copy_last_newies v3 >>= fun () ->
      add_and_find_commit ~hook:before_flip v4 >>= fun () ->
      add_and_find_commit ~hook:before_clear v5 >>= fun () ->
      add_and_find_commit ~hook:after_clear v6 >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_add_during_freeze x () =
    let test repo =
      let find_commit t v () =
        let* c = S.Head.get t in
        let* commit =
          fail_with_none
            (S.Commit.of_key repo (S.Commit.key c))
            "no hash found in repo"
        in
        let tree = S.Commit.tree commit in
        let+ v' = S.Tree.find tree [ "a"; "b"; "c" ] in
        Alcotest.(check (option string)) "v" v' (Some v)
      in
      let add_commit t v () =
        let* tree = S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v in
        S.set_tree_exn ~parents:[] ~info:(infof "tree1") t [] tree
      in
      let add_and_find_commit ~hook t v =
        let hook, p = hook (add_commit t v) in
        (* copy in upper too *)
        S.Private_layer.freeze' ~hook repo >>= fun () ->
        p >>= fun () ->
        find_commit t v () >>= fun () ->
        S.Private_layer.wait_for_freeze repo >>= fun () -> find_commit t v ()
      in
      let* t = S.master repo in
      add_and_find_commit ~hook:before_copy t v1 >>= fun () ->
      add_and_find_commit ~hook:before_copy_newies t v2 >>= fun () ->
      add_and_find_commit ~hook:before_copy_last_newies t v3 >>= fun () ->
      add_and_find_commit ~hook:before_flip t v4 >>= fun () ->
      add_and_find_commit ~hook:before_clear t v5 >>= fun () ->
      add_and_find_commit ~hook:after_clear t v6 >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_add_again x () =
    let test repo =
      let* t = S.master repo in
      let* tree = S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v1 in
      S.set_tree_exn ~parents:[] ~info:(infof "v1") t [] tree >>= fun () ->
      let* tree = S.Tree.add S.Tree.empty [ "a"; "d" ] v2 in
      S.set_tree_exn ~parents:[] ~info:(infof "v2") t [] tree >>= fun () ->
      let add_commit () =
        let* tree = S.Tree.add S.Tree.empty [ "a"; "b"; "c" ] v1 in
        let* tree = S.Tree.add tree [ "a"; "e" ] v3 in
        S.set_tree_exn ~parents:[] ~info:(infof "v3") t [] tree
      in
      let hook, p = before_copy add_commit in
      let* () =
        (* copy in upper too *)
        let* heads = S.Repo.heads repo in
        S.Private_layer.freeze' repo ~hook ~min_lower:heads ~max_lower:heads
      in
      p >>= fun () ->
      S.Private_layer.wait_for_freeze repo >>= fun () ->
      S.Repo.close repo >>= fun () ->
      let* repo = S.Repo.v x.config in
      let* t = S.master repo in
      let* c = S.Head.get t in
      let* commit =
        fail_with_none
          (S.Commit.of_key repo (S.Commit.key c))
          "no hash found in repo"
      in
      let tree = S.Commit.tree commit in
      let* v' = S.Tree.find tree [ "a"; "b"; "c" ] in
      Alcotest.(check (option string)) "v" v' (Some v1);
      S.Repo.close repo
    in
    run x test
end
