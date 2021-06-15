(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "test" ~doc:"Irmin tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (S : S) = struct
  include Common.Make_helpers (S)
  module History = Irmin.Private.Commit.History (P.Commit)

  let with_binding k v t = S.Tree.add t k v
  let random_value value = random_string value

  let random_path ~label ~path =
    let short () = random_ascii_string label in
    let rec aux = function 0 -> [] | n -> short () :: aux (n - 1) in
    aux path

  let random_node ~label ~path ~value =
    (random_path ~label ~path, random_value value)

  let random_nodes ?(label = 8) ?(path = 5) ?(value = 1024) n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (random_node ~label ~path ~value :: acc) (n - 1)
    in
    aux [] n

  let old k () = Lwt.return_ok (Some k)

  let may repo commits = function
    | None -> Lwt.return_unit
    | Some f -> f repo commits

  let may_get_hashes repo hashes = function
    | None -> Lwt.return_unit
    | Some f ->
        let* commits =
          Lwt_list.map_p
            (fun hash ->
              S.Commit.of_hash repo hash >|= function
              | None -> Alcotest.fail "Cannot read commit hash"
              | Some c -> c)
            hashes
        in
        f repo commits

  let may_with_branch branches repo hook =
    let* heads =
      Lwt_list.map_p
        (fun branch ->
          let+ h = S.Head.find branch in
          match h with
          | None -> Alcotest.fail "Cannot read head"
          | Some head -> head)
        branches
    in
    may repo heads hook

  let contents c = S.Tree.v (`Contents (c, S.Metadata.default))

  let test_contents x () =
    let test repo =
      let t = P.Repo.contents_t repo in
      let check_key = check P.Contents.Key.t in
      let check_val = check (T.option S.contents_t) in
      let* kv2 = kv2 ~repo in
      let* k2' = with_contents repo (fun t -> P.Contents.add t v2) in
      check_key "kv2" kv2 k2';
      let* v2' = P.Contents.find t k2' in
      check_val "v2" (Some v2) v2';
      let* k2'' = with_contents repo (fun t -> P.Contents.add t v2) in
      check_key "kv2" kv2 k2'';
      let* kv1 = kv1 ~repo in
      let* k1' = with_contents repo (fun t -> P.Contents.add t v1) in
      check_key "kv1" kv1 k1';
      let* k1'' = with_contents repo (fun t -> P.Contents.add t v1) in
      check_key "kv1" kv1 k1'';
      let* v1' = P.Contents.find t kv1 in
      check_val "v1" (Some v1) v1';
      let* v2' = P.Contents.find t kv2 in
      check_val "v2" (Some v2) v2';
      P.Repo.close repo >>= fun () ->
      Lwt.catch
        (fun () ->
          let+ _ = with_contents repo (fun t -> P.Contents.add t v2) in
          Alcotest.fail "Add after close should not be allowed")
        (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
    in
    run x test

  let get = function None -> Alcotest.fail "get" | Some v -> v

  module H_node = Irmin.Hash.Typed (P.Hash) (P.Node.Val)

  let test_nodes x () =
    let test repo =
      let g = g repo and n = n repo in
      let k = normal (P.Contents.Key.hash "foo") in
      let check_key = check P.Node.Key.t in
      let check_val = check [%typ: Graph.value option] in
      let check_list = checks [%typ: S.step * P.Node.Val.value] in
      let check_node msg v =
        let h' = H_node.hash v in
        let+ h = with_node repo (fun n -> P.Node.add n v) in
        check_key (msg ^ ": hash(v) = add(v)") h h'
      in
      let v = P.Node.Val.empty in
      check_node "empty node" v >>= fun () ->
      let v1 = P.Node.Val.add v "x" k in
      check_node "node: x" v1 >>= fun () ->
      let v2 = P.Node.Val.add v "x" k in
      check_node "node: x (bis)" v2 >>= fun () ->
      check P.Node.Val.t "add x" v1 v2;
      let v0 = P.Node.Val.remove v1 "x" in
      check P.Node.Val.t "remove x" v v0;
      let v3 = P.Node.Val.add v1 "x" k in
      Alcotest.(check bool) "same same" true (v1 == v3);
      let u = P.Node.Val.add v3 "y" k in
      check_node "node: x+y" v3 >>= fun () ->
      let u = P.Node.Val.add u "z" k in
      check_node "node: x+y+z" u >>= fun () ->
      let check_values u =
        check_val "find x" (Some k) (P.Node.Val.find u "x");
        check_val "find y" (Some k) (P.Node.Val.find u "y");
        check_val "find z" (Some k) (P.Node.Val.find u "x");
        check_val "find xx" None (P.Node.Val.find u "xx")
      in
      check_values u;
      let w = P.Node.Val.v [ ("y", k); ("z", k); ("x", k) ] in
      check P.Node.Val.t "v" u w;
      let l = P.Node.Val.list u in
      check_list "list all" [ ("x", k); ("y", k); ("z", k) ] l;
      let l = P.Node.Val.list ~length:1 u in
      check_list "list length=1" [ ("x", k) ] l;
      let l = P.Node.Val.list ~offset:1 u in
      check_list "list offset=1" [ ("y", k); ("z", k) ] l;
      let l = P.Node.Val.list ~offset:1 ~length:1 u in
      check_list "list offset=1 length=1" [ ("y", k) ] l;
      let u = P.Node.Val.add u "a" k in
      check_node "node: x+y+z+a" u >>= fun () ->
      let u = P.Node.Val.add u "b" k in
      check_node "node: x+y+z+a+b" u >>= fun () ->
      let h = H_node.hash u in
      let* k = with_node repo (fun n -> P.Node.add n u) in
      check_key "hash(v) = add(v)" h k;
      let* w = P.Node.find n k in
      check_values (get w);
      let* kv1 = kv1 ~repo in
      let* k1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let* k1' = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      check_key "k1.1" k1 k1';
      let* t1 = P.Node.find n k1 in
      let k' = P.Node.Val.find (get t1) "x" in
      check
        (Irmin.Type.option P.Node.Val.value_t)
        "find x"
        (Some (normal kv1))
        k';
      let* k1'' = with_node repo (fun n -> P.Node.add n (get t1)) in
      check_key "k1.2" k1 k1'';
      let* k2 = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      let* k2' = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      check_key "k2.1" k2 k2';
      let* t2 = P.Node.find n k2 in
      let* k2'' = with_node repo (fun n -> P.Node.add n (get t2)) in
      check_key "k2.2" k2 k2'';
      let* k1''' = Graph.find g k2 [ "b" ] in
      check_val "k1.3" (Some (`Node k1)) k1''';
      let* k3 = with_node repo (fun g -> Graph.v g [ ("a", `Node k2) ]) in
      let* k3' = with_node repo (fun g -> Graph.v g [ ("a", `Node k2) ]) in
      check_key "k3.1" k3 k3';
      let* t3 = P.Node.find n k3 in
      let* k3'' = with_node repo (fun n -> P.Node.add n (get t3)) in
      check_key "k3.2" k3 k3'';
      let* k2'' = Graph.find g k3 [ "a" ] in
      check_val "k2.3" (Some (`Node k2)) k2'';
      let* k1'''' = Graph.find g k2' [ "b" ] in
      check_val "t1.2" (Some (`Node k1)) k1'''';
      let* k1''''' = Graph.find g k3 [ "a"; "b" ] in
      check_val "t1.3" (Some (`Node k1)) k1''''';
      let* kv11 = Graph.find g k1 [ "x" ] in
      check_val "v1.1" (Some (normal kv1)) kv11;
      let* kv12 = Graph.find g k2 [ "b"; "x" ] in
      check_val "v1.2" (Some (normal kv1)) kv12;
      let* kv13 = Graph.find g k3 [ "a"; "b"; "x" ] in
      check_val "v1" (Some (normal kv1)) kv13;
      let* kv2 = kv2 ~repo in
      let* k4 = with_node repo (fun g -> Graph.v g [ ("x", normal kv2) ]) in
      let* k5 =
        with_node repo (fun g -> Graph.v g [ ("b", `Node k1); ("c", `Node k4) ])
      in
      let* k6 = with_node repo (fun g -> Graph.v g [ ("a", `Node k5) ]) in
      let* k6' =
        with_node repo (fun g -> Graph.add g k3 [ "a"; "c"; "x" ] (normal kv2))
      in
      check_key "node k6" k6 k6';
      let* n6' = P.Node.find n k6' in
      let* n6 = P.Node.find n k6 in
      check T.(option P.Node.Val.t) "node n6" n6 n6';
      let assert_no_duplicates n node =
        let names = ref [] in
        let+ all = Graph.list g node in
        List.iter
          (fun (s, _) ->
            if List.mem s !names then Alcotest.failf "%s: duplicate!" n
            else names := s :: !names)
          all
      in
      let* n0 = with_node repo (fun g -> Graph.v g []) in
      let* n1 = with_node repo (fun g -> Graph.add g n0 [ "b" ] (`Node n0)) in
      let* n2 = with_node repo (fun g -> Graph.add g n1 [ "a" ] (`Node n0)) in
      let* n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (`Node n0)) in
      assert_no_duplicates "1" n3 >>= fun () ->
      let* n1 = with_node repo (fun g -> Graph.add g n0 [ "a" ] (`Node n0)) in
      let* n2 = with_node repo (fun g -> Graph.add g n1 [ "b" ] (`Node n0)) in
      let* n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (`Node n0)) in
      assert_no_duplicates "2" n3 >>= fun () ->
      let* n1 = with_node repo (fun g -> Graph.add g n0 [ "b" ] (normal kv1)) in
      let* n2 = with_node repo (fun g -> Graph.add g n1 [ "a" ] (normal kv1)) in
      let* n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (normal kv1)) in
      assert_no_duplicates "3" n3 >>= fun () ->
      let* n1 = with_node repo (fun g -> Graph.add g n0 [ "a" ] (normal kv1)) in
      let* n2 = with_node repo (fun g -> Graph.add g n1 [ "b" ] (normal kv1)) in
      let* n3 = with_node repo (fun g -> Graph.add g n2 [ "b" ] (normal kv1)) in
      assert_no_duplicates "4" n3 >>= fun () ->
      S.Repo.close repo >>= fun () ->
      Lwt.catch
        (fun () ->
          let* n0 = with_node repo (fun g -> Graph.v g []) in
          let* _ =
            with_node repo (fun g -> Graph.add g n0 [ "b" ] (`Node n0))
          in
          Alcotest.fail "Add after close should not be allowed")
        (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
    in
    run x test

  let test_commits x () =
    let test repo =
      let info date =
        let msg = Fmt.strf "Test commit: %d" date in
        Irmin.Info.v ~date:(Int64.of_int date) ~author:"test" msg
      in
      let* kv1 = kv1 ~repo in
      let h = h repo and c = P.Repo.commit_t repo in
      let check_val = check (T.option P.Commit.Val.t) in
      let check_key = check P.Commit.Key.t in
      let check_keys = checks P.Commit.Key.t in
      (* t3 -a-> t2 -b-> t1 -x-> (v1) *)
      let* kt1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let* kt2 = with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) in
      let* kt3 = with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) in
      (* r1 : t2 *)
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let* kr1, _ = with_info 3 (History.v ~node:kt2 ~parents:[]) in
      let* kr1', _ = with_info 3 (History.v ~node:kt2 ~parents:[]) in
      let* t1 = P.Commit.find c kr1 in
      let* t1' = P.Commit.find c kr1' in
      check_val "t1" t1 t1';
      check_key "kr1" kr1 kr1';

      (* r1 -> r2 : t3 *)
      let* kr2, _ = with_info 4 (History.v ~node:kt3 ~parents:[ kr1 ]) in
      let* kr2', _ = with_info 4 (History.v ~node:kt3 ~parents:[ kr1 ]) in
      check_key "kr2" kr2 kr2';
      let* kr1s = History.closure h ~min:[] ~max:[ kr1 ] in
      check_keys "g1" [ kr1 ] kr1s;
      let* kr2s = History.closure h ~min:[] ~max:[ kr2 ] in
      check_keys "g2" [ kr1; kr2 ] kr2s;
      let* () =
        S.Commit.of_hash repo kr1 >|= function
        | None -> Alcotest.fail "Cannot read commit hash"
        | Some c ->
            Alcotest.(check string)
              "author" "test"
              (Irmin.Info.author (S.Commit.info c))
      in
      S.Repo.close repo >>= fun () ->
      Lwt.catch
        (fun () ->
          let+ _ = with_info 3 (History.v ~node:kt1 ~parents:[]) in
          Alcotest.fail "Add after close should not be allowed")
        (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
    in
    run x test

  let test_closure x () =
    let test repo =
      let info date =
        let msg = Fmt.strf "Test commit: %d" date in
        Irmin.Info.v ~date:(Int64.of_int date) ~author:"test" msg
      in
      let check_keys = checks P.Commit.Key.t in
      let h = h repo in
      let initialise_nodes =
        Lwt_list.map_p
          (fun i ->
            let* kv =
              with_contents repo (fun t -> P.Contents.add t (string_of_int i))
            in
            with_node repo (fun g -> Graph.v g [ (string_of_int i, normal kv) ]))
          [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
      in
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let initialise_graph nodes =
        match nodes with
        | [] -> assert false
        | node :: rest ->
            let* kr0, _ = with_info 0 (History.v ~node ~parents:[]) in
            let commits = Array.make 9 kr0 in
            let commit ~node ~parents i =
              let+ kr1, _ = with_info i (History.v ~node ~parents) in
              commits.(i) <- kr1;
              i + 1
            in
            let+ _ =
              Lwt_list.fold_left_s
                (fun i node ->
                  match i with
                  | 1 -> commit ~node ~parents:[ commits.(0) ] 1
                  | 2 -> commit ~node ~parents:[] 2
                  | 3 -> commit ~node ~parents:[ commits.(1) ] 3
                  | 4 -> commit ~node ~parents:[ commits.(1); commits.(2) ] 4
                  | 5 -> commit ~node ~parents:[ commits.(3); commits.(4) ] 5
                  | 6 -> commit ~node ~parents:[ commits.(4) ] 6
                  | 7 -> commit ~node ~parents:[] 7
                  | 8 -> commit ~node ~parents:[ commits.(7) ] 8
                  | _ -> assert false)
                1 rest
            in
            commits
      in
      (* initialise_graph creates the following graph of commits:
            0 <- 1 <- 3 <- 5   and   7 <- 8
                  \   /
             2 <-- 4 <- 6 *)
      let* commits = initialise_nodes >>= initialise_graph in
      let* krs = History.closure h ~min:[ commits.(1) ] ~max:[ commits.(5) ] in
      check_keys "commits between 1 and 5"
        [ commits.(1); commits.(2); commits.(3); commits.(4); commits.(5) ]
        krs;
      let* krs = History.closure h ~min:[] ~max:[ commits.(5) ] in
      check_keys "all commits under 5"
        [
          commits.(0);
          commits.(1);
          commits.(2);
          commits.(3);
          commits.(4);
          commits.(5);
        ]
        krs;
      let* krs =
        History.closure h
          ~min:[ commits.(1); commits.(2) ]
          ~max:[ commits.(5); commits.(6) ]
      in
      check_keys "disconnected max and min returns a connected graph"
        [
          commits.(1);
          commits.(2);
          commits.(3);
          commits.(4);
          commits.(5);
          commits.(6);
        ]
        krs;
      let* krs =
        History.closure h
          ~min:[ commits.(1); commits.(7) ]
          ~max:[ commits.(4); commits.(8) ]
      in
      check_keys "disconnected min and max returns a disconnected graph"
        [ commits.(1); commits.(2); commits.(7); commits.(4); commits.(8) ]
        krs;
      let* () =
        History.closure h ~min:[ commits.(7) ] ~max:[] >|= function
        | [] -> ()
        | _ -> Alcotest.fail "expected empty list"
      in
      let* () =
        let+ ls = History.closure h ~min:[ commits.(7) ] ~max:[ commits.(6) ] in
        if List.mem commits.(7) ls then
          Alcotest.fail "disconnected node should not be in closure"
      in
      let* krs =
        History.closure h ~min:[ commits.(4) ] ~max:[ commits.(4); commits.(6) ]
      in
      check_keys "min and max have the same commit"
        [ commits.(6); commits.(4) ]
        krs;
      let* () =
        let+ ls =
          History.closure h
            ~min:[ commits.(4); commits.(0) ]
            ~max:[ commits.(4); commits.(6) ]
        in
        if List.mem commits.(0) ls then
          Alcotest.fail "disconnected node should not be in closure"
      in
      S.Repo.close repo
    in
    run x test

  let test_branches ?hook x () =
    let test repo =
      let check_keys = checks S.Branch.t in
      let check_val = check (T.option @@ S.commit_t repo) in
      let* kv1 = r1 ~repo in
      let* kv2 = r2 ~repo in
      line "pre-update";
      S.Branch.set repo b1 kv1 >>= fun () ->
      may repo [ kv2 ] hook >>= fun () ->
      line "post-update";
      let* k1' = S.Branch.find repo b1 in
      check_val "r1" (Some kv1) k1';
      S.Branch.set repo b2 kv2 >>= fun () ->
      let* k2' = S.Branch.find repo b2 in
      check_val "r2" (Some kv2) k2';
      S.Branch.set repo b1 kv2 >>= fun () ->
      let* k2'' = S.Branch.find repo b1 in
      check_val "r1-after-update" (Some kv2) k2'';
      let* bs = S.Branch.list repo in
      check_keys "list" [ b1; b2 ] bs;
      S.Branch.remove repo b1 >>= fun () ->
      let* empty = S.Branch.find repo b1 in
      check_val "empty" None empty;
      let* b2' = S.Branch.list repo in
      check_keys "all-after-remove" [ b2 ] b2';
      S.Repo.close repo >>= fun () ->
      Lwt.catch
        (fun () ->
          let+ _ = S.Branch.set repo b1 kv1 in
          Alcotest.fail "Add after close should not be allowed")
        (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
    in
    run x test

  let test_tree_hashes x () =
    let test repo =
      let node bindings =
        with_node repo (fun g ->
            let* empty = Graph.empty g in
            Lwt_list.fold_left_s
              (fun t (k, v) ->
                let* v = with_contents repo (fun t -> P.Contents.add t v) in
                Graph.add g t k (`Contents (v, S.Metadata.default)))
              empty bindings)
      in
      let tree bindings =
        Lwt_list.fold_left_s
          (fun t (k, v) -> S.Tree.add t k v)
          S.Tree.empty bindings
      in
      let check_hash msg bindings =
        let* node = node bindings in
        let+ tree = tree bindings in
        check S.Hash.t msg node (S.Tree.hash tree)
      in
      check_hash "empty" [] >>= fun () ->
      let bindings1 = [ ([ "a" ], "x"); ([ "b" ], "y") ] in
      check_hash "1 level" bindings1 >>= fun () ->
      let bindings2 = [ ([ "a"; "b" ], "x"); ([ "a"; "c" ], "y") ] in
      check_hash "2 levels" bindings2 >>= fun () -> S.Repo.close repo
    in
    run x test

  let test_simple_merges ?hook x () =
    (* simple merges *)
    let check_merge () =
      let ok = Irmin.Merge.ok in
      let dt = [%typ: int64 option] in
      let dx = [%typ: (string * int64) list] in
      let merge_skip ~old:_ _ _ = ok None in
      let merge_left ~old:_ x _ = ok x in
      let merge_right ~old:_ _ y = ok y in
      let merge_default = Irmin.Merge.default dt in
      let merge = function
        | "left" -> Irmin.Merge.v dt merge_left
        | "right" -> Irmin.Merge.v dt merge_right
        | "skip" -> Irmin.Merge.v dt merge_skip
        | _ -> merge_default
      in
      let merge_x = Irmin.Merge.alist T.string T.int64 merge in
      let old () = ok (Some [ ("left", 1L); ("foo", 2L) ]) in
      let x = [ ("left", 2L); ("right", 0L) ] in
      let y = [ ("left", 1L); ("bar", 3L); ("skip", 0L) ] in
      let m = [ ("left", 2L); ("bar", 3L) ] in
      Irmin.Merge.(f merge_x) ~old x y >>= function
      | Error (`Conflict c) -> Alcotest.failf "conflict %s" c
      | Ok m' ->
          check dx "compound merge" m m';
          Lwt.return_unit
    in
    let test repo =
      check_merge () >>= fun () ->
      let* kv1 = kv1 ~repo in
      let* kv2 = kv2 ~repo in
      let check_result =
        check (Irmin.Merge.result_t T.(option P.Contents.Key.t))
      in
      (* merge contents *)
      let* kv1' =
        with_contents repo (fun v ->
            Irmin.Merge.f (P.Contents.merge v) ~old:(old (Some kv1)) (Some kv1)
              (Some kv1))
      in
      check_result "merge kv1" (Ok (Some kv1)) kv1';
      let* kv2' =
        with_contents repo (fun v ->
            Irmin.Merge.f (P.Contents.merge v) ~old:(old (Some kv1)) (Some kv1)
              (Some kv2))
      in
      check_result "merge kv2" (Ok (Some kv2)) kv2';

      (* merge nodes *)
      let g = g repo in
      (* The empty node *)
      let* k0 = with_node repo (fun g -> Graph.v g []) in
      (* Create the node t1 -x-> (v1) *)
      let* k1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      (* Create the node t2 -b-> t1 -x-> (v1) *)
      let* k2 = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      (* Create the node t3 -c-> t1 -x-> (v1) *)
      let* k3 = with_node repo (fun g -> Graph.v g [ ("c", `Node k1) ]) in
      (* Should create the node:
                          t4 -b-> t1 -x-> (v1)
                             \c/ *)
      let* k4 =
        with_node repo (fun g ->
            Irmin.Merge.(f @@ P.Node.merge g)
              ~old:(old (Some k0)) (Some k2) (Some k3))
      in
      let* k4 = merge_exn "k4" k4 in
      let k4 = match k4 with Some k -> k | None -> failwith "k4" in
      let _ = k4 in
      let succ_t = [%typ: string * Graph.value] in
      let* succ = Graph.list g k4 in
      checks succ_t "k4" [ ("b", `Node k1); ("c", `Node k1) ] succ;
      let info date =
        let i = Int64.of_int date in
        Irmin.Info.v ~date:i ~author:"test" "Test commit"
      in
      let c = P.Repo.commit_t repo in
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let* kr0, _ = with_info 0 (History.v ~node:k0 ~parents:[]) in
      let* kr1, _ = with_info 1 (History.v ~node:k2 ~parents:[ kr0 ]) in
      let* kr2, _ = with_info 2 (History.v ~node:k3 ~parents:[ kr0 ]) in
      may_get_hashes repo [ kr1; kr2 ] hook >>= fun () ->
      let* kr3 =
        with_info 3 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr0) kr1 kr2)
      in
      let* kr3 = merge_exn "kr3" kr3 in
      may_get_hashes repo [ kr3 ] hook >>= fun () ->
      let* kr3_id' =
        with_info 4 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr2) kr2 kr3)
      in
      let* kr3_id' = merge_exn "kr3_id'" kr3_id' in
      check S.Hash.t "kr3 id with immediate parent'" kr3 kr3_id';
      let* kr3_id =
        with_info 5 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr0) kr0 kr3)
      in
      let* kr3_id = merge_exn "kr3_id" kr3_id in
      check S.Hash.t "kr3 id with old parent" kr3 kr3_id;
      let* kr3', _ = with_info 3 @@ History.v ~node:k4 ~parents:[ kr1; kr2 ] in
      let* r3 = P.Commit.find c kr3 in
      let* r3' = P.Commit.find c kr3' in
      check T.(option P.Commit.Val.t) "r3" r3 r3';
      check S.Hash.t "kr3" kr3 kr3';
      P.Repo.close repo
    in
    run x test

  let test_history ?hook x () =
    let test repo =
      let info date =
        let i = Int64.of_int date in
        Irmin.Info.v ~date:i ~author:"test" "Test commit"
      in
      let assert_lcas_err msg err l2 =
        let err_str = function
          | `Too_many_lcas -> "Too_many_lcas"
          | `Max_depth_reached -> "Max_depth_reached"
        in
        let pp_commits = Fmt.Dump.(list S.Commit.pp_hash) in
        let l2 =
          match l2 with
          | Ok x -> Alcotest.failf "%s: %a" msg pp_commits x
          | Error e -> err_str e
        in
        Alcotest.(check string) msg (err_str err) l2
      in
      let assert_lcas msg l1 l2 =
        let l2 =
          match l2 with
          | Ok x -> x
          | Error `Too_many_lcas -> Alcotest.failf "%s: Too many LCAs" msg
          | Error `Max_depth_reached ->
              Alcotest.failf "%s: max depth reached" msg
        in
        checks (S.commit_t repo) msg l1 l2
      in
      let assert_lcas msg ~max_depth n a b expected =
        let* a = S.of_commit a in
        let* b = S.of_commit b in
        let* lcas = S.lcas ~max_depth ~n a b in
        assert_lcas msg expected lcas;
        let* lcas = S.lcas ~max_depth:(max_depth - 1) ~n a b in
        let msg = Printf.sprintf "%s [max-depth=%d]" msg (max_depth - 1) in
        assert_lcas_err msg `Max_depth_reached lcas;
        Lwt.return_unit
      in
      let assert_last_modified msg ?depth ~n t key expected =
        let+ last = S.last_modified ?depth ~n t key in
        S.repo t |> fun repo ->
        let msg = Printf.sprintf "%s [n=%d]" msg n in
        checks (S.commit_t repo) msg expected last
      in
      let assert_history_empty msg c expected =
        let* t = S.of_commit c in
        S.history t
        >|= S.History.is_empty
        >|= Alcotest.(check bool) msg expected
      in
      let tree = S.Tree.empty in
      let k0 = random_path ~label:8 ~path:5 in
      let k1 = random_path ~label:8 ~path:4 in
      let k2 = random_path ~label:8 ~path:6 in

      (* test that we don't compute too many lcas

         0(k0, k1) -> 1(k1) -> 2(k0) -> 3(k1, k0) -> 4(k1)
      *)
      let* tree = S.Tree.add tree k0 (random_value 1024) in
      let* tree = S.Tree.add tree k1 (random_value 1024) in
      let* c0 = S.Commit.v repo ~info:(info 0) ~parents:[] tree in
      may repo [ c0 ] hook >>= fun () ->
      assert_history_empty "nonempty 1 commit" c0 false >>= fun () ->
      let* tree = S.Tree.add tree k1 (random_value 1024) in
      let* c1 =
        S.Commit.v repo ~info:(info 1) ~parents:[ S.Commit.hash c0 ] tree
      in
      assert_history_empty "nonempty 2 commits" c0 false >>= fun () ->
      let* tree = S.Tree.add tree k0 (random_value 1024) in
      let* c2 =
        S.Commit.v repo ~info:(info 2) ~parents:[ S.Commit.hash c1 ] tree
      in
      let* tree = S.Tree.add tree k0 (random_value 1024) in
      let* tree = S.Tree.add tree k1 (random_value 1024) in
      let* c3 =
        S.Commit.v repo ~info:(info 3) ~parents:[ S.Commit.hash c2 ] tree
      in
      may repo [ c3 ] hook >>= fun () ->
      let* tree = S.Tree.add tree k1 (random_value 1024) in
      let* c4 =
        S.Commit.v repo ~info:(info 4) ~parents:[ S.Commit.hash c3 ] tree
      in
      assert_lcas "line lcas 1" ~max_depth:0 3 c3 c4 [ c3 ] >>= fun () ->
      assert_lcas "line lcas 2" ~max_depth:1 3 c2 c4 [ c2 ] >>= fun () ->
      assert_lcas "line lcas 3" ~max_depth:2 3 c1 c4 [ c1 ] >>= fun () ->
      let* store = S.of_commit c4 in
      let* () =
        assert_last_modified "line last_modified 1" ~n:1 store k0 [ c3 ]
      in
      let* () =
        assert_last_modified "line last_modified 2" ~n:2 store k0 [ c2; c3 ]
      in
      let* () =
        assert_last_modified "line last_modified 3" ~n:3 store k0 [ c0; c2; c3 ]
      in
      let* () =
        assert_last_modified "line last_modified 4" ~depth:1 ~n:3 store k0
          [ c3 ]
      in
      assert_last_modified "line last_modified 5" ~n:1 store k2 [] >>= fun () ->
      let* () =
        assert_last_modified "line last_modified 5" ~depth:0 ~n:2 store k0 []
      in
      (* test for multiple lca

         4(k1) -> 10 (k2) ---> 11(k0, k2) --> 13(k1) --> 15(k1, k2)
                  |                 \_______________________/____
                  |           _____________________/             \
                  |          /                                   \
                  \---> 12 (k0, k1) --> 14 (k2) --> 16 (k2) --> 17 (k0)
      *)
      let* tree = S.Tree.add tree k2 (random_value 1024) in
      let* c10 =
        S.Commit.v repo ~info:(info 10) ~parents:[ S.Commit.hash c4 ] tree
      in
      let* tree_up = S.Tree.add tree k0 (random_value 1024) in
      let* tree_up = S.Tree.add tree_up k2 (random_value 1024) in
      let* c11 =
        S.Commit.v repo ~info:(info 11) ~parents:[ S.Commit.hash c10 ] tree_up
      in
      let* tree_down = S.Tree.add tree k0 (random_value 1024) in
      let* tree_12 = S.Tree.add tree_down k1 (random_value 1024) in
      let* c12 =
        S.Commit.v repo ~info:(info 12) ~parents:[ S.Commit.hash c10 ] tree_12
      in
      let* tree_up = S.Tree.add tree_up k1 (random_value 1024) in
      let* c13 =
        S.Commit.v repo ~info:(info 13) ~parents:[ S.Commit.hash c11 ] tree_up
      in
      let* tree_down = S.Tree.add tree_12 k2 (random_value 1024) in
      let* c14 =
        S.Commit.v repo ~info:(info 14) ~parents:[ S.Commit.hash c12 ] tree_down
      in
      let* tree_up = S.Tree.add tree_12 k1 (random_value 1024) in
      let* tree_up = S.Tree.add tree_up k2 (random_value 1024) in
      let* c15 =
        S.Commit.v repo ~info:(info 15)
          ~parents:[ S.Commit.hash c12; S.Commit.hash c13 ]
          tree_up
      in
      let* tree_down = S.Tree.add tree_down k2 (random_value 1024) in
      let* c16 =
        S.Commit.v repo ~info:(info 16) ~parents:[ S.Commit.hash c14 ] tree_down
      in
      let* tree_down = S.Tree.add tree_down k0 (random_value 1024) in
      let* c17 =
        S.Commit.v repo ~info:(info 17)
          ~parents:[ S.Commit.hash c11; S.Commit.hash c16 ]
          tree_down
      in
      assert_lcas "x lcas 0" ~max_depth:0 5 c10 c10 [ c10 ] >>= fun () ->
      assert_lcas "x lcas 1" ~max_depth:0 5 c14 c14 [ c14 ] >>= fun () ->
      assert_lcas "x lcas 2" ~max_depth:0 5 c10 c11 [ c10 ] >>= fun () ->
      assert_lcas "x lcas 3" ~max_depth:1 5 c12 c16 [ c12 ] >>= fun () ->
      assert_lcas "x lcas 4" ~max_depth:1 5 c10 c13 [ c10 ] >>= fun () ->
      assert_lcas "x lcas 5" ~max_depth:2 5 c13 c14 [ c10 ] >>= fun () ->
      assert_lcas "x lcas 6" ~max_depth:3 5 c15 c16 [ c12 ] >>= fun () ->
      assert_lcas "x lcas 7" ~max_depth:3 5 c15 c17 [ c11; c12 ] >>= fun () ->
      let* store = S.of_commit c17 in
      let* () =
        assert_last_modified "x last_modified 1" ~n:3 store k0 [ c11; c12; c17 ]
      in
      let* () =
        assert_last_modified "x last_modified 2" ~n:1 store k2 [ c16 ]
      in
      let* () =
        assert_last_modified "x last_modified 3" ~n:2 store k1 [ c4; c12 ]
      in
      let* () =
        assert_last_modified "x last_modified 4" ~depth:3 ~n:5 store k1
          [ c4; c12 ]
      in
      let* () =
        assert_last_modified "x last_modified 5" ~depth:2 ~n:3 store k0
          [ c11; c17 ]
      in
      (* lcas on non transitive reduced graphs

                  /->16
                 |
         4->10->11->12->13->14->15
                 |        \--|--/
                 \-----------/
      *)
      let* c10 =
        S.Commit.v repo ~info:(info 10) ~parents:[ S.Commit.hash c4 ] tree
      in
      let* c11 =
        S.Commit.v repo ~info:(info 11) ~parents:[ S.Commit.hash c10 ] tree
      in
      let* c12 =
        S.Commit.v repo ~info:(info 12) ~parents:[ S.Commit.hash c11 ] tree
      in
      let* c13 =
        S.Commit.v repo ~info:(info 13) ~parents:[ S.Commit.hash c12 ] tree
      in
      let* c14 =
        S.Commit.v repo ~info:(info 14)
          ~parents:[ S.Commit.hash c11; S.Commit.hash c13 ]
          tree
      in
      let* c15 =
        S.Commit.v repo ~info:(info 15)
          ~parents:[ S.Commit.hash c13; S.Commit.hash c14 ]
          tree
      in
      let* c16 =
        S.Commit.v repo ~info:(info 16) ~parents:[ S.Commit.hash c11 ] tree
      in
      assert_lcas "weird lcas 1" ~max_depth:0 3 c14 c15 [ c14 ] >>= fun () ->
      assert_lcas "weird lcas 2" ~max_depth:0 3 c13 c15 [ c13 ] >>= fun () ->
      assert_lcas "weird lcas 3" ~max_depth:1 3 c12 c15 [ c12 ] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:1 3 c11 c15 [ c11 ] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:3 3 c15 c16 [ c11 ] >>= fun () ->
      (* fast-forward *)
      let ff = testable Irmin.Type.(result unit S.ff_error_t) in
      let* t12 = S.of_commit c12 in
      let* b1 = S.Head.fast_forward t12 c16 in
      Alcotest.(check ff) "ff 1.1" (Error `Rejected) b1;
      let* k12' = S.Head.get t12 in
      check (S.commit_t repo) "ff 1.2" c12 k12';
      let* b2 = S.Head.fast_forward t12 ~n:1 c14 in
      Alcotest.(check ff) "ff 2.1" (Error `Rejected) b2;
      let* k12'' = S.Head.get t12 in
      check (S.commit_t repo) "ff 2.2" c12 k12'';
      let* b3 = S.Head.fast_forward t12 c14 in
      Alcotest.(check ff) "ff 2.2" (Ok ()) b3;
      let* c14' = S.Head.get t12 in
      check (S.commit_t repo) "ff 2.3" c14 c14';
      P.Repo.close repo
    in
    run x test

  let test_empty ?hook x () =
    let test repo =
      let* t = S.empty repo in
      let* h = S.Head.find t in
      check T.(option @@ S.commit_t repo) "empty" None h;
      let* r1 = r1 ~repo in
      may repo [ r1 ] hook >>= fun () ->
      S.set_exn t ~info:Irmin.Info.none [ "b"; "x" ] v1 >>= fun () ->
      let* h = S.Head.find t in
      check T.(option @@ S.commit_t repo) "not empty" (Some r1) h;
      P.Repo.close repo
    in
    run x test

  let test_slice ?hook x () =
    let test repo =
      let* t = S.master repo in
      let a = "" in
      let b = "haha" in
      S.set_exn t ~info:(infof "slice") [ "x"; "a" ] a >>= fun () ->
      S.set_exn t ~info:(infof "slice") [ "x"; "b" ] b >>= fun () ->
      may_with_branch [ t ] repo hook >>= fun () ->
      let* slice = S.Repo.export repo in
      let str = T.to_json_string P.Slice.t slice in
      let slice' =
        match T.decode_json P.Slice.t (Jsonm.decoder (`String str)) with
        | Ok t -> t
        | Error (`Msg e) -> Alcotest.failf "decoding error: %s" e
      in
      check P.Slice.t "slices" slice slice';
      P.Repo.close repo
    in
    run x test

  let test_private_nodes ?hook x () =
    let test repo =
      let check_val = check [%typ: S.contents option] in
      let vx = "VX" in
      let vy = "VY" in
      let* t = S.master repo in
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] vx >>= fun () ->
      let* tree = S.get_tree t [ "x" ] in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree >>= fun () ->
      let* vx' = S.find t [ "u"; "y"; "z" ] in
      check_val "vx" (Some vx) vx';
      let* tree1 = S.get_tree t [ "u" ] in
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] vy >>= fun () ->
      may_with_branch [ t ] repo hook >>= fun () ->
      let* tree2 = S.get_tree t [ "u" ] in
      let* tree3 = S.Tree.add tree [ "x"; "z" ] vx in
      let* v' =
        Irmin.Merge.f S.Tree.merge ~old:(Irmin.Merge.promise tree1) tree2 tree3
        >>= merge_exn "tree"
      in
      S.set_tree_exn t ~info:(infof "merge") [ "u" ] v' >>= fun () ->
      let* vy' = S.find t [ "u"; "x"; "y" ] in
      check_val "vy after merge" (Some vy) vy';
      let* vx' = S.find t [ "u"; "x"; "z" ] in
      check_val "vx after merge" (Some vx) vx';
      P.Repo.close repo
    in
    run x test

  let test_stores x () =
    let test repo =
      let check_val = check [%typ: S.contents option] in
      let check_list = checks [%typ: S.Key.step * S.tree] in
      let* t = S.master repo in
      S.set_exn t ~info:(infof "init") [ "a"; "b" ] v1 >>= fun () ->
      let* b0 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem0" true b0;
      let* t = S.clone ~src:t ~dst:"test" in
      let* b1 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem1" true b1;
      let* b2 = S.mem t [ "a" ] in
      Alcotest.(check bool) "mem2" false b2;
      let* v1' = S.find t [ "a"; "b" ] in
      check_val "v1.1" (Some v1) v1';
      let* r1 = S.Head.get t in
      let* t = S.clone ~src:t ~dst:"test" in
      S.set_exn t ~info:(infof "update") [ "a"; "c" ] v2 >>= fun () ->
      let* b1 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem3" true b1;
      let* b2 = S.mem t [ "a" ] in
      Alcotest.(check bool) "mem4" false b2;
      let* v1' = S.find t [ "a"; "b" ] in
      check_val "v1.1" (Some v1) v1';
      let* b1 = S.mem t [ "a"; "c" ] in
      Alcotest.(check bool) "mem5" true b1;
      let* v2' = S.find t [ "a"; "c" ] in
      check_val "v1.1" (Some v2) v2';
      S.remove_exn t ~info:(infof "remove") [ "a"; "b" ] >>= fun () ->
      let* v1'' = S.find t [ "a"; "b" ] in
      check_val "v1.2" None v1'';
      S.Head.set t r1 >>= fun () ->
      let* v1'' = S.find t [ "a"; "b" ] in
      check_val "v1.3" (Some v1) v1'';
      let* ks = S.list t [ "a" ] in
      check_list "path" [ ("b", contents v1) ] ks;
      let* () =
        S.set_exn t ~info:(infof "update2") [ "a"; long_random_ascii_string ] v1
      in
      S.remove_exn t ~info:(infof "remove rec") [ "a" ] >>= fun () ->
      let* dirs = S.list t [] in
      check_list "remove rec" [] dirs;
      let* () =
        Lwt.catch
          (fun () ->
            S.set_exn t ~info:(infof "update root") [] v1 >>= fun () ->
            Alcotest.fail "update root")
          (function
            | Invalid_argument _ -> Lwt.return_unit
            | e -> Alcotest.fail ("update root: " ^ Printexc.to_string e))
      in
      let* none = S.find t [] in
      check_val "read root" none None;
      S.set_exn t ~info:(infof "update") [ "a" ] v1 >>= fun () ->
      S.remove_exn t ~info:(infof "remove rec --all") [] >>= fun () ->
      let* dirs = S.list t [] in
      check_list "remove rec root" [] dirs;
      let a = "ok" in
      let b = "maybe?" in
      S.set_exn t ~info:(infof "fst one") [ "fst" ] a >>= fun () ->
      S.set_exn t ~info:(infof "snd one") [ "fst"; "snd" ] b >>= fun () ->
      let* fst = S.find t [ "fst" ] in
      check_val "data model 1" None fst;
      let* snd = S.find t [ "fst"; "snd" ] in
      check_val "data model 2" (Some b) snd;
      S.set_exn t ~info:(infof "fst one") [ "fst" ] a >>= fun () ->
      let* fst = S.find t [ "fst" ] in
      check_val "data model 3" (Some a) fst;
      let* snd = S.find t [ "fst"; "snd" ] in
      check_val "data model 4" None snd;
      let tagx = "x" in
      let tagy = "y" in
      let xy = [ "x"; "y" ] in
      let vx = "VX" in
      let* tx = S.of_branch repo tagx in
      S.Branch.remove repo tagx >>= fun () ->
      S.Branch.remove repo tagy >>= fun () ->
      S.set_exn tx ~info:(infof "update") xy vx >>= fun () ->
      let* ty = S.clone ~src:tx ~dst:tagy in
      let* vx' = S.find ty xy in
      check_val "update tag" (Some vx) vx';
      S.status tx |> fun tagx' ->
      S.status ty |> fun tagy' ->
      check (S.Status.t repo) "tagx" (`Branch tagx) tagx';
      check (S.Status.t repo) "tagy" (`Branch tagy) tagy';
      let* t = S.master repo in
      S.Repo.close repo >>= fun () ->
      Lwt.catch
        (fun () ->
          let+ _ = S.set_exn t ~info:(infof "add after close") [ "a" ] "bar" in
          Alcotest.fail "Add after close should not be allowed")
        (function Irmin.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)
    in
    run x test

  let stats_t = Alcotest.testable (Irmin.Type.pp_dump S.Tree.stats_t) ( = )

  let empty_stats =
    { S.Tree.nodes = 0; leafs = 0; skips = 0; depth = 0; width = 0 }

  let inspect =
    Alcotest.testable
      (fun ppf -> function
        | `Contents -> Fmt.string ppf "contents"
        | `Node `Hash -> Fmt.string ppf "hash"
        | `Node `Map -> Fmt.string ppf "map"
        | `Node `Value -> Fmt.string ppf "value")
      ( = )

  let test_tree_caches x () =
    let test repo =
      let info = Irmin.Info.none in
      let* t1 = S.master repo in
      S.set_exn t1 ~info [ "a"; "b" ] "foo" >>= fun () ->
      (* Testing cache *)
      S.Tree.reset_counters ();
      let* v = S.get_tree t1 [] in
      Alcotest.(check inspect) "inspect" (`Node `Hash) (S.Tree.inspect v);
      let* v = S.Tree.add v [ "foo" ] "foo" in
      Alcotest.(check inspect) "inspect:0" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:0" 0 (S.Tree.counters ()).node_val_v;
      let* v = S.Tree.add v [ "bar"; "foo" ] "bar" in
      Alcotest.(check inspect) "inspect:1" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:1" 0 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:1" 0 (S.Tree.counters ()).node_val_list;
      let _ = S.Tree.hash v in
      Alcotest.(check inspect) "inspect:2" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:2" 0 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:2" 0 (S.Tree.counters ()).node_val_list;
      S.set_tree_exn t1 ~info [] v >>= fun () ->
      Alcotest.(check inspect) "inspect:3" (`Node `Hash) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:3" 0 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:3" 0 (S.Tree.counters ()).node_val_list;

      (* Test caching (makesure that no tree is lying in scope) *)
      let v0 = S.Tree.shallow repo (`Node (P.Contents.Key.hash "foo-x")) in
      S.Tree.reset_counters ();
      let foo = "foo-x" in
      let* v0 = S.Tree.add v0 [ "foo" ] foo in
      (* 2 calls to Node.find because v0 is a shallow tree *)
      Alcotest.(check int) "1 Node.find" 2 (S.Tree.counters ()).node_find;

      (* cache is filled whenever we hash something *)
      let _ = S.Tree.hash v0 in
      let* _v0 = S.Tree.add v0 [ "foo" ] foo in
      let _k = S.Tree.hash v0 in
      let v0 = S.Tree.shallow repo (`Node (P.Contents.Key.hash "bar-x")) in
      let xxx = "xxx" in
      let yyy = "yyy" in
      let zzz = "zzz" in
      let* v0 = S.Tree.add v0 [ "a" ] xxx in
      S.set_tree_exn ~info t1 [] v0 >>= fun () ->
      let* v0 = S.get_tree t1 [] in
      let* v0 = S.Tree.add v0 [ "b" ] xxx in
      S.set_tree_exn ~info t1 [] v0 >>= fun () ->
      let* v0 = S.Tree.add v0 [ "c"; "d" ] yyy in
      let* v0 = S.Tree.add v0 [ "c"; "e"; "f" ] zzz in
      Alcotest.(check inspect) "inspect" (`Node `Value) (S.Tree.inspect v0);
      S.set_tree_exn ~info t1 [] v0 >>= fun () -> P.Repo.close repo
    in
    run x test

  let pp_depth = Irmin.Type.pp S.Tree.depth_t

  let test_trees x () =
    let test repo =
      let* t = S.master repo in
      let nodes = random_nodes 100 in
      let foo1 = random_value 10 in
      let foo2 = random_value 10 in
      (* Testing [Tree.remove] *)
      S.Tree.empty |> fun v1 ->
      let* v1 = S.Tree.add v1 [ "foo"; "toto" ] foo1 in
      let* v1 = S.Tree.add v1 [ "foo"; "bar"; "toto" ] foo2 in
      let* () =
        S.Tree.fold ~depth:(`Eq 1) ~force:`True S.Tree.empty ()
          ~contents:(fun k _ ->
            assert (List.length k = 1);
            Alcotest.fail "contents")
          ~node:(fun k _ ->
            assert (List.length k = 1);
            Alcotest.fail "node")
      in
      let fold depth ecs ens =
        let* cs, ns =
          S.Tree.fold v1 ?depth ~force:`And_clear
            ~contents:(fun path _ (cs, ns) -> Lwt.return (path :: cs, ns))
            ~node:(fun path _ (cs, ns) -> Lwt.return (cs, path :: ns))
            ([], [])
        in
        let paths = Alcotest.slist (testable S.Key.t) compare in
        Alcotest.(check paths)
          (Fmt.str "contents depth=%a" Fmt.(Dump.option pp_depth) depth)
          ecs cs;
        Alcotest.(check paths)
          (Fmt.str "nodes depth=%a" Fmt.(Dump.option pp_depth) depth)
          ens ns;
        Lwt.return ()
      in
      let* () =
        fold None
          [ [ "foo"; "bar"; "toto" ]; [ "foo"; "toto" ] ]
          [ []; [ "foo" ]; [ "foo"; "bar" ] ]
      in
      fold (Some (`Eq 0)) [] [ [] ] >>= fun () ->
      fold (Some (`Eq 1)) [] [ [ "foo" ] ] >>= fun () ->
      let* () =
        fold (Some (`Eq 2)) [ [ "foo"; "toto" ] ] [ [ "foo"; "bar" ] ]
      in
      fold (Some (`Lt 2)) [] [ []; [ "foo" ] ] >>= fun () ->
      let* () =
        fold
          (Some (`Le 2))
          [ [ "foo"; "toto" ] ]
          [ []; [ "foo" ]; [ "foo"; "bar" ] ]
      in
      let* () =
        fold
          (Some (`Ge 2))
          [ [ "foo"; "toto" ]; [ "foo"; "bar"; "toto" ] ]
          [ [ "foo"; "bar" ] ]
      in
      fold (Some (`Gt 2)) [ [ "foo"; "bar"; "toto" ] ] [] >>= fun () ->
      let* v1 = S.Tree.remove v1 [ "foo"; "bar"; "toto" ] in
      let* v = S.Tree.find v1 [ "foo"; "toto" ] in
      Alcotest.(check (option string)) "remove" (Some foo1) v;
      S.Tree.empty |> fun v1 ->
      let* s = S.Tree.stats v1 in
      Alcotest.(check stats_t) "empty stats" empty_stats s;
      let* v1 = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      let* v1 = S.Tree.add v1 [ "foo"; "2" ] foo2 in
      let* s = S.Tree.stats v1 in
      Alcotest.(check stats_t)
        "stats 1"
        { S.Tree.nodes = 2; leafs = 2; skips = 0; depth = 2; width = 2 }
        s;
      let* v1 = S.Tree.remove v1 [ "foo"; "1" ] in
      let* v1 = S.Tree.remove v1 [ "foo"; "2" ] in
      let* s = S.Tree.stats v1 in
      Alcotest.(check stats_t) "empty stats" empty_stats s;
      S.set_tree_exn t ~info:(infof "empty tree") [] v1 >>= fun () ->
      let* head = S.Head.get t in
      S.Commit.hash head |> fun head ->
      let* commit = P.Commit.find (ct repo) head in
      let node = P.Commit.Val.node (get commit) in
      let* node = P.Node.find (n repo) node in
      check T.(option P.Node.Val.t) "empty tree" (Some P.Node.Val.empty) node;

      (* Testing [Tree.diff] *)
      let contents_t = T.pair S.contents_t S.metadata_t in
      let diff = T.(pair S.key_t (Irmin.Diff.t contents_t)) in
      let check_diffs = checks diff in
      let check_val = check T.(option contents_t) in
      let check_ls = checks T.(pair S.step_t S.tree_t) in
      let normal c = Some (c, S.Metadata.default) in
      let d0 = S.Metadata.default in
      S.Tree.empty |> fun v0 ->
      S.Tree.empty |> fun v1 ->
      S.Tree.empty |> fun v2 ->
      let* v1 = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      let* f = S.Tree.find_all v1 [ "foo"; "1" ] in
      check_val "tree update" (normal foo1) f;
      let* v1' = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      Alcotest.(check bool) "Tree.add keeps sharing" true (v1 == v1');
      let* v1' = S.Tree.remove v1 [ "foo"; "2" ] in
      Alcotest.(check bool) "Tree.remove keeps sharing" true (v1 == v1');
      let* v1' = S.Tree.add_tree v1 [] v1 in
      Alcotest.(check bool) "Tree.add_tree keeps sharing" true (v1 == v1');
      let* v2 = S.Tree.add v2 [ "foo"; "1" ] foo2 in
      let* v2 = S.Tree.add v2 [ "foo"; "2" ] foo1 in
      let* d1 = S.Tree.diff v0 v1 in
      check_diffs "diff 1" [ ([ "foo"; "1" ], `Added (foo1, d0)) ] d1;
      let* d2 = S.Tree.diff v1 v0 in
      check_diffs "diff 2" [ ([ "foo"; "1" ], `Removed (foo1, d0)) ] d2;
      let* d3 = S.Tree.diff v1 v2 in
      check_diffs "diff 3"
        [
          ([ "foo"; "1" ], `Updated ((foo1, d0), (foo2, d0)));
          ([ "foo"; "2" ], `Added (foo1, d0));
        ]
        d3;
      let* v3 = S.Tree.add v2 [ "foo"; "bar"; "1" ] foo1 in
      let* d4 = S.Tree.diff v2 v3 in
      check_diffs "diff 4" [ ([ "foo"; "bar"; "1" ], `Added (foo1, d0)) ] d4;
      let* d5 = S.Tree.diff v3 v2 in
      check_diffs "diff 4" [ ([ "foo"; "bar"; "1" ], `Removed (foo1, d0)) ] d5;

      (* Testing paginated lists *)
      let tree =
        let c ?(info = S.Metadata.default) blob = `Contents (blob, info) in
        S.Tree.of_concrete
          (`Tree
            [
              ("aa", c "0");
              ("a", c "1");
              ("bbb", c "3");
              ("b", c "3");
              ("aaa", c "1");
            ])
      in
      let* _ = S.set_tree_exn t ~info:(infof "add tree") [] tree in
      let* e = S.Tree.get_tree tree [ "a" ] in
      let ls =
        [
          ("aa", contents "0");
          ("a", e);
          ("bbb", contents "3");
          ("b", contents "3");
          ("aaa", e);
        ]
      in
      let* () =
        let* l1 = S.Tree.list ~offset:0 ~length:2 tree [] in
        let* l2 = S.Tree.list ~offset:2 ~length:2 tree [] in
        let+ l3 = S.Tree.list ~offset:4 ~length:2 tree [] in
        Alcotest.(check int) "size l1" 2 (List.length l1);
        Alcotest.(check int) "size l2" 2 (List.length l2);
        Alcotest.(check int) "size l3" 1 (List.length l3);
        check_ls "2 paginated list" ls (l1 @ l2 @ l3)
      in
      let* () =
        let* l1 = S.Tree.list ~offset:0 ~length:3 tree [] in
        let+ l2 = S.Tree.list ~offset:3 ~length:6 tree [] in
        Alcotest.(check int) "size l1" 3 (List.length l1);
        Alcotest.(check int) "size l2" 2 (List.length l2);
        check_ls "3 paginated list" ls (l1 @ l2)
      in
      let* () =
        let* l1 = S.Tree.list ~offset:0 ~length:4 tree [] in
        let+ l2 = S.Tree.list ~offset:4 ~length:4 tree [] in
        Alcotest.(check int) "size l1" 4 (List.length l1);
        Alcotest.(check int) "size l2" 1 (List.length l2);
        check_ls "4 paginated list" ls (l1 @ l2)
      in
      let* () =
        let* l1 = S.Tree.list ~offset:0 ~length:5 tree [] in
        let+ l2 = S.Tree.list ~offset:5 ~length:5 tree [] in
        Alcotest.(check int) "size l1" 5 (List.length l1);
        Alcotest.(check int) "size l2" 0 (List.length l2);
        check_ls "5 paginated list" ls (l1 @ l2)
      in
      let c0 = S.Tree.empty in
      let* c0 = S.Tree.add c0 [ "foo"; "a" ] "1" in
      let* c0 = S.Tree.add c0 [ "foo"; "b"; "c" ] "2" in
      let* c0 = S.Tree.add c0 [ "foo"; "c" ] "3" in
      let* c0 = S.Tree.add c0 [ "foo"; "d" ] "4" in
      let* b = S.Tree.get_tree c0 [ "foo"; "b" ] in
      let* ls = S.Tree.list c0 [ "foo" ] in
      check_ls "list all"
        [
          ("a", contents "1"); ("b", b); ("c", contents "3"); ("d", contents "4");
        ]
        ls;
      let* ls = S.Tree.list ~offset:2 c0 [ "foo" ] in
      check_ls "list offset=2" [ ("c", contents "3"); ("d", contents "4") ] ls;
      let* ls = S.Tree.list ~offset:2 ~length:1 c0 [ "foo" ] in
      check_ls "list offset=2 length=1" [ ("c", contents "3") ] ls;
      let* ls = S.Tree.list ~length:1 c0 [ "foo" ] in
      check_ls "list length=1" [ ("a", contents "1") ] ls;

      (* Testing concrete representation *)
      let* c0 =
        Lwt.return S.Tree.empty
        >>= with_binding [ "foo"; "a" ] "1"
        >>= with_binding [ "foo"; "b"; "c" ] "2"
        >>= with_binding [ "bar"; "d" ] "3"
        >>= with_binding [ "e" ] "4"
      in
      let* t0 = c0 |> S.Tree.to_concrete >|= S.Tree.of_concrete in
      let* () =
        let+ d0 = S.Tree.diff c0 t0 in
        check_diffs "concrete roundtrip" [] d0
      in
      let* () =
        let* c0' = S.Tree.list c0 [] in
        let+ t0' = S.Tree.list t0 [] in
        check_ls "concrete list /" c0' t0'
      in
      let* () =
        let* c0' = S.Tree.list c0 [ "foo" ] in
        let+ t0' = S.Tree.list t0 [ "foo" ] in
        check_ls "concrete tree list /foo" c0' t0'
      in
      let* () =
        let* c0' = S.Tree.list c0 [ "bar"; "d" ] in
        let+ t0' = S.Tree.list t0 [ "bar"; "d" ] in
        check_ls "concrete tree list /bar/d" c0' t0'
      in

      (* Testing other tree operations. *)
      S.Tree.empty |> fun v0 ->
      let* c = S.Tree.to_concrete v0 in
      (match c with
      | `Tree [] -> ()
      | _ -> Alcotest.fail "Excpected empty tree");
      let* v0 = S.Tree.add v0 [] foo1 in
      let* foo1' = S.Tree.find_all v0 [] in
      check_val "read /" (normal foo1) foo1';
      let* v0 = S.Tree.add v0 [ "foo"; "1" ] foo1 in
      let* foo1' = S.Tree.find_all v0 [ "foo"; "1" ] in
      check_val "read foo/1" (normal foo1) foo1';
      let* v0 = S.Tree.add v0 [ "foo"; "2" ] foo2 in
      let* foo2' = S.Tree.find_all v0 [ "foo"; "2" ] in
      check_val "read foo/2" (normal foo2) foo2';
      let check_tree v =
        let* ls = S.Tree.list v [ "foo" ] in
        check_ls "path1" [ ("1", contents foo1); ("2", contents foo2) ] ls;
        let* foo1' = S.Tree.find_all v [ "foo"; "1" ] in
        check_val "foo1" (normal foo1) foo1';
        let* foo2' = S.Tree.find_all v [ "foo"; "2" ] in
        check_val "foo2" (normal foo2) foo2';
        Lwt.return_unit
      in
      let* v0 =
        Lwt_list.fold_left_s (fun v0 (k, v) -> S.Tree.add v0 k v) v0 nodes
      in
      check_tree v0 >>= fun () ->
      S.set_tree_exn t ~info:(infof "update_path b/") [ "b" ] v0 >>= fun () ->
      S.set_tree_exn t ~info:(infof "update_path a/") [ "a" ] v0 >>= fun () ->
      let* ls = S.list t [ "b"; "foo" ] in
      check_ls "path2" [ ("1", contents foo1); ("2", contents foo2) ] ls;
      let* foo1' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "foo1" (normal foo1) foo1';
      let* foo2' = S.find_all t [ "a"; "foo"; "2" ] in
      check_val "foo2" (normal foo2) foo2';
      let* v0 = S.get_tree t [ "b" ] in
      check_tree v0 >>= fun () ->
      S.set_exn t ~info:(infof "update b/x") [ "b"; "x" ] foo1 >>= fun () ->
      let* v2 = S.get_tree t [ "b" ] in
      let* v1 = S.Tree.add v0 [ "y" ] foo2 in
      let* v' =
        Irmin.Merge.(f S.Tree.merge ~old:(promise v0) v1 v2)
        >>= merge_exn "merge trees"
      in
      S.set_tree_exn t ~info:(infof "merge_path") [ "b" ] v' >>= fun () ->
      let* foo1' = S.find_all t [ "b"; "x" ] in
      let* foo2' = S.find_all t [ "b"; "y" ] in
      check_val "merge: b/x" (normal foo1) foo1';
      check_val "merge: b/y" (normal foo2) foo2';
      let* () =
        Lwt_list.iteri_s
          (fun i (k, v) ->
            let* v' = S.find_all t ("a" :: k) in
            check_val ("a" ^ string_of_int i) (normal v) v';
            let* v' = S.find_all t ("b" :: k) in
            check_val ("b" ^ string_of_int i) (normal v) v';
            Lwt.return_unit)
          nodes
      in
      let* v2 = S.get_tree t [ "b" ] in
      let* _ = S.Tree.find_all v2 [ "foo"; "1" ] in
      let* v2 = S.Tree.add v2 [ "foo"; "1" ] foo2 in
      S.set_tree_exn t ~info:(infof "v2") [ "b" ] v2 >>= fun () ->
      let* foo2' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "update tree" (normal foo2) foo2';
      let* v3 = S.get_tree t [ "b" ] in
      let* _ = S.Tree.find_all v3 [ "foo"; "1" ] in
      let* v3 = S.Tree.remove v3 [ "foo"; "1" ] in
      S.set_tree_exn t ~info:(infof "v3") [ "b" ] v3 >>= fun () ->
      let* foo2' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "remove tree" None foo2';
      let* r1 = r1 ~repo in
      let* r2 = r2 ~repo in
      let i0 = Irmin.Info.empty in
      let* c =
        S.Commit.v repo ~info:Irmin.Info.empty
          ~parents:[ S.Commit.hash r1; S.Commit.hash r2 ]
          v3
      in
      S.Head.set t c >>= fun () ->
      let* h = S.Head.get t in
      S.Commit.info h |> fun i ->
      check Irmin.Info.t "commit info" i0 i;
      let* tt = S.of_commit h in
      let* g = S.history tt in
      let pred = S.History.pred g h in
      checks (S.commit_t repo) "head" [ r1; r2 ] pred;
      let* foo2'' = S.find_all tt [ "b"; "foo"; "1" ] in
      check_val "remove tt" None foo2'';
      let vx = "VX" in
      let px = [ "x"; "y"; "z" ] in
      S.set_exn tt ~info:(infof "update") px vx >>= fun () ->
      let* tree = S.get_tree tt [] in
      S.Tree.clear tree;
      let* s = S.Tree.stats tree in
      Alcotest.(check stats_t)
        "lazy stats"
        { S.Tree.nodes = 0; leafs = 0; skips = 1; depth = 0; width = 0 }
        s;
      S.Tree.clear tree;
      let* s = S.Tree.stats ~force:true tree in
      Alcotest.(check stats_t)
        "forced stats"
        { S.Tree.nodes = 404; leafs = 103; skips = 0; depth = 5; width = 103 }
        s;
      let* vx' = S.Tree.find_all tree px in
      check_val "updates" (normal vx) vx';
      S.Tree.empty |> fun v ->
      let* v = S.Tree.add v [] vx in
      let* () =
        S.set_tree_exn t ~info:(infof "update file as tree") [ "a" ] v
      in
      let* vx' = S.find_all t [ "a" ] in
      check_val "update file as tree" (normal vx) vx';
      P.Repo.close repo
    in
    run x test

  let test_wide_nodes x () =
    let test repo =
      let size = 500_000 in
      let c0 = S.Tree.empty in
      let rec wide_node i c =
        if i >= size then Lwt.return c
        else
          S.Tree.add c [ "foo"; string_of_int i ] (string_of_int i) >>= fun c ->
          wide_node (i + 1) c
      in
      wide_node 0 c0 >>= fun c ->
      S.Tree.list c [ "foo" ] >>= fun ls ->
      Alcotest.(check int) "list wide dir" size (List.length ls);
      S.Tree.fold ~force:`True c ~uniq:`False
        ~contents:(fun k _ i ->
          Alcotest.(check int) "contents at [foo; i]" (List.length k) 2;
          Lwt.return (i + 1))
        ~node:(fun k _ i ->
          if not (List.length k = 0 || List.length k = 1) then
            Alcotest.failf "nodes should be at [] and [foo], got %a"
              (Irmin.Type.pp S.key_t) k;
          Lwt.return i)
        0
      >>= fun nb_contents ->
      Alcotest.(check int) "nb of contents folded over" size nb_contents;
      S.Tree.remove c [ "foo"; "499999" ] >>= fun c1 ->
      S.Tree.add c0 [] "499999" >>= fun c2 ->
      S.Tree.add_tree c1 [ "foo"; "499999" ] c2 >>= fun c' ->
      let h' = S.Tree.hash c' in
      let h = S.Tree.hash c in
      check S.Hash.t "same tree" h h';
      S.Tree.get_tree c [ "foo" ] >>= fun c1 ->
      (match S.Tree.destruct c1 with
      | `Contents _ -> Alcotest.fail "got `Contents, expected `Node"
      | `Node node -> (
          S.to_private_node node >>= function
          | Ok v -> (
              let ls = P.Node.Val.list v in
              Alcotest.(check int) "list wide node" size (List.length ls);
              let k = normal (P.Contents.Key.hash "bar") in
              let v1 = P.Node.Val.add v "x" k in
              let h' = H_node.hash v1 in
              with_node repo (fun n -> P.Node.add n v1) >>= fun h ->
              check H_node.t "wide node + x: hash(v) = add(v)" h h';
              let v2 = P.Node.Val.add v "x" k in
              check P.Node.Val.t "add x" v1 v2;
              let v0 = P.Node.Val.remove v1 "x" in
              check P.Node.Val.t "remove x" v v0;
              let v3 = P.Node.Val.remove v "1" in
              let h' = H_node.hash v3 in
              with_node repo (fun n -> P.Node.add n v3) >|= fun h ->
              check H_node.t "wide node - 1 : hash(v) = add(v)" h h';
              (match P.Node.Val.find v "499999" with
              | None -> Alcotest.fail "value 499999 not found"
              | Some x ->
                  let x' = normal (P.Contents.Key.hash "499999") in
                  check P.Node.Val.value_t "find 499999" x x');
              match P.Node.Val.find v "500000" with
              | None -> ()
              | Some _ -> Alcotest.fail "value 500000 should not be found")
          | Error (`Dangling_hash _) ->
              Alcotest.fail "unexpected dangling hash in wide node"))
      >>= fun () -> P.Repo.close repo
    in
    run x test

  let test_commit_wide_node x () =
    let test repo =
      let size = 500_000 in
      let c0 = S.Tree.empty in
      let rec wide_node i c =
        if i >= size then Lwt.return c
        else
          S.Tree.add c [ "foo"; string_of_int i ] (string_of_int i) >>= fun c ->
          wide_node (i + 1) c
      in
      wide_node 0 c0 >>= fun c ->
      S.master repo >>= fun t ->
      S.set_tree_exn t [ "wide" ] ~info:(infof "commit_wide_nodes") c
      >>= fun () ->
      S.list t [ "wide"; "foo" ] >>= fun ls ->
      Alcotest.(check int) "commit wide node list" size (List.length ls);
      P.Repo.close repo
    in
    run x test

  module Sync = Irmin.Sync (S)

  let test_sync x () =
    let test repo =
      let* t1 = S.master repo in
      S.set_exn t1 ~info:(infof "update a/b") [ "a"; "b" ] v1 >>= fun () ->
      let* h = S.Head.get t1 in
      let* _r1 = S.Head.get t1 in
      S.set_exn t1 ~info:(infof "update a/c") [ "a"; "c" ] v2 >>= fun () ->
      let* r2 = S.Head.get t1 in
      S.set_exn t1 ~info:(infof "update a/d") [ "a"; "d" ] v1 >>= fun () ->
      let* _r3 = S.Head.get t1 in
      let* h = S.history t1 ~min:[ h ] in
      Alcotest.(check int) "history-v" 3 (S.History.nb_vertex h);
      Alcotest.(check int) "history-e" 2 (S.History.nb_edges h);
      let remote = Irmin.remote_store (module S) t1 in
      let* partial = Sync.fetch_exn t1 ~depth:0 remote in
      let partial =
        match partial with
        | `Head x -> x
        | `Empty -> failwith "no head: partial"
      in
      let* full = Sync.fetch_exn t1 remote in
      let full =
        match full with `Head x -> x | `Empty -> failwith "no head: full"
      in
      (* Restart a fresh store and import everything in there. *)
      let tag = "export" in
      let* t2 = S.of_branch repo tag in
      S.Head.set t2 partial >>= fun () ->
      let* b1 = S.mem t2 [ "a"; "b" ] in
      Alcotest.(check bool) "mem-ab" true b1;
      let* b2 = S.mem t2 [ "a"; "c" ] in
      Alcotest.(check bool) "mem-ac" true b2;
      let* b3 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ad" true b3;
      let* v1' = S.get t2 [ "a"; "d" ] in
      check S.contents_t "v1" v1 v1';
      S.Head.set t2 r2 >>= fun () ->
      let* b4 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ab" false b4;
      S.Head.set t2 full >>= fun () ->
      S.Head.set t2 r2 >>= fun () ->
      let* b4 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ad" false b4;
      P.Repo.close repo
    in
    run x test

  module Dot = Irmin.Dot (S)

  let output_file x t file =
    let buf = Buffer.create 1024 in
    let date d =
      let tm = Unix.localtime (Int64.to_float d) in
      Fmt.strf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Dot.output_buffer t ~date buf >>= fun () ->
    let oc =
      open_out_bin
        (Filename.get_temp_dir_name () / Fmt.str "%s-%s.dot" x.name file)
    in
    output_string oc (Buffer.contents buf);
    close_out oc;
    Lwt.return_unit

  let test_merge ?hook x () =
    let test repo =
      let v1 = "X1" in
      let v2 = "X2" in
      let v3 = "X3" in
      let* t1 = S.master repo in
      let* () =
        S.set_exn t1 ~info:(infof "update a/b/a") [ "a"; "b"; "a" ] v1
      in
      let* () =
        S.set_exn t1 ~info:(infof "update a/b/b") [ "a"; "b"; "b" ] v2
      in
      let* () =
        S.set_exn t1 ~info:(infof "update a/b/c") [ "a"; "b"; "c" ] v3
      in
      let test = "test" in
      let* t2 = S.clone ~src:t1 ~dst:test in
      let* () =
        S.set_exn t1 ~info:(infof "update master:a/b/b") [ "a"; "b"; "b" ] v1
      in
      let* () =
        S.set_exn t1 ~info:(infof "update master:a/b/b") [ "a"; "b"; "b" ] v3
      in
      let* () =
        S.set_exn t2 ~info:(infof "update test:a/b/c") [ "a"; "b"; "c" ] v1
      in
      output_file x t1 "before" >>= fun () ->
      let* m =
        S.merge_into ~info:(infof "merge test into master") t2 ~into:t1
      in
      merge_exn "m" m >>= fun () ->
      may_with_branch [ t1 ] repo hook >>= fun () ->
      output_file x t1 "after" >>= fun () ->
      let* v1' = S.get t1 [ "a"; "b"; "c" ] in
      let* v2' = S.get t2 [ "a"; "b"; "b" ] in
      let* v3' = S.get t1 [ "a"; "b"; "b" ] in
      check S.contents_t "v1" v1 v1';
      check S.contents_t "v2" v2 v2';
      check S.contents_t "v3" v3 v3';
      P.Repo.close repo
    in
    run x test

  (* in this test an outdated reference to a tree is used by a commit: [tree] is
     the tree with root [x] created by [c1] and modified by [c2]. [c3] reuse [tree]
     which implicitly deletes the changes of [c2]. *)
  let test_merge_outdated_tree x () =
    let check_val = check T.(option S.contents_t) in
    let none_fail f msg =
      f >>= function None -> Alcotest.fail msg | Some c -> Lwt.return c
    in
    let test repo =
      let vx = "VX" in
      let vy = "VY" in
      let old () = Lwt.return (Ok None) in
      let* t = S.master repo in
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] vx >>= fun () ->
      let* _c1 = none_fail (S.Head.find t) "head not found" in
      let* tree = S.get_tree t [ "x" ] in
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] vy >>= fun () ->
      let* c2 = none_fail (S.Head.find t) "head not found" in
      let* tree3 = S.Tree.add tree [ "x"; "z" ] vx in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree3 >>= fun () ->
      let* c3 = none_fail (S.Head.find t) "head not found" in
      let info () = S.Commit.info c3 in
      with_commit repo (fun commit_t ->
          Irmin.Merge.f
            (P.Commit.merge commit_t ~info)
            ~old
            (Some (S.Commit.hash c3))
            (Some (S.Commit.hash c2)))
      >>= merge_exn "commit"
      >>= function
      | None -> Lwt.return_unit
      | Some c4 ->
          let* k = none_fail (S.Commit.of_hash repo c4) "of hash" in
          S.Branch.set repo "foo" k >>= fun () ->
          let* t = S.of_branch repo "foo" in
          let* vy' = S.find t [ "u"; "x"; "y" ] in
          check_val "vy after merge" None vy';
          P.Repo.close repo
    in
    run x test

  let test_merge_unrelated ?hook x () =
    run x @@ fun repo ->
    let v1 = "X1" in
    let* foo = S.of_branch repo "foo" in
    let* bar = S.of_branch repo "bar" in
    S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1 >>= fun () ->
    S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1 >>= fun () ->
    may_with_branch [ foo; bar ] repo hook >>= fun () ->
    let* _ =
      S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo
      >>= merge_exn "merge unrelated"
    in
    P.Repo.close repo

  let rec write fn = function
    | 0 -> []
    | i -> (fun () -> fn i >>= Lwt_unix.yield) :: write fn (i - 1)

  let perform l = Lwt_list.iter_p (fun f -> f ()) l

  let rec read fn check = function
    | 0 -> []
    | i -> (fun () -> fn i >|= fun v -> check i v) :: read fn check (i - 1)

  let test_concurrent_low x () =
    let test_branches repo =
      let k = b1 in
      let* v = r1 ~repo in
      let write = write (fun _i -> S.Branch.set repo k v) in
      let read =
        read
          (fun _i -> S.Branch.find repo k >|= get)
          (fun i -> check (S.commit_t repo) (Fmt.strf "tag %d" i) v)
      in
      perform (write 1) >>= fun () ->
      perform (write 10 @ read 10 @ write 10 @ read 10)
    in
    let test_contents repo =
      let* k = kv2 ~repo in
      let v = v2 in
      let t = P.Repo.contents_t repo in
      let write =
        write (fun _i ->
            let* _ = with_contents repo (fun t -> P.Contents.add t v) in
            Lwt.return_unit)
      in
      let read =
        read
          (fun _i -> P.Contents.find t k >|= get)
          (fun i -> check S.contents_t (Fmt.strf "contents %d" i) v)
      in
      perform (write 1) >>= fun () ->
      perform (write 10 @ read 10 @ write 10 @ read 10)
    in
    run x (fun repo ->
        Lwt.choose [ test_branches repo; test_contents repo ] >>= fun () ->
        P.Repo.close repo)

  let test_concurrent_updates x () =
    let test_one repo =
      let k = [ "a"; "b"; "d" ] in
      let v = "X1" in
      let* t1 = S.master repo in
      let* t2 = S.master repo in
      let write t =
        write (fun i -> S.set_exn t ~info:(infof "update: one %d" i) k v)
      in
      let read t =
        read
          (fun _ -> S.get t k)
          (fun i -> check S.contents_t (Fmt.strf "update: one %d" i) v)
      in
      perform (write t1 10 @ write t2 10) >>= fun () -> perform (read t1 10)
    in
    let test_multi repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      let* t1 = S.master repo in
      let* t2 = S.master repo in
      let write t =
        write (fun i ->
            S.set_exn t ~info:(infof "update: multi %d" i) (k i) (v i))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      perform (write t1 10 @ write t2 10) >>= fun () -> perform (read t1 10)
    in
    run x (fun repo ->
        test_one repo >>= fun () ->
        test_multi repo >>= fun () -> P.Repo.close repo)

  let test_concurrent_merges x () =
    let test repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      let* t1 = S.master repo in
      let* t2 = S.master repo in
      let write t n =
        write (fun i ->
            let tag = Fmt.strf "tmp-%d-%d" n i in
            let* m = S.clone ~src:t ~dst:tag in
            S.set_exn m ~info:(infof "update") (k i) (v i) >>= fun () ->
            Lwt_unix.yield () >>= fun () ->
            S.merge_into ~info:(infof "update: multi %d" i) m ~into:t
            >>= merge_exn "update: multi")
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0) >>= fun () ->
      perform (write t1 1 10 @ write t2 2 10) >>= fun () ->
      perform (read t1 10) >>= fun () -> P.Repo.close repo
    in
    run x test

  let pp_write_error = Irmin.Type.pp S.write_error_t
  let tree_t = testable S.tree_t

  let test_with_tree x () =
    let test repo =
      let* t = S.master repo in
      let update ?retries key strategy r w =
        S.with_tree t ?retries ~info:(infof "with-tree") ~strategy key (fun _ ->
            let+ v = Lwt_mvar.take r in
            Some (S.Tree.of_contents v))
        >>= Lwt_mvar.put w
      in
      let check_ok = function
        | Ok () -> ()
        | Error e -> Alcotest.failf "%a" pp_write_error e
      in
      let check_test e = function
        | Error (`Test_was e') ->
            Alcotest.(check (option tree_t)) "test-was" e e'
        | Ok () -> Alcotest.fail "error expected"
        | Error e ->
            Alcotest.failf "an other error was expected: %a" pp_write_error e
      in
      let check_conflict = function
        | Error (`Conflict _) -> ()
        | Ok () -> Alcotest.fail "error expected"
        | Error e ->
            Alcotest.failf "an other error was expected: %a" pp_write_error e
      in
      let set () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") [ "a" ] "0" >>= fun () ->
        Lwt.join
          [
            update [ "a" ] ~retries:0 `Set rx wx;
            update [ "a" ] ~retries:0 `Set ry wy;
            ( Lwt_mvar.put rx "1" >>= fun () ->
              Lwt_mvar.take wx >|= check_ok >>= fun () ->
              let* a = S.get t [ "a" ] in
              Alcotest.(check string) "set x" "1" a;
              Lwt_mvar.put ry "2" >>= fun () ->
              Lwt_mvar.take wy >|= check_ok >>= fun () ->
              let+ a = S.get t [ "a" ] in
              Alcotest.(check string) "set y" "2" a );
          ]
      in
      let test_and_set () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        let rz = Lwt_mvar.create_empty () in
        let wz = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") [ "a" ] "0" >>= fun () ->
        Lwt.join
          [
            update [ "a" ] ~retries:0 `Test_and_set rx wx;
            update [ "a" ] ~retries:0 `Test_and_set ry wy;
            update [ "a" ] ~retries:1 `Test_and_set rz wz;
            ( Lwt_mvar.put rx "1" >>= fun () ->
              Lwt_mvar.take wx >|= check_ok >>= fun () ->
              let* a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set x" "1" a;
              Lwt_mvar.put ry "2" >>= fun () ->
              let* e = Lwt_mvar.take wy in
              check_test (Some (S.Tree.of_contents "1")) e;
              let* a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set y" "1" a;
              Lwt_mvar.put rz "3" >>= fun () ->
              (* there's a conflict, the transaction is restarted so need to feed a
                 new value *)
              Lwt_mvar.put rz "4" >>= fun () ->
              Lwt_mvar.take wz >|= check_ok >>= fun () ->
              let+ a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set z" "4" a );
          ]
      in
      let merge () =
        let rx = Lwt_mvar.create_empty () in
        let wx = Lwt_mvar.create_empty () in
        let ry = Lwt_mvar.create_empty () in
        let wy = Lwt_mvar.create_empty () in
        let rz = Lwt_mvar.create_empty () in
        let wz = Lwt_mvar.create_empty () in
        S.set_exn t ~info:(infof "init") [ "a" ] "0" >>= fun () ->
        Lwt.join
          [
            update [ "a" ] ~retries:0 `Merge rx wx;
            update [ "a" ] ~retries:0 `Merge ry wy;
            update [ "a" ] ~retries:1 `Merge rz wz;
            ( Lwt_mvar.put rx "1" >>= fun () ->
              Lwt_mvar.take wx >|= check_ok >>= fun () ->
              let* a = S.get t [ "a" ] in
              Alcotest.(check string) "merge x" "1" a;
              Lwt_mvar.put ry "2" >>= fun () ->
              Lwt_mvar.take wy >|= check_conflict >>= fun () ->
              let* a = S.get t [ "a" ] in
              Alcotest.(check string) "merge y" a "1";
              Lwt_mvar.put rz "3" >>= fun () ->
              (* there's a conflict, the transaction is restarted so need to feed a
                 new value *)
              Lwt_mvar.put rz "4" >>= fun () ->
              Lwt_mvar.take wz >|= check_ok >>= fun () ->
              let+ a = S.get t [ "a" ] in
              Alcotest.(check string) "merge z" a "4" );
          ]
      in
      set () >>= test_and_set >>= merge >>= fun () -> P.Repo.close repo
    in
    run x test

  let test_concurrent_head_updates x () =
    let test repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.strf "X%d" i in
      let* t1 = S.master repo in
      let* t2 = S.master repo in
      let retry d fn =
        let rec aux i =
          fn () >>= function
          | true ->
              Log.debug (fun f -> f "%d: ok!" d);
              Lwt.return_unit
          | false ->
              Log.debug (fun f -> f "%d: conflict, retrying (%d)." d i);
              aux (i + 1)
        in
        aux 1
      in
      let write t n =
        write (fun i ->
            retry i (fun () ->
                let* test = S.Head.find t in
                let tag = Fmt.strf "tmp-%d-%d" n i in
                let* m = S.clone ~src:t ~dst:tag in
                S.set_exn m ~info:(infof "update") (k i) (v i) >>= fun () ->
                let* set = S.Head.find m in
                Lwt_unix.yield () >>= fun () -> S.Head.test_and_set t ~test ~set))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.strf "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0) >>= fun () ->
      perform (write t1 1 5 @ write t2 2 5) >>= fun () ->
      perform (read t1 5) >>= fun () -> P.Repo.close repo
    in
    run x test

  let test_shallow_objects x () =
    let test repo =
      let foo_k = S.Private.Contents.Key.hash "foo" in
      let bar_k = S.Private.Contents.Key.hash "bar" in
      let tree_1 = S.Tree.shallow repo (`Node foo_k) in
      let tree_2 = S.Tree.shallow repo (`Node bar_k) in
      let node_3 =
        S.Private.Node.Val.v
          [
            ("foo", `Contents (foo_k, S.Metadata.default)); ("bar", `Node bar_k);
          ]
      in
      let tree_3 = S.Tree.of_node (S.of_private_node repo node_3) in
      let info () = info "shallow" in
      let* t = S.master repo in
      S.set_tree_exn t [ "1" ] tree_1 ~info >>= fun () ->
      S.set_tree_exn t [ "2" ] tree_2 ~info >>= fun () ->
      let* h = S.Head.get t in
      let commit =
        S.of_private_commit repo
        @@ S.Private.Commit.Val.v ~info:(info ()) ~node:(S.Tree.hash tree_3)
             ~parents:[ S.Commit.hash h; foo_k ]
      in
      S.set_tree_exn t [ "3" ] ~parents:[ commit ] tree_3 ~info >>= fun () ->
      let* t1 = S.find_tree t [ "1" ] in
      Alcotest.(check (option tree_t)) "shallow tree" (Some tree_1) t1;
      P.Repo.close repo
    in
    run x test

  let test_clear x () =
    let test repo =
      let vt = v repo and b = b repo and ct = ct repo and n = n repo in
      let check_none msg =
        Alcotest.(check (testable Fmt.(option (const string "<value>")) ( = )))
          msg None
      in
      let check_val = check (T.option S.contents_t) in
      let check_commit = check (T.option @@ S.commit_t repo) in
      let* h2 = kv2 ~repo in
      P.Contents.clear vt >>= fun () ->
      let* () =
        P.Contents.find vt h2 >|= check_val "v2 after clear is not found" None
      in
      let* h2 = kv2 ~repo in
      let* () =
        P.Contents.find vt h2 >|= check_val "add v2 again after clear" (Some v2)
      in
      let* h1 = r1 ~repo in
      let* n1 = n1 ~repo in
      S.Branch.set repo b1 h1 >>= fun () ->
      let* () =
        S.Branch.find repo b1
        >|= check_commit "r1 before clear is found" (Some h1)
      in
      let* () =
        Lwt.join [ P.Branch.clear b; P.Commit.clear ct; P.Node.clear n ]
      in
      let* () =
        S.Branch.find repo b1
        >|= check_commit "r1 after clear is not found" None
      in
      let* () =
        P.Commit.find ct (S.Commit.hash h1)
        >|= check_none "after clear commit is not found"
      in
      let* () =
        P.Node.find n n1 >|= check_none "after clear node is not found"
      in
      let* h2 = kv2 ~repo in
      P.Contents.clear vt >>= fun () ->
      let* () =
        P.Contents.find vt h2 >|= check_none "v2 after clear is not found"
      in
      r1 ~repo >>= S.Branch.set repo b1 >>= fun () ->
      P.Branch.clear b >>= fun () ->
      let* () =
        S.Branch.find repo b1 >|= check_commit "cleared twice r1" None
      in
      P.Repo.close repo
    in
    run x test
end

let suite' l ?(prefix = "") (_, x) =
  let (module S) = x.store in
  let module T = Make (S) in
  (prefix ^ x.name, l)

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make (S) in
  let module T_graph = Store_graph.Make (S) in
  let module T_watch = Store_watch.Make (Log) (S) in
  suite'
    ([
       ("Basic operations on contents", speed, T.test_contents x);
       ("Basic operations on nodes", speed, T.test_nodes x);
       ("Basic operations on commits", speed, T.test_commits x);
       ("Basic operations on branches", speed, T.test_branches x);
       ("Hash operations on trees", speed, T.test_tree_hashes x);
       ("Basic merge operations", speed, T.test_simple_merges x);
       ("Basic operations on slices", speed, T.test_slice x);
       ("Test merges on tree updates", speed, T.test_merge_outdated_tree x);
       ("Tree caches and hashconsing", speed, T.test_tree_caches x);
       ("Complex histories", speed, T.test_history x);
       ("Empty stores", speed, T.test_empty x);
       ("Private node manipulation", speed, T.test_private_nodes x);
       ("High-level store operations", speed, T.test_stores x);
       ("High-level operations on trees", speed, T.test_trees x);
       ("High-level store synchronisation", speed, T.test_sync x);
       ("High-level store merges", speed, T.test_merge x);
       ("Unrelated merges", speed, T.test_merge_unrelated x);
       ("Low-level concurrency", speed, T.test_concurrent_low x);
       ("Concurrent updates", speed, T.test_concurrent_updates x);
       ("with_tree strategies", speed, T.test_with_tree x);
       ("Concurrent head updates", speed, T.test_concurrent_head_updates x);
       ("Concurrent merges", speed, T.test_concurrent_merges x);
       ("Shallow objects", speed, T.test_shallow_objects x);
       ("Closure with disconnected commits", speed, T.test_closure x);
       ("Clear", speed, T.test_clear x);
     ]
    @ List.map (fun (n, test) -> ("Graph." ^ n, speed, test x)) T_graph.tests
    @ List.map (fun (n, test) -> ("Watch." ^ n, speed, test x)) T_watch.tests)
    (speed, x)

let slow_suite (speed, x) =
  let (module S) = x.store in
  let module T = Make (S) in
  suite' ~prefix:"SLOW_"
    [
      ("Commit wide node", speed, T.test_commit_wide_node x);
      ("Wide nodes", `Slow, T.test_wide_nodes x);
    ]
    (speed, x)

let layered_suite (speed, x) =
  ( "LAYERED_" ^ x.name,
    match x.layered_store with
    | None -> []
    | Some layered_store ->
        let (module S) = layered_store in
        let module T = Make (S) in
        let module TL = Layered_store.Make_Layered (S) in
        let hook repo max = S.freeze repo ~max_lower:max in
        [
          ("Basic operations on branches", speed, T.test_branches ~hook x);
          ("Basic merge operations", speed, T.test_simple_merges ~hook x);
          ("Complex histories", speed, T.test_history ~hook x);
          ("Empty stores", speed, T.test_empty ~hook x);
          ("Basic operations on slices", speed, T.test_slice ~hook x);
          ("Private node manipulation", speed, T.test_private_nodes ~hook x);
          ("High-level store merges", speed, T.test_merge ~hook x);
          ("Unrelated merges", speed, T.test_merge_unrelated ~hook x);
          ("Test commits and graphs", speed, TL.test_graph_and_history x);
          ("Update branches after freeze", speed, TL.test_fail_branch x);
          ("Test operations on set", speed, TL.test_set x);
          ("Test operations on set tree", speed, TL.test_set_tree x);
          ("Gc and tree operations", speed, TL.test_gc x);
          ( "Merge into deleted branch",
            speed,
            TL.test_merge_into_deleted_branch x );
          ( "Merge with deleted branch",
            speed,
            TL.test_merge_with_deleted_branch x );
          ("Freeze with squash", speed, TL.test_squash x);
          ("Branches with squash", speed, TL.test_branch_squash x);
          ("Consecutive freezes", speed, TL.test_consecutive_freeze x);
          ("Test find tree after freeze", speed, TL.test_freeze_tree x);
          ("Keep max and copy from upper", speed, TL.test_copy_in_upper x);
          ("Keep max and heads after max", speed, TL.test_keep_heads x);
          ("Test find during freeze", speed, TL.test_find_during_freeze x);
          ("Test add during freeze", speed, TL.test_add_during_freeze x);
          ("Adds again objects deleted by freeze", speed, TL.test_add_again x);
        ] )

let run name ?(slow = false) ~misc tl =
  Printexc.record_backtrace true;
  let tl1 = List.map suite tl in
  let tl1 = if slow then tl1 @ List.map slow_suite tl else tl1 in
  let tl2 = List.map layered_suite tl in
  Alcotest.run name (tl1 @ tl2 @ misc)
