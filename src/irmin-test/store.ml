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

let src = Logs.Src.create "test" ~doc:"Irmin tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (S : Generic_key) = struct
  include Common.Make_helpers (S)
  module History = Irmin.Commit.History (B.Commit)

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

  let old k () = Ok (Some k)
  let may repo commits = function None -> () | Some f -> f repo commits

  let map_p xs =
    Eio.Switch.run @@ fun sw -> List.map (Eio.Fiber.fork_promise ~sw) xs

  let may_get_keys repo keys = function
    | None -> ()
    | Some f ->
        let commits =
          List.map
            (fun key () ->
              S.Commit.of_key repo key |> function
              | None -> Alcotest.fail "Cannot read commit hash"
              | Some c -> c)
            keys
          |> map_p
        in
        f repo commits

  let may_with_branch branches repo hook =
    let heads =
      List.map
        (fun branch () ->
          let h = S.Head.find branch in
          match h with
          | None -> Alcotest.fail "Cannot read head"
          | Some head -> head)
        branches
      |> map_p
    in
    may repo heads hook

  let contents c = S.Tree.v (`Contents (c, S.Metadata.default))

  let test_contents x () =
    let test repo =
      let t = B.Repo.contents_t repo in
      let check_key = check B.Contents.Key.t in
      let check_val = check (T.option S.contents_t) in
      let kv2 = kv2 ~repo in
      let k2' = with_contents repo (fun t -> B.Contents.add t v2) in
      check_key "kv2" kv2 k2';
      let v2' = B.Contents.find t k2' in
      check_val "v2" (Some v2) v2';
      let k2'' = with_contents repo (fun t -> B.Contents.add t v2) in
      check_key "kv2" kv2 k2'';
      let kv1 = kv1 ~repo in
      let k1' = with_contents repo (fun t -> B.Contents.add t v1) in
      check_key "kv1" kv1 k1';
      let k1'' = with_contents repo (fun t -> B.Contents.add t v1) in
      check_key "kv1" kv1 k1'';
      let v1' = B.Contents.find t kv1 in
      check_val "v1" (Some v1) v1';
      let v2' = B.Contents.find t kv2 in
      check_val "v2" (Some v2) v2';
      B.Repo.close repo;
      try
        let _ = with_contents repo (fun t -> B.Contents.add t v2) in
        Alcotest.fail "Add after close should not be allowed"
      with
      | Irmin.Closed -> ()
      | exn -> raise exn
    in
    run x test

  let get = function None -> Alcotest.fail "get" | Some v -> v

  let test_nodes x () =
    let test repo =
      let g = g repo and n = n repo in
      let k = with_contents repo (fun c -> B.Contents.add c "foo") |> normal in
      let check_hash = check B.Hash.t in
      let check_key = check B.Node.Key.t in
      let check_val = check [%typ: Graph.value option] in
      let check_list = checks [%typ: S.step * B.Node.Val.value] in
      let check_node msg v =
        let h' = B.Node.Hash.hash v in
        let key = with_node repo (fun n -> B.Node.add n v) in
        check_hash (msg ^ ": hash(v) = add(v)") (B.Node.Key.to_hash key) h'
      in
      let v = B.Node.Val.empty () in
      check_node "empty node" v;
      let v1 = B.Node.Val.add v "x" k in
      check_node "node: x" v1;
      let v2 = B.Node.Val.add v "x" k in
      check_node "node: x (bis)" v2;
      check B.Node.Val.t "add x" v1 v2;
      let v0 = B.Node.Val.remove v1 "x" in
      check B.Node.Val.t "remove x" v v0;
      let v3 = B.Node.Val.add v1 "x" k in
      Alcotest.(check bool) "same same" true (v1 == v3);
      let u = B.Node.Val.add v3 "y" k in
      check_node "node: x+y" v3;
      let u = B.Node.Val.add u "z" k in
      check_node "node: x+y+z" u;
      let check_values u =
        check_val "find x" (Some k) (B.Node.Val.find u "x");
        check_val "find y" (Some k) (B.Node.Val.find u "y");
        check_val "find z" (Some k) (B.Node.Val.find u "x");
        check_val "find xx" None (B.Node.Val.find u "xx")
      in
      check_values u;
      let () =
        let _w = B.Node.Val.of_list [ ("y", k); ("z", k); ("x", k) ] in
        (* XXX: this isn't a valid check. [u] is not concrete, and [w] is. *)
        (* check B.Node.Val.t "v" u w; *)
        ()
      in
      let all = B.Node.Val.list u in
      check_list "list all" [ ("x", k); ("y", k); ("z", k) ] all;
      let l = B.Node.Val.list ~length:1 u in
      check_list "list length=1" [ ("x", k) ] l;
      let l = B.Node.Val.list ~offset:1 u in
      check_list "list offset=1" [ ("y", k); ("z", k) ] l;
      let l = B.Node.Val.list ~offset:1 ~length:1 u in
      check_list "list offset=1 length=1" [ List.nth all 1 ] l;
      let u = B.Node.Val.add u "a" k in
      check_node "node: x+y+z+a" u;
      let u = B.Node.Val.add u "b" k in
      check_node "node: x+y+z+a+b" u;
      let h = B.Node.Hash.hash u in
      let k = with_node repo (fun n -> B.Node.add n u) in
      check_hash "hash(v) = add(v)" h (B.Node.Key.to_hash k);
      let w = B.Node.find n k in
      check_values (get w);
      let kv1 = kv1 ~repo in
      let k1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let k1' = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      check_key "k1.1" k1 k1';
      let t1 = B.Node.find n k1 in
      let k' = B.Node.Val.find (get t1) "x" in
      check
        (Irmin.Type.option B.Node.Val.value_t)
        "find x"
        (Some (normal kv1))
        k';
      let k1'' = with_node repo (fun n -> B.Node.add n (get t1)) in
      check_key "k1.2" k1 k1'';
      let k2 = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      let k2' = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      check_key "k2.1" k2 k2';
      let t2 = B.Node.find n k2 in
      let k2'' = with_node repo (fun n -> B.Node.add n (get t2)) in
      check_key "k2.2" k2 k2'';
      let k1''' = Graph.find g k2 [ "b" ] in
      check_val "k1.3" (Some (`Node k1)) k1''';
      let k3 = with_node repo (fun g -> Graph.v g [ ("a", `Node k2) ]) in
      let k3' = with_node repo (fun g -> Graph.v g [ ("a", `Node k2) ]) in
      check_key "k3.1" k3 k3';
      let t3 = B.Node.find n k3 in
      let k3'' = with_node repo (fun n -> B.Node.add n (get t3)) in
      check_key "k3.2" k3 k3'';
      let k2'' = Graph.find g k3 [ "a" ] in
      check_val "k2.3" (Some (`Node k2)) k2'';
      let k1'''' = Graph.find g k2' [ "b" ] in
      check_val "t1.2" (Some (`Node k1)) k1'''';
      let k1''''' = Graph.find g k3 [ "a"; "b" ] in
      check_val "t1.3" (Some (`Node k1)) k1''''';
      let kv11 = Graph.find g k1 [ "x" ] in
      check_val "v1.1" (Some (normal kv1)) kv11;
      let kv12 = Graph.find g k2 [ "b"; "x" ] in
      check_val "v1.2" (Some (normal kv1)) kv12;
      let kv13 = Graph.find g k3 [ "a"; "b"; "x" ] in
      check_val "v1" (Some (normal kv1)) kv13;
      let kv2 = kv2 ~repo in
      let k4 = with_node repo (fun g -> Graph.v g [ ("x", normal kv2) ]) in
      let k5 =
        with_node repo (fun g -> Graph.v g [ ("b", `Node k1); ("c", `Node k4) ])
      in
      let k6 = with_node repo (fun g -> Graph.v g [ ("a", `Node k5) ]) in
      let k6' =
        with_node repo (fun g -> Graph.add g k3 [ "a"; "c"; "x" ] (normal kv2))
      in
      check_key "node k6" k6 k6';
      let n6' = B.Node.find n k6' in
      let n6 = B.Node.find n k6 in
      check T.(option B.Node.Val.t) "node n6" n6 n6';
      let assert_no_duplicates n node =
        let names = ref [] in
        let all = Graph.list g node in
        List.iter
          (fun (s, _) ->
            if List.mem ~equal:String.equal s !names then
              Alcotest.failf "%s: duplicate!" n
            else names := s :: !names)
          all
      in
      let n0 = with_node repo (fun g -> Graph.v g []) in
      let n1 = with_node repo (fun g -> Graph.add g n0 [ "b" ] (`Node n0)) in
      let n2 = with_node repo (fun g -> Graph.add g n1 [ "a" ] (`Node n0)) in
      let n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (`Node n0)) in
      assert_no_duplicates "1" n3;
      let n1 = with_node repo (fun g -> Graph.add g n0 [ "a" ] (`Node n0)) in
      let n2 = with_node repo (fun g -> Graph.add g n1 [ "b" ] (`Node n0)) in
      let n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (`Node n0)) in
      assert_no_duplicates "2" n3;
      let n1 = with_node repo (fun g -> Graph.add g n0 [ "b" ] (normal kv1)) in
      let n2 = with_node repo (fun g -> Graph.add g n1 [ "a" ] (normal kv1)) in
      let n3 = with_node repo (fun g -> Graph.add g n2 [ "a" ] (normal kv1)) in
      assert_no_duplicates "3" n3;
      let n1 = with_node repo (fun g -> Graph.add g n0 [ "a" ] (normal kv1)) in
      let n2 = with_node repo (fun g -> Graph.add g n1 [ "b" ] (normal kv1)) in
      let n3 = with_node repo (fun g -> Graph.add g n2 [ "b" ] (normal kv1)) in
      assert_no_duplicates "4" n3;
      S.Repo.close repo;
      try
        let n0 = with_node repo (fun g -> Graph.v g []) in
        let _ = with_node repo (fun g -> Graph.add g n0 [ "b" ] (`Node n0)) in
        Alcotest.fail "Add after close should not be allowed"
      with
      | Irmin.Closed -> ()
      | exn -> raise exn
    in
    run x test

  let test_commits x () =
    let test repo =
      let info date =
        let message = Fmt.str "Test commit: %d" date in
        S.Info.v ~author:"test" ~message (Int64.of_int date)
      in
      let kv1 = kv1 ~repo in
      let h = h repo and c = B.Repo.commit_t repo in
      let check_val = check (T.option B.Commit.Val.t) in
      let check_key = check B.Commit.Key.t in
      let check_keys = checks B.Commit.Key.t in
      (* t3 -a-> t2 -b-> t1 -x-> (v1) *)
      let kt1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      let kt2 = with_node repo (fun g -> Graph.v g [ ("a", `Node kt1) ]) in
      let kt3 = with_node repo (fun g -> Graph.v g [ ("b", `Node kt2) ]) in
      (* r1 : t2 *)
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let kr1, _ = with_info 3 (History.v ~node:kt2 ~parents:[]) in
      let kr1', _ = with_info 3 (History.v ~node:kt2 ~parents:[]) in
      let t1 = B.Commit.find c kr1 in
      let t1' = B.Commit.find c kr1' in
      check_val "t1" t1 t1';
      check_key "kr1" kr1 kr1';

      (* r1 -> r2 : t3 *)
      let kr2, _ = with_info 4 (History.v ~node:kt3 ~parents:[ kr1 ]) in
      let kr2', _ = with_info 4 (History.v ~node:kt3 ~parents:[ kr1 ]) in
      check_key "kr2" kr2 kr2';
      let kr1s = History.closure h ~min:[] ~max:[ kr1 ] in
      check_keys "g1" [ kr1 ] kr1s;
      let kr2s = History.closure h ~min:[] ~max:[ kr2 ] in
      check_keys "g2" [ kr1; kr2 ] kr2s;
      let () =
        S.Commit.of_key repo kr1 |> function
        | None -> Alcotest.fail "Cannot read commit hash"
        | Some c ->
            Alcotest.(check string)
              "author" "test"
              (S.Info.author (S.Commit.info c))
      in
      S.Repo.close repo;
      try
        let _ = with_info 3 (History.v ~node:kt1 ~parents:[]) in
        Alcotest.fail "Add after close should not be allowed"
      with
      | Irmin.Closed -> ()
      | exn -> raise exn
    in
    run x test

  let test_closure x () =
    let test repo =
      let info date =
        let message = Fmt.str "Test commit: %d" date in
        S.Info.v ~author:"test" ~message (Int64.of_int date)
      in
      let check_keys = checks B.Commit.Key.t in
      let equal_key = Irmin.Type.(unstage (equal B.Commit.Key.t)) in
      let h = h repo in
      let initialise_nodes =
        List.map
          (fun i ->
            let kv =
              with_contents repo (fun t -> B.Contents.add t (string_of_int i))
            in
            with_node repo (fun g -> Graph.v g [ (string_of_int i, normal kv) ]))
          [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
      in
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let initialise_graph nodes =
        match nodes with
        | [] -> assert false
        | node :: rest ->
            let kr0, _ = with_info 0 (History.v ~node ~parents:[]) in
            let commits = Array.make 9 kr0 in
            let commit ~node ~parents i =
              let kr1, _ = with_info i (History.v ~node ~parents) in
              commits.(i) <- kr1;
              i + 1
            in
            let _ =
              List.fold_left
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
      let commits = initialise_nodes |> initialise_graph in
      let krs = History.closure h ~min:[ commits.(1) ] ~max:[ commits.(5) ] in
      check_keys "commits between 1 and 5"
        [ commits.(1); commits.(2); commits.(3); commits.(4); commits.(5) ]
        krs;
      let krs = History.closure h ~min:[] ~max:[ commits.(5) ] in
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
      let krs =
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
      let krs =
        History.closure h
          ~min:[ commits.(1); commits.(7) ]
          ~max:[ commits.(4); commits.(8) ]
      in
      check_keys "disconnected min and max returns a disconnected graph"
        [ commits.(1); commits.(2); commits.(7); commits.(4); commits.(8) ]
        krs;
      let () =
        History.closure h ~min:[ commits.(7) ] ~max:[] |> function
        | [] -> ()
        | _ -> Alcotest.fail "expected empty list"
      in
      let () =
        let ls = History.closure h ~min:[ commits.(7) ] ~max:[ commits.(6) ] in
        if List.mem ~equal:equal_key commits.(7) ls then
          Alcotest.fail "disconnected node should not be in closure"
      in
      let krs =
        History.closure h ~min:[ commits.(4) ] ~max:[ commits.(4); commits.(6) ]
      in
      check_keys "min and max have the same commit"
        [ commits.(6); commits.(4) ]
        krs;
      let () =
        let ls =
          History.closure h
            ~min:[ commits.(4); commits.(0) ]
            ~max:[ commits.(4); commits.(6) ]
        in
        if List.mem ~equal:equal_key commits.(0) ls then
          Alcotest.fail "disconnected node should not be in closure"
      in
      S.Repo.close repo
    in
    run x test

  let test_branches ?hook x () =
    let test repo =
      let check_keys = checks S.Branch.t in
      let check_val = check (T.option @@ S.commit_t repo) in
      let kv1 = r1 ~repo in
      let kv2 = r2 ~repo in
      line "pre-update";
      S.Branch.set repo b1 kv1;
      may repo [ kv2 ] hook;
      line "post-update";
      let k1' = S.Branch.find repo b1 in
      check_val "r1" (Some kv1) k1';
      S.Branch.set repo b2 kv2;
      let k2' = S.Branch.find repo b2 in
      check_val "r2" (Some kv2) k2';
      S.Branch.set repo b1 kv2;
      let k2'' = S.Branch.find repo b1 in
      check_val "r1-after-update" (Some kv2) k2'';
      let bs = S.Branch.list repo in
      check_keys "list" [ b1; b2 ] bs;
      S.Branch.remove repo b1;
      let empty = S.Branch.find repo b1 in
      check_val "empty" None empty;
      let b2' = S.Branch.list repo in
      check_keys "all-after-remove" [ b2 ] b2';
      S.Repo.close repo;
      try
        let _ = S.Branch.set repo b1 kv1 in
        Alcotest.fail "Add after close should not be allowed"
      with
      | Irmin.Closed -> ()
      | exn -> raise exn
    in
    run x test

  let test_tree_hashes x () =
    let test repo =
      let node bindings =
        with_node repo (fun g ->
            let empty = Graph.empty g in
            List.fold_left
              (fun t (k, v) ->
                let v = with_contents repo (fun t -> B.Contents.add t v) in
                Graph.add g t k (`Contents (v, S.Metadata.default)))
              empty bindings)
      in
      let tree bindings =
        List.fold_left
          (fun t (k, v) -> S.Tree.add t k v)
          (S.Tree.empty ()) bindings
      in
      let check_hash msg bindings =
        let node = node bindings in
        let tree = tree bindings in
        check B.Hash.t msg (B.Node.Key.to_hash node) (S.Tree.hash tree)
      in
      check_hash "empty" [];
      let bindings1 = [ ([ "a" ], "x"); ([ "b" ], "y") ] in
      check_hash "1 level" bindings1;
      let bindings2 = [ ([ "a"; "b" ], "x"); ([ "a"; "c" ], "y") ] in
      check_hash "2 levels" bindings2;
      S.Repo.close repo
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
      Irmin.Merge.(f merge_x) ~old x y |> function
      | Error (`Conflict c) -> Alcotest.failf "conflict %s" c
      | Ok m' ->
          check dx "compound merge" m m';
          ()
    in
    let test repo =
      check_merge ();
      let kv1 = kv1 ~repo in
      let kv2 = kv2 ~repo in
      let result =
        T.(result (option B.Contents.Key.t) Irmin.Merge.conflict_t)
      in
      (* merge contents *)
      let kv1' =
        with_contents repo (fun v ->
            Irmin.Merge.f (B.Contents.merge v) ~old:(old (Some kv1)) (Some kv1)
              (Some kv1))
      in
      check result "merge kv1" (Ok (Some kv1)) kv1';
      let kv2' =
        with_contents repo (fun v ->
            Irmin.Merge.f (B.Contents.merge v) ~old:(old (Some kv1)) (Some kv1)
              (Some kv2))
      in
      check result "merge kv2" (Ok (Some kv2)) kv2';

      (* merge nodes *)
      let g = g repo in
      (* The empty node *)
      let k0 = with_node repo (fun g -> Graph.v g []) in
      (* Create the node t1 -x-> (v1) *)
      let k1 = with_node repo (fun g -> Graph.v g [ ("x", normal kv1) ]) in
      (* Create the node t2 -b-> t1 -x-> (v1) *)
      let k2 = with_node repo (fun g -> Graph.v g [ ("b", `Node k1) ]) in
      (* Create the node t3 -c-> t1 -x-> (v1) *)
      let k3 = with_node repo (fun g -> Graph.v g [ ("c", `Node k1) ]) in
      (* Should create the node:
                          t4 -b-> t1 -x-> (v1)
                             \c/ *)
      let k4 =
        with_node repo (fun g ->
            Irmin.Merge.(f @@ B.Node.merge g)
              ~old:(old (Some k0)) (Some k2) (Some k3))
      in
      let k4 = merge_exn "k4" k4 in
      let k4 = match k4 with Some k -> k | None -> failwith "k4" in
      let _ = k4 in
      let succ_t = [%typ: string * Graph.value] in
      let succ = Graph.list g k4 in
      checks succ_t "k4" [ ("b", `Node k1); ("c", `Node k1) ] succ;
      let info date =
        let i = Int64.of_int date in
        S.Info.v ~author:"test" ~message:"Test commit" i
      in
      let c = B.Repo.commit_t repo in
      let with_info n fn = with_commit repo (fun h -> fn h ~info:(info n)) in
      let kr0, _ = with_info 0 (History.v ~node:k0 ~parents:[]) in
      let kr1, _ = with_info 1 (History.v ~node:k2 ~parents:[ kr0 ]) in
      let kr2, _ = with_info 2 (History.v ~node:k3 ~parents:[ kr0 ]) in
      may_get_keys repo [ kr1; kr2 ] hook;
      let kr3 =
        with_info 3 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr0) kr1 kr2)
      in
      let kr3 = merge_exn "kr3" kr3 in
      may_get_keys repo [ kr3 ] hook;
      let kr3_key' =
        with_info 4 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr2) kr2 kr3)
      in
      let kr3_key' = merge_exn "kr3_key'" kr3_key' in
      let check_key = check B.Commit.Key.t in
      check_key "kr3 id with immediate parent'" kr3 kr3_key';
      let kr3_key =
        with_info 5 (fun h ~info ->
            Irmin.Merge.f
              (History.merge h ~info:(fun () -> info))
              ~old:(old kr0) kr0 kr3)
      in
      let kr3_key = merge_exn "kr3_key" kr3_key in
      check_key "kr3 key with old parent" kr3 kr3_key;
      let kr3', _ = with_info 3 @@ History.v ~node:k4 ~parents:[ kr1; kr2 ] in
      let r3 = B.Commit.find c kr3 in
      let r3' = B.Commit.find c kr3' in
      check T.(option B.Commit.Val.t) "r3" r3 r3';
      check_key "kr3" kr3 kr3';
      B.Repo.close repo
    in
    run x test

  let test_history ?hook x () =
    let test repo =
      let info date =
        let i = Int64.of_int date in
        S.Info.v ~author:"test" ~message:"Test commit" i
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
        let a = S.of_commit a in
        let b = S.of_commit b in
        let lcas = S.lcas ~max_depth ~n a b in
        assert_lcas msg expected lcas;
        let lcas = S.lcas ~max_depth:(max_depth - 1) ~n a b in
        let msg = Printf.sprintf "%s [max-depth=%d]" msg (max_depth - 1) in
        assert_lcas_err msg `Max_depth_reached lcas
      in
      let assert_last_modified msg ?depth ~n t key expected =
        let last = S.last_modified ?depth ~n t key in
        S.repo t |> fun repo ->
        let msg = Printf.sprintf "%s [n=%d]" msg n in
        checks (S.commit_t repo) msg expected last
      in
      let assert_history_empty msg c expected =
        let t = S.of_commit c in
        S.history t |> S.History.is_empty |> Alcotest.(check bool) msg expected
      in
      let tree = S.Tree.empty () in
      let k0 = random_path ~label:8 ~path:5 in
      let k1 = random_path ~label:8 ~path:4 in
      let k2 = random_path ~label:8 ~path:6 in

      (* test that we don't compute too many lcas

         0(k0, k1) -> 1(k1) -> 2(k0) -> 3(k1, k0) -> 4(k1)
      *)
      let tree = S.Tree.add tree k0 (random_value 1024) in
      let tree = S.Tree.add tree k1 (random_value 1024) in
      let c0 = S.Commit.v repo ~info:(info 0) ~parents:[] tree in
      may repo [ c0 ] hook;
      assert_history_empty "nonempty 1 commit" c0 false;
      let tree = S.Tree.add tree k1 (random_value 1024) in
      let c1 =
        S.Commit.v repo ~info:(info 1) ~parents:[ S.Commit.key c0 ] tree
      in
      assert_history_empty "nonempty 2 commits" c0 false;
      let tree = S.Tree.add tree k0 (random_value 1024) in
      let c2 =
        S.Commit.v repo ~info:(info 2) ~parents:[ S.Commit.key c1 ] tree
      in
      let tree = S.Tree.add tree k0 (random_value 1024) in
      let tree = S.Tree.add tree k1 (random_value 1024) in
      let c3 =
        S.Commit.v repo ~info:(info 3) ~parents:[ S.Commit.key c2 ] tree
      in
      may repo [ c3 ] hook;
      let tree = S.Tree.add tree k1 (random_value 1024) in
      let c4 =
        S.Commit.v repo ~info:(info 4) ~parents:[ S.Commit.key c3 ] tree
      in
      assert_lcas "line lcas 1" ~max_depth:0 3 c3 c4 [ c3 ];
      assert_lcas "line lcas 2" ~max_depth:1 3 c2 c4 [ c2 ];
      assert_lcas "line lcas 3" ~max_depth:2 3 c1 c4 [ c1 ];
      let store = S.of_commit c4 in
      assert_last_modified "line last_modified 1" ~n:1 store k0 [ c3 ];
      assert_last_modified "line last_modified 2" ~n:2 store k0 [ c2; c3 ];
      assert_last_modified "line last_modified 3" ~n:3 store k0 [ c0; c2; c3 ];
      assert_last_modified "line last_modified 4" ~depth:1 ~n:3 store k0 [ c3 ];
      assert_last_modified "line last_modified 5" ~n:1 store k2 [];
      assert_last_modified "line last_modified 5" ~depth:0 ~n:2 store k0 [];
      (* test for multiple lca

         4(k1) -> 10 (k2) ---> 11(k0, k2) --> 13(k1) --> 15(k1, k2)
                  |                 \_______________________/____
                  |           _____________________/             \
                  |          /                                   \
                  \---> 12 (k0, k1) --> 14 (k2) --> 16 (k2) --> 17 (k0)
      *)
      let tree = S.Tree.add tree k2 (random_value 1024) in
      let c10 =
        S.Commit.v repo ~info:(info 10) ~parents:[ S.Commit.key c4 ] tree
      in
      let tree_up = S.Tree.add tree k0 (random_value 1024) in
      let tree_up = S.Tree.add tree_up k2 (random_value 1024) in
      let c11 =
        S.Commit.v repo ~info:(info 11) ~parents:[ S.Commit.key c10 ] tree_up
      in
      let tree_down = S.Tree.add tree k0 (random_value 1024) in
      let tree_12 = S.Tree.add tree_down k1 (random_value 1024) in
      let c12 =
        S.Commit.v repo ~info:(info 12) ~parents:[ S.Commit.key c10 ] tree_12
      in
      let tree_up = S.Tree.add tree_up k1 (random_value 1024) in
      let c13 =
        S.Commit.v repo ~info:(info 13) ~parents:[ S.Commit.key c11 ] tree_up
      in
      let tree_down = S.Tree.add tree_12 k2 (random_value 1024) in
      let c14 =
        S.Commit.v repo ~info:(info 14) ~parents:[ S.Commit.key c12 ] tree_down
      in
      let tree_up = S.Tree.add tree_12 k1 (random_value 1024) in
      let tree_up = S.Tree.add tree_up k2 (random_value 1024) in
      let c15 =
        S.Commit.v repo ~info:(info 15)
          ~parents:[ S.Commit.key c12; S.Commit.key c13 ]
          tree_up
      in
      let tree_down = S.Tree.add tree_down k2 (random_value 1024) in
      let c16 =
        S.Commit.v repo ~info:(info 16) ~parents:[ S.Commit.key c14 ] tree_down
      in
      let tree_down = S.Tree.add tree_down k0 (random_value 1024) in
      let c17 =
        S.Commit.v repo ~info:(info 17)
          ~parents:[ S.Commit.key c11; S.Commit.key c16 ]
          tree_down
      in
      assert_lcas "x lcas 0" ~max_depth:0 5 c10 c10 [ c10 ];
      assert_lcas "x lcas 1" ~max_depth:0 5 c14 c14 [ c14 ];
      assert_lcas "x lcas 2" ~max_depth:0 5 c10 c11 [ c10 ];
      assert_lcas "x lcas 3" ~max_depth:1 5 c12 c16 [ c12 ];
      assert_lcas "x lcas 4" ~max_depth:1 5 c10 c13 [ c10 ];
      assert_lcas "x lcas 5" ~max_depth:2 5 c13 c14 [ c10 ];
      assert_lcas "x lcas 6" ~max_depth:3 5 c15 c16 [ c12 ];
      assert_lcas "x lcas 7" ~max_depth:3 5 c15 c17 [ c11; c12 ];
      let store = S.of_commit c17 in
      let () =
        assert_last_modified "x last_modified 1" ~n:3 store k0 [ c11; c12; c17 ]
      in
      let () = assert_last_modified "x last_modified 2" ~n:1 store k2 [ c16 ] in
      let () =
        assert_last_modified "x last_modified 3" ~n:2 store k1 [ c4; c12 ]
      in
      let () =
        assert_last_modified "x last_modified 4" ~depth:3 ~n:5 store k1
          [ c4; c12 ]
      in
      let () =
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
      let c10 =
        S.Commit.v repo ~info:(info 10) ~parents:[ S.Commit.key c4 ] tree
      in
      let c11 =
        S.Commit.v repo ~info:(info 11) ~parents:[ S.Commit.key c10 ] tree
      in
      let c12 =
        S.Commit.v repo ~info:(info 12) ~parents:[ S.Commit.key c11 ] tree
      in
      let c13 =
        S.Commit.v repo ~info:(info 13) ~parents:[ S.Commit.key c12 ] tree
      in
      let c14 =
        S.Commit.v repo ~info:(info 14)
          ~parents:[ S.Commit.key c11; S.Commit.key c13 ]
          tree
      in
      let c15 =
        S.Commit.v repo ~info:(info 15)
          ~parents:[ S.Commit.key c13; S.Commit.key c14 ]
          tree
      in
      let c16 =
        S.Commit.v repo ~info:(info 16) ~parents:[ S.Commit.key c11 ] tree
      in
      assert_lcas "weird lcas 1" ~max_depth:0 3 c14 c15 [ c14 ];
      assert_lcas "weird lcas 2" ~max_depth:0 3 c13 c15 [ c13 ];
      assert_lcas "weird lcas 3" ~max_depth:1 3 c12 c15 [ c12 ];
      assert_lcas "weird lcas 4" ~max_depth:1 3 c11 c15 [ c11 ];
      assert_lcas "weird lcas 4" ~max_depth:3 3 c15 c16 [ c11 ];
      (* fast-forward *)
      let ff = testable Irmin.Type.(result unit S.ff_error_t) in
      let t12 = S.of_commit c12 in
      let b1 = S.Head.fast_forward t12 c16 in
      Alcotest.(check ff) "ff 1.1" (Error `Rejected) b1;
      let k12' = S.Head.get t12 in
      check (S.commit_t repo) "ff 1.2" c12 k12';
      let b2 = S.Head.fast_forward t12 ~n:1 c14 in
      Alcotest.(check ff) "ff 2.1" (Error `Rejected) b2;
      let k12'' = S.Head.get t12 in
      check (S.commit_t repo) "ff 2.2" c12 k12'';
      let b3 = S.Head.fast_forward t12 c14 in
      Alcotest.(check ff) "ff 2.2" (Ok ()) b3;
      let c14' = S.Head.get t12 in
      check (S.commit_t repo) "ff 2.3" c14 c14';
      B.Repo.close repo
    in
    run x test

  let test_empty ?hook x () =
    let test repo =
      let t = S.empty repo in
      let h = S.Head.find t in
      check T.(option @@ S.commit_t repo) "empty" None h;
      let r1 = r1 ~repo in
      may repo [ r1 ] hook;
      S.set_exn t ~info:S.Info.none [ "b"; "x" ] v1;
      let h = S.Head.find t in
      check T.(option @@ S.commit_t repo) "not empty" (Some r1) h;
      B.Repo.close repo
    in
    run x test

  let test_slice ?hook x () =
    let test repo =
      let t = S.main repo in
      let a = "" in
      let b = "haha" in
      S.set_exn t ~info:(infof "slice") [ "x"; "a" ] a;
      S.set_exn t ~info:(infof "slice") [ "x"; "b" ] b;
      may_with_branch [ t ] repo hook;
      let slice = S.Repo.export repo in
      let str = T.to_json_string B.Slice.t slice in
      let slice' =
        match T.decode_json B.Slice.t (Jsonm.decoder (`String str)) with
        | Ok t -> t
        | Error (`Msg e) -> Alcotest.failf "decoding error: %s" e
      in
      check B.Slice.t "slices" slice slice';
      B.Repo.close repo
    in
    run x test

  let test_backend_nodes ?hook x () =
    let test repo =
      let check_val = check [%typ: S.contents option] in
      let vx = "VX" in
      let vy = "VY" in
      let t = S.main repo in
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] vx;
      let tree = S.get_tree t [ "x" ] in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree;
      let vx' = S.find t [ "u"; "y"; "z" ] in
      check_val "vx" (Some vx) vx';
      let tree1 = S.get_tree t [ "u" ] in
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] vy;
      may_with_branch [ t ] repo hook;
      let tree2 = S.get_tree t [ "u" ] in
      let tree3 = S.Tree.add tree [ "x"; "z" ] vx in
      let v' =
        Irmin.Merge.f S.Tree.merge ~old:(Irmin.Merge.promise tree1) tree2 tree3
        |> merge_exn "tree"
      in
      S.set_tree_exn t ~info:(infof "merge") [ "u" ] v';
      let vy' = S.find t [ "u"; "x"; "y" ] in
      check_val "vy after merge" (Some vy) vy';
      let vx' = S.find t [ "u"; "x"; "z" ] in
      check_val "vx after merge" (Some vx) vx';
      B.Repo.close repo
    in
    run x test

  let test_stores x () =
    let test repo =
      let check_val = check [%typ: S.contents option] in
      let check_list = checks [%typ: S.Path.step * S.tree] in
      let t = S.main repo in
      S.set_exn t ~info:(infof "init") [ "a"; "b" ] v1;
      let b0 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem0" true b0;
      let t = S.clone ~src:t ~dst:"test" in
      let b1 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem1" true b1;
      let b2 = S.mem t [ "a" ] in
      Alcotest.(check bool) "mem2" false b2;
      let v1' = S.find t [ "a"; "b" ] in
      check_val "v1.1" (Some v1) v1';
      let r1 = S.Head.get t in
      let t = S.clone ~src:t ~dst:"test" in
      S.set_exn t ~info:(infof "update") [ "a"; "c" ] v2;
      let b1 = S.mem t [ "a"; "b" ] in
      Alcotest.(check bool) "mem3" true b1;
      let b2 = S.mem t [ "a" ] in
      Alcotest.(check bool) "mem4" false b2;
      let v1' = S.find t [ "a"; "b" ] in
      check_val "v1.1" (Some v1) v1';
      let b1 = S.mem t [ "a"; "c" ] in
      Alcotest.(check bool) "mem5" true b1;
      let v2' = S.find t [ "a"; "c" ] in
      check_val "v1.1" (Some v2) v2';
      S.remove_exn t ~info:(infof "remove") [ "a"; "b" ];
      let v1'' = S.find t [ "a"; "b" ] in
      check_val "v1.2" None v1'';
      S.Head.set t r1;
      let v1'' = S.find t [ "a"; "b" ] in
      check_val "v1.3" (Some v1) v1'';
      let ks = S.list t [ "a" ] in
      check_list "path" [ ("b", contents v1) ] ks;
      let () =
        S.set_exn t ~info:(infof "update2") [ "a"; long_random_ascii_string ] v1
      in
      S.remove_exn t ~info:(infof "remove rec") [ "a" ];
      let dirs = S.list t [] in
      check_list "remove rec" [] dirs;
      let () =
        try
          S.set_exn t ~info:(infof "update root") [] v1;
          Alcotest.fail "update root"
        with
        | Invalid_argument _ -> ()
        | e -> Alcotest.fail ("update root: " ^ Printexc.to_string e)
      in
      let none = S.find t [] in
      check_val "read root" none None;
      S.set_exn t ~info:(infof "update") [ "a" ] v1;
      S.remove_exn t ~info:(infof "remove rec --all") [];
      let dirs = S.list t [] in
      check_list "remove rec root" [] dirs;
      let a = "ok" in
      let b = "maybe?" in
      S.set_exn t ~info:(infof "fst one") [ "fst" ] a;
      S.set_exn t ~info:(infof "snd one") [ "fst"; "snd" ] b;
      let fst = S.find t [ "fst" ] in
      check_val "data model 1" None fst;
      let snd = S.find t [ "fst"; "snd" ] in
      check_val "data model 2" (Some b) snd;
      S.set_exn t ~info:(infof "fst one") [ "fst" ] a;
      let fst = S.find t [ "fst" ] in
      check_val "data model 3" (Some a) fst;
      let snd = S.find t [ "fst"; "snd" ] in
      check_val "data model 4" None snd;
      let tagx = "x" in
      let tagy = "y" in
      let xy = [ "x"; "y" ] in
      let vx = "VX" in
      let tx = S.of_branch repo tagx in
      S.Branch.remove repo tagx;
      S.Branch.remove repo tagy;
      S.set_exn tx ~info:(infof "update") xy vx;
      let ty = S.clone ~src:tx ~dst:tagy in
      let vx' = S.find ty xy in
      check_val "update tag" (Some vx) vx';
      S.status tx |> fun tagx' ->
      S.status ty |> fun tagy' ->
      check (S.Status.t repo) "tagx" (`Branch tagx) tagx';
      check (S.Status.t repo) "tagy" (`Branch tagy) tagy';
      let t = S.main repo in
      S.Repo.close repo;
      try
        let _ = S.set_exn t ~info:(infof "add after close") [ "a" ] "bar" in
        Alcotest.fail "Add after close should not be allowed"
      with
      | Irmin.Closed -> ()
      | exn -> raise exn
    in
    run x test

  let test_atomic x () =
    let test repo =
      let check_commit = check T.(option (S.commit_t repo)) in
      let t = S.main repo in
      let _c_empty =
        S.test_set_and_get_exn t ~info:(infof "init") [ "a"; "b" ] ~test:None
          ~set:(Some v1)
      in
      let c_none =
        S.test_set_and_get_exn t ~info:(infof "init") [ "a"; "b" ]
          ~test:(Some v1) ~set:(Some v1)
      in
      check_commit "No commit" None c_none;
      let message0 = "first" in
      let message1 = "second" in
      let v3 = "v3" in
      let c0 =
        S.test_set_and_get_exn t ~info:(infof "%s" message0) [ "a"; "b" ]
          ~test:(Some v1) ~set:(Some v2)
      in
      let c0 = Option.get c0 in
      let c0_message = S.Commit.info c0 |> S.Info.message in
      Alcotest.(check string) "commit0" message0 c0_message;
      let c1 =
        S.test_set_and_get_exn t ~info:(infof "%s" message1) [ "a"; "b" ]
          ~test:(Some v2) ~set:(Some v3)
      in
      let c0_store = S.of_commit c0 in
      let v2' = S.get c0_store [ "a"; "b" ] in
      Alcotest.(check string) "commit0 value" v2 v2';
      let c1_store = S.of_commit (Option.get c1) in
      let v3' = S.get c1_store [ "a"; "b" ] in
      Alcotest.(check string) "commit1 value" v3 v3';
      S.Repo.close repo
    in
    run x test

  let stats_t = Alcotest.testable (Irmin.Type.pp_dump S.Tree.stats_t) ( = )

  let empty_stats =
    { S.Tree.nodes = 0; leafs = 0; skips = 0; depth = 0; width = 0 }

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

  let test_tree_caches x () =
    let test repo =
      let info = S.Info.none in
      let t1 = S.main repo in
      S.set_exn t1 ~info [ "a"; "b" ] "foo";
      (* Testing cache *)
      S.Tree.reset_counters ();
      let v = S.get_tree t1 [] in
      Alcotest.(check inspect) "inspect" (`Node `Key) (S.Tree.inspect v);
      let v = S.Tree.add v [ "foo" ] "foo" in
      Alcotest.(check inspect) "inspect:0" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:0" 0 (S.Tree.counters ()).node_val_v;
      let v = S.Tree.add v [ "bar"; "foo" ] "bar" in
      Alcotest.(check inspect) "inspect:1" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:1" 0 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:1" 0 (S.Tree.counters ()).node_val_list;
      let _ = S.Tree.hash v in
      Alcotest.(check inspect) "inspect:2" (`Node `Value) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:2" 1 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:2" 0 (S.Tree.counters ()).node_val_list;
      S.set_tree_exn t1 ~info [] v;
      Alcotest.(check inspect) "inspect:3" (`Node `Key) (S.Tree.inspect v);
      Alcotest.(check int) "val-v:3" 2 (S.Tree.counters ()).node_val_v;
      Alcotest.(check int) "val-list:3" 0 (S.Tree.counters ()).node_val_list;
      B.Repo.close repo
    in
    run x test

  let pp_depth = Irmin.Type.pp S.Tree.depth_t
  let pp_key = Irmin.Type.pp S.Path.t
  let contents_t = T.pair S.contents_t S.metadata_t
  let diff_t = T.(pair S.path_t (Irmin.Diff.t contents_t))
  let check_diffs = checks diff_t
  let check_ls = checks T.(pair S.step_t S.tree_t)

  let test_trees x () =
    let test repo =
      let t = S.main repo in
      let nodes = random_nodes 100 in
      let foo1 = random_value 10 in
      let foo2 = random_value 10 in
      let v1 =
        S.Tree.singleton [ "foo"; "bar"; "toto" ] foo2
        |> with_binding [ "foo"; "toto" ] foo1
      in
      S.Tree.clear v1;
      let () =
        let dont_skip k =
          Alcotest.failf "should not have skipped: '%a'" pp_key k
        in
        S.Tree.fold ~depth:(`Eq 1) ~force:(`False dont_skip) v1 ()
      in
      let () =
        S.Tree.fold ~depth:(`Eq 1) ~force:`True (S.Tree.empty ()) ()
          ~contents:(fun k _ ->
            assert (List.length k = 1);
            Alcotest.fail "contents")
          ~node:(fun k _ ->
            assert (List.length k = 1);
            Alcotest.fail "node")
      in
      let fold depth ecs ens =
        let cs, ns =
          S.Tree.fold v1 ?depth ~force:`True ~cache:false
            ~contents:(fun path _ (cs, ns) -> (path :: cs, ns))
            ~node:(fun path _ (cs, ns) -> (cs, path :: ns))
            ([], [])
        in
        let paths = Alcotest.slist (testable S.Path.t) compare in
        Alcotest.(check paths)
          (Fmt.str "contents depth=%a" Fmt.(Dump.option pp_depth) depth)
          ecs cs;
        Alcotest.(check paths)
          (Fmt.str "nodes depth=%a" Fmt.(Dump.option pp_depth) depth)
          ens ns
      in
      let () =
        fold None
          [ [ "foo"; "bar"; "toto" ]; [ "foo"; "toto" ] ]
          [ []; [ "foo" ]; [ "foo"; "bar" ] ]
      in
      fold (Some (`Eq 0)) [] [ [] ];
      fold (Some (`Eq 1)) [] [ [ "foo" ] ];
      let () = fold (Some (`Eq 2)) [ [ "foo"; "toto" ] ] [ [ "foo"; "bar" ] ] in
      fold (Some (`Lt 2)) [] [ []; [ "foo" ] ];
      let () =
        fold
          (Some (`Le 2))
          [ [ "foo"; "toto" ] ]
          [ []; [ "foo" ]; [ "foo"; "bar" ] ]
      in
      let () =
        fold
          (Some (`Ge 2))
          [ [ "foo"; "toto" ]; [ "foo"; "bar"; "toto" ] ]
          [ [ "foo"; "bar" ] ]
      in
      fold (Some (`Gt 2)) [ [ "foo"; "bar"; "toto" ] ] [];
      let v1 = S.Tree.remove v1 [ "foo"; "bar"; "toto" ] in
      let v = S.Tree.find v1 [ "foo"; "toto" ] in
      Alcotest.(check (option string)) "remove" (Some foo1) v;
      let v1 = S.Tree.empty () in
      let s = S.Tree.stats v1 in
      Alcotest.(check stats_t) "empty stats" empty_stats s;
      let v1 = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      let v1 = S.Tree.add v1 [ "foo"; "2" ] foo2 in
      let s = S.Tree.stats v1 in
      Alcotest.(check stats_t)
        "stats 1"
        { S.Tree.nodes = 2; leafs = 2; skips = 0; depth = 2; width = 2 }
        s;
      let v1 = S.Tree.remove v1 [ "foo"; "1" ] in
      let v1 = S.Tree.remove v1 [ "foo"; "2" ] in
      let s = S.Tree.stats v1 in
      Alcotest.(check stats_t) "empty stats" empty_stats s;
      S.set_tree_exn t ~info:(infof "empty tree") [] v1;
      let head = S.Head.get t in
      S.Commit.key head |> fun head ->
      let commit = B.Commit.find (ct repo) head in
      let node = B.Commit.Val.node (get commit) in
      let node = B.Node.find (n repo) node in
      check
        T.(option B.Node.Val.t)
        "empty tree"
        (Some (B.Node.Val.empty ()))
        node;

      (* Testing [Tree.diff] *)
      let contents_t = T.pair S.contents_t S.metadata_t in
      let diff = T.(pair S.path_t (Irmin.Diff.t contents_t)) in
      let check_diffs = checks diff in
      let check_val = check T.(option contents_t) in
      let check_ls = checks T.(pair S.step_t S.tree_t) in
      let normal c = Some (c, S.Metadata.default) in
      let d0 = S.Metadata.default in
      let v0 = S.Tree.empty () in
      let v1 = S.Tree.empty () in
      let v2 = S.Tree.empty () in
      let v1 = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      let f = S.Tree.find_all v1 [ "foo"; "1" ] in
      check_val "tree update" (normal foo1) f;
      let v1' = S.Tree.add v1 [ "foo"; "1" ] foo1 in
      Alcotest.(check bool) "Tree.add keeps sharing" true (v1 == v1');
      let v1' = S.Tree.remove v1 [ "foo"; "2" ] in
      Alcotest.(check bool) "Tree.remove keeps sharing" true (v1 == v1');
      let v1' = S.Tree.add_tree v1 [] v1 in
      Alcotest.(check bool) "Tree.add_tree keeps sharing" true (v1 == v1');
      let v2 = S.Tree.add v2 [ "foo"; "1" ] foo2 in
      let v2 = S.Tree.add v2 [ "foo"; "2" ] foo1 in
      let d1 = S.Tree.diff v0 v1 in
      check_diffs "diff 1" [ ([ "foo"; "1" ], `Added (foo1, d0)) ] d1;
      let d2 = S.Tree.diff v1 v0 in
      check_diffs "diff 2" [ ([ "foo"; "1" ], `Removed (foo1, d0)) ] d2;
      let d3 = S.Tree.diff v1 v2 in
      check_diffs "diff 3"
        [
          ([ "foo"; "1" ], `Updated ((foo1, d0), (foo2, d0)));
          ([ "foo"; "2" ], `Added (foo1, d0));
        ]
        d3;
      let v3 = S.Tree.add v2 [ "foo"; "bar"; "1" ] foo1 in
      let d4 = S.Tree.diff v2 v3 in
      check_diffs "diff 4" [ ([ "foo"; "bar"; "1" ], `Added (foo1, d0)) ] d4;
      let d5 = S.Tree.diff v3 v2 in
      check_diffs "diff 4" [ ([ "foo"; "bar"; "1" ], `Removed (foo1, d0)) ] d5;

      (* Testing length *)
      let check_length msg t =
        let n = S.Tree.length t [] in
        let l = S.Tree.list t [] in
        Alcotest.(check int) msg n (List.length l)
      in
      let () = check_length "bindings1 length" v2 in
      let () =
        let t = contents "foo" in
        check_length "contents length" t
      in

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
      let _ = S.set_tree_exn t ~info:(infof "add tree") [] tree in
      let e = S.Tree.get_tree tree [ "a" ] in
      let ls =
        [
          ("aa", contents "0");
          ("a", e);
          ("bbb", contents "3");
          ("b", contents "3");
          ("aaa", e);
        ]
      in
      let () =
        let l1 = S.Tree.list ~offset:0 ~length:2 tree [] in
        let l2 = S.Tree.list ~offset:2 ~length:2 tree [] in
        let l3 = S.Tree.list ~offset:4 ~length:2 tree [] in
        Alcotest.(check int) "size l1" 2 (List.length l1);
        Alcotest.(check int) "size l2" 2 (List.length l2);
        Alcotest.(check int) "size l3" 1 (List.length l3);
        check_ls "2 paginated list" ls (l1 @ l2 @ l3)
      in
      let () =
        let l1 = S.Tree.list ~offset:0 ~length:3 tree [] in
        let l2 = S.Tree.list ~offset:3 ~length:6 tree [] in
        Alcotest.(check int) "size l1" 3 (List.length l1);
        Alcotest.(check int) "size l2" 2 (List.length l2);
        check_ls "3 paginated list" ls (l1 @ l2)
      in
      let () =
        let l1 = S.Tree.list ~offset:0 ~length:4 tree [] in
        let l2 = S.Tree.list ~offset:4 ~length:4 tree [] in
        Alcotest.(check int) "size l1" 4 (List.length l1);
        Alcotest.(check int) "size l2" 1 (List.length l2);
        check_ls "4 paginated list" ls (l1 @ l2)
      in
      let () =
        let l1 = S.Tree.list ~offset:0 ~length:5 tree [] in
        let l2 = S.Tree.list ~offset:5 ~length:5 tree [] in
        Alcotest.(check int) "size l1" 5 (List.length l1);
        Alcotest.(check int) "size l2" 0 (List.length l2);
        check_ls "5 paginated list" ls (l1 @ l2)
      in
      let c0 =
        S.Tree.singleton [ "foo"; "a" ] "1"
        |> with_binding [ "foo"; "b"; "c" ] "2"
        |> with_binding [ "foo"; "c" ] "3"
        |> with_binding [ "foo"; "d" ] "4"
      in
      let b = S.Tree.get_tree c0 [ "foo"; "b" ] in
      let ls = S.Tree.list c0 [ "foo" ] in
      check_ls "list all"
        [
          ("a", contents "1"); ("b", b); ("c", contents "3"); ("d", contents "4");
        ]
        ls;
      let ls = S.Tree.list ~offset:2 c0 [ "foo" ] in
      check_ls "list offset=2" [ ("c", contents "3"); ("d", contents "4") ] ls;
      let ls = S.Tree.list ~offset:2 ~length:1 c0 [ "foo" ] in
      check_ls "list offset=2 length=1" [ ("c", contents "3") ] ls;
      let ls = S.Tree.list ~length:1 c0 [ "foo" ] in
      check_ls "list length=1" [ ("a", contents "1") ] ls;

      (* Testing concrete representation *)
      let c0 =
        S.Tree.empty ()
        |> with_binding [ "foo"; "a" ] "1"
        |> with_binding [ "foo"; "b"; "c" ] "2"
        |> with_binding [ "bar"; "d" ] "3"
        |> with_binding [ "e" ] "4"
      in
      let t0 = c0 |> S.Tree.to_concrete |> S.Tree.of_concrete in
      let () =
        let d0 = S.Tree.diff c0 t0 in
        check_diffs "concrete roundtrip" [] d0
      in
      let () =
        let c0' = S.Tree.list c0 [] in
        let t0' = S.Tree.list t0 [] in
        check_ls "concrete list /" c0' t0'
      in
      let () =
        let c0' = S.Tree.list c0 [ "foo" ] in
        let t0' = S.Tree.list t0 [ "foo" ] in
        check_ls "concrete tree list /foo" c0' t0'
      in
      let () =
        let c0' = S.Tree.list c0 [ "bar"; "d" ] in
        let t0' = S.Tree.list t0 [ "bar"; "d" ] in
        check_ls "concrete tree list /bar/d" c0' t0'
      in

      (* Testing other tree operations. *)
      let v0 = S.Tree.empty () in
      let c = S.Tree.to_concrete v0 in
      (match c with
      | `Tree [] -> ()
      | _ -> Alcotest.fail "Excpected empty tree");
      let v0 = S.Tree.add v0 [] foo1 in
      let foo1' = S.Tree.find_all v0 [] in
      check_val "read /" (normal foo1) foo1';
      let v0 = S.Tree.add v0 [ "foo"; "1" ] foo1 in
      let foo1' = S.Tree.find_all v0 [ "foo"; "1" ] in
      check_val "read foo/1" (normal foo1) foo1';
      let v0 = S.Tree.add v0 [ "foo"; "2" ] foo2 in
      let foo2' = S.Tree.find_all v0 [ "foo"; "2" ] in
      check_val "read foo/2" (normal foo2) foo2';
      let check_tree v =
        let ls = S.Tree.list v [ "foo" ] in
        check_ls "path1" [ ("1", contents foo1); ("2", contents foo2) ] ls;
        let foo1' = S.Tree.find_all v [ "foo"; "1" ] in
        check_val "foo1" (normal foo1) foo1';
        let foo2' = S.Tree.find_all v [ "foo"; "2" ] in
        check_val "foo2" (normal foo2) foo2'
      in
      let v0 = List.fold_left (fun v0 (k, v) -> S.Tree.add v0 k v) v0 nodes in
      check_tree v0;
      S.set_tree_exn t ~info:(infof "update_path b/") [ "b" ] v0;
      S.set_tree_exn t ~info:(infof "update_path a/") [ "a" ] v0;
      let ls = S.list t [ "b"; "foo" ] in
      check_ls "path2" [ ("1", contents foo1); ("2", contents foo2) ] ls;
      let foo1' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "foo1" (normal foo1) foo1';
      let foo2' = S.find_all t [ "a"; "foo"; "2" ] in
      check_val "foo2" (normal foo2) foo2';
      let v0 = S.get_tree t [ "b" ] in
      check_tree v0;
      S.set_exn t ~info:(infof "update b/x") [ "b"; "x" ] foo1;
      let v2 = S.get_tree t [ "b" ] in
      let v1 = S.Tree.add v0 [ "y" ] foo2 in
      let v' =
        Irmin.Merge.(f S.Tree.merge ~old:(promise v0) v1 v2)
        |> merge_exn "merge trees"
      in
      S.set_tree_exn t ~info:(infof "merge_path") [ "b" ] v';
      let foo1' = S.find_all t [ "b"; "x" ] in
      let foo2' = S.find_all t [ "b"; "y" ] in
      check_val "merge: b/x" (normal foo1) foo1';
      check_val "merge: b/y" (normal foo2) foo2';
      let () =
        List.iteri
          (fun i (k, v) ->
            let v' = S.find_all t ("a" :: k) in
            check_val ("a" ^ string_of_int i) (normal v) v';
            let v' = S.find_all t ("b" :: k) in
            check_val ("b" ^ string_of_int i) (normal v) v')
          nodes
      in
      let v2 = S.get_tree t [ "b" ] in
      let _ = S.Tree.find_all v2 [ "foo"; "1" ] in
      let v2 = S.Tree.add v2 [ "foo"; "1" ] foo2 in
      S.set_tree_exn t ~info:(infof "v2") [ "b" ] v2;
      let foo2' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "update tree" (normal foo2) foo2';
      let v3 = S.get_tree t [ "b" ] in
      let _ = S.Tree.find_all v3 [ "foo"; "1" ] in
      let v3 = S.Tree.remove v3 [ "foo"; "1" ] in
      S.set_tree_exn t ~info:(infof "v3") [ "b" ] v3;
      let foo2' = S.find_all t [ "b"; "foo"; "1" ] in
      check_val "remove tree" None foo2';
      let r1 = r1 ~repo in
      let r2 = r2 ~repo in
      let i0 = S.Info.empty in
      let c =
        S.Commit.v repo ~info:S.Info.empty
          ~parents:[ S.Commit.key r1; S.Commit.key r2 ]
          v3
      in
      S.Head.set t c;
      let h = S.Head.get t in
      S.Commit.info h |> fun i ->
      check S.Info.t "commit info" i0 i;
      let tt = S.of_commit h in
      let g = S.history tt in
      let pred = S.History.pred g h in
      checks (S.commit_t repo) "head" [ r1; r2 ] pred;
      let foo2'' = S.find_all tt [ "b"; "foo"; "1" ] in
      check_val "remove tt" None foo2'';
      let vx = "VX" in
      let px = [ "x"; "y"; "z" ] in
      S.set_exn tt ~info:(infof "update") px vx;
      let tree = S.get_tree tt [] in
      S.Tree.clear tree;
      let s = S.Tree.stats tree in
      Alcotest.(check stats_t)
        "lazy stats"
        { S.Tree.nodes = 0; leafs = 0; skips = 1; depth = 0; width = 0 }
        s;
      S.Tree.clear tree;
      let s = S.Tree.stats ~force:true tree in
      Alcotest.(check stats_t)
        "forced stats"
        { S.Tree.nodes = 404; leafs = 103; skips = 0; depth = 5; width = 103 }
        s;
      let vx' = S.Tree.find_all tree px in
      check_val "updates" (normal vx) vx';
      let v = S.Tree.singleton [] vx in
      let () = S.set_tree_exn t ~info:(infof "update file as tree") [ "a" ] v in
      let vx' = S.find_all t [ "a" ] in
      check_val "update file as tree" (normal vx) vx';
      B.Repo.close repo
    in
    run x test

  let pp_proof = Irmin.Type.pp (S.Tree.Proof.t S.Tree.Proof.tree_t)
  let pp_stream = Irmin.Type.pp (S.Tree.Proof.t S.Tree.Proof.stream_t)

  let test_proofs x () =
    let test repo =
      (* Testing Merkle proof *)
      let large_dir =
        List.init 1000 (fun i ->
            let v = string_of_int i in
            ([ "dir"; v ], "BLOB:" ^ v))
      in
      let c0 =
        S.Tree.empty ()
        |> with_binding [ "foo"; "a" ] "1"
        |> with_binding [ "foo"; "b"; "c" ] "2"
        |> with_binding [ "bar"; "d" ] "3"
        |> with_binding [ "e" ] "4"
        |> fun t ->
        List.fold_left (fun acc (k, v) -> S.Tree.add acc k v) t large_dir
      in
      let to_proof t =
        let store = S.empty repo in
        let () = S.set_tree_exn ~info:(infof "to_proof") store [] t in
        let key =
          match S.Tree.key t with None -> assert false | Some k -> k
        in
        let rec aux p t =
          let bindings =
            try S.Tree.list t [] with
            | S.Tree.Pruned_hash _ -> []
            | e -> raise e
          in
          List.iter (fun (s, v) -> aux (p @ [ s ]) v) bindings
        in
        S.Tree.produce_proof repo key (fun t ->
            let () = aux [] t in
            (t, ()))
      in
      let p0, () = to_proof c0 in
      [%log.debug "p0=%a" pp_proof p0];
      let t0 = S.Tree.Proof.to_tree p0 in
      let () =
        let d0 = S.Tree.diff c0 t0 in
        check_diffs "proof roundtrip" [] d0
      in
      let () =
        let c0' = S.Tree.list c0 [] in
        let t0' = S.Tree.list t0 [] in
        check_ls "proof list /" c0' t0'
      in
      let () =
        let c0' = S.Tree.list c0 [ "foo" ] in
        let t0' = S.Tree.list t0 [ "foo" ] in
        check_ls "proof tree list /foo" c0' t0'
      in
      let () =
        let c0' = S.Tree.list c0 [ "bar"; "d" ] in
        let t0' = S.Tree.list t0 [ "bar"; "d" ] in
        check_ls "proof tree list /bar/d" c0' t0'
      in
      let () =
        let c0' = S.Tree.list c0 [ "dir" ] in
        let t0' = S.Tree.list t0 [ "dir" ] in
        check_ls "proof tree list /dir" c0' t0'
      in
      let add_noise n prefix =
        List.map (fun k -> (prefix @ [ k ], k)) (List.init n string_of_int)
      in
      let bindings =
        [
          ([ "foo"; "age" ], "0");
          ([ "foo"; "version" ], "1");
          ([ "bar"; "age" ], "2");
          ([ "bar"; "version" ], "3");
        ]
        @ add_noise 100 [ "foo" ]
        @ add_noise 10 [ "hey" ]
        @ add_noise 50 [ "bar" ]
      in
      let increment = function
        | None -> assert false
        | Some i -> Some (int_of_string i + 1 |> string_of_int)
      in
      let check_proof_f0 p =
        let t = S.Tree.Proof.to_tree p in
        let i = S.Tree.find t [ "bar"; "age" ] in
        Alcotest.(check (option string))
          "inside: find bar/age in proof" (Some "2") i;
        let i = S.Tree.find t [ "bar"; "version" ] in
        Alcotest.(check (option string))
          "inside: find bar/version in proof" (Some "3") i;
        let i = S.Tree.find t [ "hello"; "there" ] in
        Alcotest.(check (option string))
          "inside: do not find hello/there in proof" None i;
        let () =
          try
            let _ = S.Tree.find t [ "foo"; "version" ] in
            Alcotest.fail "inside: should have raise: pruned_hash exn"
          with
          | S.Tree.Pruned_hash _ | B.Node.Val.Dangling_hash _ -> ()
          | e -> raise e
        in
        ()
      in

      let check_proof_f1 p =
        let t = S.Tree.Proof.to_tree p in
        let i = S.Tree.find t [ "foo"; "version" ] in
        Alcotest.(check (option string))
          "outside: find foo/version" (Some "1") i
      in

      let init_tree bindings =
        let tree = S.Tree.empty () in
        let tree =
          List.fold_left (fun tree (k, v) -> S.Tree.add tree k v) tree bindings
        in
        let store = S.empty repo in
        let () = S.set_tree_exn ~info:(infof "init_tree") store [] tree in
        S.tree store
      in
      let tree = init_tree bindings in
      let key =
        match S.Tree.key tree with None -> assert false | Some k -> k
      in

      let f0 t0 =
        let t1 = S.Tree.update t0 [ "foo"; "age" ] increment in
        let t2 = S.Tree.update t1 [ "bar"; "age" ] increment in
        let t3 = S.Tree.get_tree t2 [ "bar" ] in
        let t4 = S.Tree.add_tree t2 [ "hello"; "there" ] t3 in
        let v = S.Tree.get t4 [ "hello"; "there"; "version" ] in
        Alcotest.(check string) "hello/there/version" "3" v;
        let t = S.Tree.empty () in
        let t5 = S.Tree.add_tree t [ "dir1"; "dir2" ] t4 in
        let v = S.Tree.get t5 [ "dir1"; "dir2"; "bar"; "age" ] in
        Alcotest.(check string) "dir1/dir2/bar/age" "3" v;
        let t = S.Tree.remove t4 [ "bar" ] in

        (* Trigger certain paths in [S.Tree] during "verify" *)
        let portable =
          (* During "verify" [portable] is [Pruned] with [portable] in env *)
          t0
        in
        let portable_dirty = t in
        let trigger_node_to_map t =
          S.Tree.fold ~depth:(`Eq 1) ~order:`Sorted ~force:`True t ()
        in
        let () = trigger_node_to_map portable in
        let () = trigger_node_to_map portable_dirty in
        let trigger_node_length t =
          let (_ : int) = S.Tree.length t [] in
          ()
        in
        let () = trigger_node_length portable in
        let () = trigger_node_length portable_dirty in
        let trigger_node_fold_undefined t =
          S.Tree.fold ~depth:(`Eq 1) ~order:`Undefined ~force:`True t ()
        in
        let () = trigger_node_fold_undefined portable in
        let () = trigger_node_fold_undefined portable_dirty in
        let (_ : bool) = S.Tree.is_empty portable in
        let trigger_node_to_backend_portable t =
          match S.Tree.destruct t with
          | `Contents _ -> assert false
          | `Node n ->
              let _ = S.to_backend_portable_node n in
              ()
        in
        let () = trigger_node_to_backend_portable portable_dirty in
        (t, ())
      in
      let f1 t0 =
        let p0, () = S.Tree.produce_proof repo key f0 in
        let () = check_proof_f0 p0 in
        let v = S.Tree.get t0 [ "foo"; "version" ] in
        Alcotest.(check string) "foo/version" "1" v;
        (t0, ())
      in
      let p, () = S.Tree.produce_proof repo key f1 in

      let () = check_proof_f1 p in

      let check_proof f =
        let p, () = S.Tree.produce_proof repo key f in
        [%log.debug "Verifying proof %a" pp_proof p];
        let r = S.Tree.verify_proof p f in
        match r with
        | Ok (_, ()) -> ()
        | Error e ->
            Alcotest.failf "check_proof: %a"
              (Irmin.Type.pp S.Tree.verifier_error_t)
              e
      in
      let () = List.iter check_proof [ f0; f1 ] in

      let check_stream f =
        let p, () = S.Tree.produce_stream repo key f in
        [%log.debug "Verifying stream %a" pp_stream p];
        let r = S.Tree.verify_stream p f in
        match r with
        | Ok (_, ()) -> ()
        | Error e ->
            Alcotest.failf "check_stream: %a"
              (Irmin.Type.pp S.Tree.verifier_error_t)
              e
      in
      let () = List.iter check_stream [ f0; f1 ] in

      (* check env sharing *)
      let tree () =
        S.Tree.of_concrete
          (`Tree [ ("foo", `Contents ("bar", S.Metadata.default)) ])
      in
      let contents () =
        S.Tree.of_concrete (`Contents ("bar", S.Metadata.default))
      in
      let check_env_empty msg t b =
        let env = S.Tree.Private.get_env t in
        Alcotest.(check bool) msg b (S.Tree.Private.Env.is_empty env)
      in
      let check_env msg t t' =
        let env = S.Tree.Private.get_env t in
        let env' = S.Tree.Private.get_env t' in
        check S.Tree.Private.Env.t msg env env'
      in
      let x = ref None in
      let _ =
        S.Tree.produce_proof repo key (fun t ->
            check_env_empty "env should be set inside the proof" t false;
            x := Some t;

            let t0 = tree () in
            check_env_empty "env should not be set for fresh trees" t0 true;

            (* test changing subtress: check that envirnoment is
               attached only the tree roots *)
            let t1 = S.Tree.add_tree t [ "foo" ] t0 in
            check_env_empty "1: t's env should not change" t false;
            check_env_empty "1: t0's env should not change" t0 true;
            check_env "1: t1's env should be the same as t's" t1 t;

            let t0 = contents () in
            let t1 = S.Tree.add_tree t [ "foo" ] t0 in
            check_env_empty "2: t's env should not change" t false;
            check_env_empty "2: t0's env should not change" t0 true;
            check_env "2: t1's env should be the same as t's" t1 t;

            (* test changing roots *)
            let t0 = tree () in
            let t1 = S.Tree.add_tree t [] t0 in
            check_env_empty "3: t's env should not change" t false;
            check_env_empty "3: t0's env should not change" t0 true;
            check_env "3: t1's env should be the same as t0's" t1 t0;

            let t0 = contents () in
            let t1 = S.Tree.add_tree t [] t0 in
            check_env_empty "4: t's env should not change" t false;
            check_env_empty "4: t0's env should not change" t0 true;
            check_env "4: t1's env should be the same as t0's" t1 t0;

            (* check subtrees *)
            let t2 = S.Tree.get_tree t [ "foo" ] in
            check_env "5: t2's env should be the same as t's" t2 t;
            let t3 = S.Tree.get_tree t [ "foo"; "age" ] in
            check_env "5: t3's env should be the same as t's" t3 t;

            (t, ()))
      in
      let t = match !x with Some t -> t | None -> assert false in
      check_env_empty "env is unset outside of the proof)" t true;

      (* test negative proofs *)
      let check_bad_proof p =
        let r = S.Tree.verify_proof p f0 in
        match r with
        | Ok _ -> Alcotest.fail "verify should have failed"
        | Error _ -> ()
      in

      let p0, () = S.Tree.produce_proof repo key f0 in
      let proof ?(before = S.Tree.Proof.before p0)
          ?(after = S.Tree.Proof.after p0) ?(state = S.Tree.Proof.state p0) () =
        S.Tree.Proof.v ~before ~after state
      in
      let wrong_hash = B.Contents.Hash.hash "not the right hash!" in
      let wrong_kinded_hash = `Node wrong_hash in
      let () = check_bad_proof (proof ~before:wrong_kinded_hash ()) in
      let () = check_bad_proof (proof ~after:wrong_kinded_hash ()) in
      let _ = S.Tree.verify_proof (proof ()) f0 in
      let some_contents : S.Tree.Proof.tree list =
        [
          Blinded_node wrong_hash;
          Node [];
          Inode { length = 1024; proofs = [] };
          Blinded_contents (wrong_hash, S.Metadata.default);
          Contents ("yo", S.Metadata.default);
        ]
      in
      let () =
        List.iter (fun c -> check_bad_proof (proof ~state:c ())) some_contents
      in

      (* test negative streams *)
      let check_bad_stream p =
        let r = S.Tree.verify_stream p f0 in
        match r with
        | Ok _ ->
            Alcotest.failf "verify_stream should have failed %a" pp_stream p
        | Error _ -> ()
      in

      let p0, () = S.Tree.produce_stream repo key f0 in
      let proof ?(before = S.Tree.Proof.before p0)
          ?(after = S.Tree.Proof.after p0) ?(contents = S.Tree.Proof.state p0)
          () =
        S.Tree.Proof.v ~before ~after contents
      in
      let wrong_hash = B.Contents.Hash.hash "not the right hash!" in
      let wrong_kinded_hash = `Node wrong_hash in
      let () = check_bad_stream (proof ~before:wrong_kinded_hash ()) in
      let () = check_bad_stream (proof ~after:wrong_kinded_hash ()) in
      let _ = S.Tree.verify_stream (proof ()) f0 in
      let some_contents : S.Tree.Proof.stream list =
        let s : S.Tree.Proof.elt list -> S.Tree.Proof.stream = List.to_seq in
        let ok = List.of_seq (S.Tree.Proof.state p0) in
        [
          s [];
          s [ Node [] ];
          s [ Inode { length = 1024; proofs = [] } ];
          s [ Contents "yo" ];
          s (ok @ [ Node [] ]);
        ]
      in
      let () =
        let x = ref 1 in
        List.iter
          (fun c ->
            incr x;
            check_bad_stream (proof ~contents:c ()))
          some_contents
      in

      B.Repo.close repo
    in
    run x test

  let test_wide_nodes x () =
    let test repo =
      let size = 500_000 in
      let c0 = S.Tree.empty () in
      let rec wide_node i c =
        if i >= size then c
        else
          S.Tree.add c [ "foo"; string_of_int i ] (string_of_int i) |> fun c ->
          wide_node (i + 1) c
      in
      wide_node 0 c0 |> fun c ->
      S.Tree.list c [ "foo" ] |> fun ls ->
      Alcotest.(check int) "list wide dir" size (List.length ls);
      S.Tree.fold ~force:`True c ~uniq:`False
        ~contents:(fun k _ i ->
          Alcotest.(check int) "contents at [foo; i]" (List.length k) 2;
          i + 1)
        ~node:(fun k _ i ->
          if not (List.length k = 0 || List.length k = 1) then
            Alcotest.failf "nodes should be at [] and [foo], got %a"
              (Irmin.Type.pp S.path_t) k;
          i)
        0
      |> fun nb_contents ->
      Alcotest.(check int) "nb of contents folded over" size nb_contents;
      S.Tree.remove c [ "foo"; "499999" ] |> fun c1 ->
      S.Tree.add c0 [] "499999" |> fun c2 ->
      S.Tree.add_tree c1 [ "foo"; "499999" ] c2 |> fun c' ->
      let h' = S.Tree.hash c' in
      let h = S.Tree.hash c in
      check S.Hash.t "same tree" h h';
      let c1 = S.Tree.get_tree c [ "foo" ] in
      let _ =
        S.Backend.Repo.batch repo (fun c n _ -> S.save_tree repo c n c1)
      in
      (match S.Tree.destruct c1 with
      | `Contents _ -> Alcotest.fail "got `Contents, expected `Node"
      | `Node node -> (
          let v = S.to_backend_node node in
          let () =
            let ls = B.Node.Val.list v in
            Alcotest.(check int) "list wide node" size (List.length ls)
          in
          let bar_key = with_contents repo (fun t -> B.Contents.add t "bar") in
          let k = normal bar_key in
          let v1 = B.Node.Val.add v "x" k in
          let () =
            let h' = B.Node.Hash.hash v1 in
            let h = with_node repo (fun n -> B.Node.add n v1) in
            check B.Node.Hash.t "wide node + x: hash(v) = add(v)"
              (B.Node.Key.to_hash h) h'
          in
          let () =
            let v2 = B.Node.Val.add v "x" k in
            check B.Node.Val.t "add x" v1 v2
          in
          let () =
            let v0 = B.Node.Val.remove v1 "x" in
            check B.Node.Val.t "remove x" v v0
          in
          let () =
            let v3 = B.Node.Val.remove v "1" in
            let h' = B.Node.Hash.hash v3 in
            with_node repo (fun n -> B.Node.add n v3) |> fun h ->
            check B.Node.Hash.t "wide node - 1 : hash(v) = add(v)"
              (B.Node.Key.to_hash h) h'
          in
          (match B.Node.Val.find v "499999" with
          | None | Some (`Node _) -> Alcotest.fail "value 499999 not found"
          | Some (`Contents (x, _)) ->
              let x = B.Contents.Key.to_hash x in
              let x' = B.Contents.Hash.hash "499999" in
              check B.Contents.Hash.t "find 499999" x x');
          match B.Node.Val.find v "500000" with
          | None -> ()
          | Some _ -> Alcotest.fail "value 500000 should not be found"));
      B.Repo.close repo
    in
    run x test

  let test_commit_wide_node x () =
    let test repo =
      let size = 500_000 in
      let c0 = S.Tree.empty () in
      let rec wide_node i c =
        if i >= size then c
        else
          S.Tree.add c [ "foo"; string_of_int i ] (string_of_int i) |> fun c ->
          wide_node (i + 1) c
      in
      wide_node 0 c0 |> fun c ->
      S.main repo |> fun t ->
      S.set_tree_exn t [ "wide" ] ~info:(infof "commit_wide_nodes") c;
      S.list t [ "wide"; "foo" ] |> fun ls ->
      Alcotest.(check int) "commit wide node list" size (List.length ls);
      B.Repo.close repo
    in
    run x test

  module Sync = Irmin.Sync.Make (S)

  let test_sync x () =
    let test repo =
      let t1 = S.main repo in
      S.set_exn t1 ~info:(infof "update a/b") [ "a"; "b" ] v1;
      let h = S.Head.get t1 in
      let _r1 = S.Head.get t1 in
      S.set_exn t1 ~info:(infof "update a/c") [ "a"; "c" ] v2;
      let r2 = S.Head.get t1 in
      S.set_exn t1 ~info:(infof "update a/d") [ "a"; "d" ] v1;
      let _r3 = S.Head.get t1 in
      let h = S.history t1 ~min:[ h ] in
      Alcotest.(check int) "history-v" 3 (S.History.nb_vertex h);
      Alcotest.(check int) "history-e" 2 (S.History.nb_edges h);
      let remote = Irmin.remote_store (module S) t1 in
      let partial = Sync.fetch_exn t1 ~depth:0 remote in
      let partial =
        match partial with
        | `Head x -> x
        | `Empty -> failwith "no head: partial"
      in
      let full = Sync.fetch_exn t1 remote in
      let full =
        match full with `Head x -> x | `Empty -> failwith "no head: full"
      in
      (* Restart a fresh store and import everything in there. *)
      let tag = "export" in
      let t2 = S.of_branch repo tag in
      S.Head.set t2 partial;
      let b1 = S.mem t2 [ "a"; "b" ] in
      Alcotest.(check bool) "mem-ab" true b1;
      let b2 = S.mem t2 [ "a"; "c" ] in
      Alcotest.(check bool) "mem-ac" true b2;
      let b3 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ad" true b3;
      let v1' = S.get t2 [ "a"; "d" ] in
      check S.contents_t "v1" v1 v1';
      S.Head.set t2 r2;
      let b4 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ab" false b4;
      S.Head.set t2 full;
      S.Head.set t2 r2;
      let b4 = S.mem t2 [ "a"; "d" ] in
      Alcotest.(check bool) "mem-ad" false b4;
      B.Repo.close repo
    in
    run x test

  module Dot = Irmin.Dot (S)

  let output_file x t file =
    let buf = Buffer.create 1024 in
    let date d =
      let tm = Unix.localtime (Int64.to_float d) in
      Fmt.str "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Dot.output_buffer t ~date buf;
    let oc =
      open_out_bin
        (Filename.get_temp_dir_name () / Fmt.str "%s-%s.dot" x.name file)
    in
    output_string oc (Buffer.contents buf);
    close_out oc

  let test_merge ?hook x () =
    let test repo =
      let v1 = "X1" in
      let v2 = "X2" in
      let v3 = "X3" in
      let t1 = S.main repo in
      let () = S.set_exn t1 ~info:(infof "update a/b/a") [ "a"; "b"; "a" ] v1 in
      let () = S.set_exn t1 ~info:(infof "update a/b/b") [ "a"; "b"; "b" ] v2 in
      let () = S.set_exn t1 ~info:(infof "update a/b/c") [ "a"; "b"; "c" ] v3 in
      let test = "test" in
      let t2 = S.clone ~src:t1 ~dst:test in
      let () =
        S.set_exn t1 ~info:(infof "update main:a/b/b") [ "a"; "b"; "b" ] v1
      in
      let () =
        S.set_exn t1 ~info:(infof "update main:a/b/b") [ "a"; "b"; "b" ] v3
      in
      let () =
        S.set_exn t2 ~info:(infof "update test:a/b/c") [ "a"; "b"; "c" ] v1
      in
      output_file x t1 "before";
      let m = S.merge_into ~info:(infof "merge test into main") t2 ~into:t1 in
      merge_exn "m" m;
      may_with_branch [ t1 ] repo hook;
      output_file x t1 "after";
      let v1' = S.get t1 [ "a"; "b"; "c" ] in
      let v2' = S.get t2 [ "a"; "b"; "b" ] in
      let v3' = S.get t1 [ "a"; "b"; "b" ] in
      check S.contents_t "v1" v1 v1';
      check S.contents_t "v2" v2 v2';
      check S.contents_t "v3" v3 v3';
      B.Repo.close repo
    in
    run x test

  (* in this test an outdated reference to a tree is used by a commit: [tree] is
     the tree with root [x] created by [c1] and modified by [c2]. [c3] reuse [tree]
     which implicitly deletes the changes of [c2]. *)
  let test_merge_outdated_tree x () =
    let check_val = check T.(option S.contents_t) in
    let none_fail f msg =
      f |> function None -> Alcotest.fail msg | Some c -> c
    in
    let test repo =
      let vx = "VX" in
      let vy = "VY" in
      let old () = Ok None in
      let t = S.main repo in
      S.set_exn t ~info:(infof "add x/y/z") [ "x"; "y"; "z" ] vx;
      let _c1 = none_fail (S.Head.find t) "head not found" in
      let tree = S.get_tree t [ "x" ] in
      S.set_exn t ~info:(infof "add u/x/y") [ "u"; "x"; "y" ] vy;
      let c2 = none_fail (S.Head.find t) "head not found" in
      let tree3 = S.Tree.add tree [ "x"; "z" ] vx in
      S.set_tree_exn t ~info:(infof "update") [ "u" ] tree3;
      let c3 = none_fail (S.Head.find t) "head not found" in
      let info () = S.Commit.info c3 in
      with_commit repo (fun commit_t ->
          Irmin.Merge.f
            (B.Commit.merge commit_t ~info)
            ~old
            (Some (S.Commit.key c3))
            (Some (S.Commit.key c2)))
      |> merge_exn "commit"
      |> function
      | None -> ()
      | Some c4 ->
          let k = none_fail (S.Commit.of_key repo c4) "of hash" in
          S.Branch.set repo "foo" k;
          let t = S.of_branch repo "foo" in
          let vy' = S.find t [ "u"; "x"; "y" ] in
          check_val "vy after merge" None vy';
          B.Repo.close repo
    in
    run x test

  let test_merge_unrelated ?hook x () =
    run x @@ fun repo ->
    let v1 = "X1" in
    let foo = S.of_branch repo "foo" in
    let bar = S.of_branch repo "bar" in
    S.set_exn foo ~info:(infof "update foo:a") [ "a" ] v1;
    S.set_exn bar ~info:(infof "update bar:b") [ "b" ] v1;
    may_with_branch [ foo; bar ] repo hook;
    let _ =
      S.merge_into ~info:(infof "merge bar into foo") bar ~into:foo
      |> merge_exn "merge unrelated"
    in
    B.Repo.close repo

  let rec write fn = function
    | 0 -> []
    | i -> (fun () -> fn i |> Eio.Fiber.yield) :: write fn (i - 1)

  let perform l = List.iter (fun f -> f ()) l

  let rec read fn check = function
    | 0 -> []
    | i -> (fun () -> fn i |> fun v -> check i v) :: read fn check (i - 1)

  let test_concurrent_low x () =
    let test_branches repo () =
      let k = b1 in
      let v = r1 ~repo in
      let write = write (fun _i -> S.Branch.set repo k v) in
      let read =
        read
          (fun _i -> S.Branch.find repo k |> get)
          (fun i -> check (S.commit_t repo) (Fmt.str "tag %d" i) v)
      in
      perform (write 1);
      perform (write 10 @ read 10 @ write 10 @ read 10)
    in
    let test_contents repo () =
      let k = kv2 ~repo in
      let v = v2 in
      let t = B.Repo.contents_t repo in
      let write =
        write (fun _i ->
            let _ = with_contents repo (fun t -> B.Contents.add t v) in
            ())
      in
      let read =
        read
          (fun _i -> B.Contents.find t k |> get)
          (fun i -> check S.contents_t (Fmt.str "contents %d" i) v)
      in
      perform (write 1);
      perform (write 10 @ read 10 @ write 10 @ read 10)
    in
    run x (fun repo ->
        Eio.Fiber.all [ test_branches repo; test_contents repo ];
        B.Repo.close repo)

  let test_concurrent_updates x () =
    let test_one repo =
      let k = [ "a"; "b"; "d" ] in
      let v = "X1" in
      let t1 = S.main repo in
      let t2 = S.main repo in
      let write t =
        write (fun i -> S.set_exn t ~info:(infof "update: one %d" i) k v)
      in
      let read t =
        read
          (fun _ -> S.get t k)
          (fun i -> check S.contents_t (Fmt.str "update: one %d" i) v)
      in
      perform (write t1 10 @ write t2 10);
      perform (read t1 10)
    in
    let test_multi repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.str "X%d" i in
      let t1 = S.main repo in
      let t2 = S.main repo in
      let write t =
        write (fun i ->
            S.set_exn t ~info:(infof "update: multi %d" i) (k i) (v i))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.str "update: multi %d" i) (v i))
      in
      perform (write t1 10 @ write t2 10);
      perform (read t1 10)
    in
    run x (fun repo ->
        test_one repo;
        test_multi repo;
        B.Repo.close repo)

  let test_concurrent_merges x () =
    let test repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.str "X%d" i in
      let t1 = S.main repo in
      let t2 = S.main repo in
      let write t n =
        write (fun i ->
            let tag = Fmt.str "tmp-%d-%d" n i in
            let m = S.clone ~src:t ~dst:tag in
            S.set_exn m ~info:(infof "update") (k i) (v i);
            Eio.Fiber.yield ();
            S.merge_into ~info:(infof "update: multi %d" i) m ~into:t
            |> merge_exn "update: multi")
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.str "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0);
      perform (write t1 1 10 @ write t2 2 10);
      perform (read t1 10);
      B.Repo.close repo
    in
    run x test

  let pp_write_error = Irmin.Type.pp S.write_error_t
  let tree_t = testable S.tree_t

  let test_with_tree x () =
    let test repo =
      let t = S.main repo in
      let update ?retries key strategy r w () =
        S.with_tree t ?retries ~info:(infof "with-tree") ~strategy key (fun _ ->
            let v = Eio.Stream.take r in
            Some (S.Tree.of_contents v))
        |> Eio.Stream.add w
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
        let rx = Eio.Stream.create 1 in
        let wx = Eio.Stream.create 1 in
        let ry = Eio.Stream.create 1 in
        let wy = Eio.Stream.create 1 in
        S.set_exn t ~info:(infof "init") [ "a" ] "0";
        Eio.Fiber.all
          [
            update [ "a" ] ~retries:0 `Set rx wx;
            update [ "a" ] ~retries:0 `Set ry wy;
            (fun () ->
              Eio.Stream.add rx "1";
              Eio.Stream.take wx |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "set x" "1" a;
              Eio.Stream.add ry "2";
              Eio.Stream.take wy |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "set y" "2" a);
          ]
      in
      let test_and_set () =
        let rx = Eio.Stream.create 1 in
        let wx = Eio.Stream.create 1 in
        let ry = Eio.Stream.create 1 in
        let wy = Eio.Stream.create 1 in
        let rz = Eio.Stream.create 1 in
        let wz = Eio.Stream.create 1 in
        S.set_exn t ~info:(infof "init") [ "a" ] "0";
        Eio.Fiber.all
          [
            update [ "a" ] ~retries:0 `Test_and_set rx wx;
            update [ "a" ] ~retries:0 `Test_and_set ry wy;
            update [ "a" ] ~retries:1 `Test_and_set rz wz;
            (fun () ->
              Eio.Stream.add rx "1";
              Eio.Stream.take wx |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set x" "1" a;
              Eio.Stream.add ry "2";
              let e = Eio.Stream.take wy in
              check_test (Some (S.Tree.of_contents "1")) e;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set y" "1" a;
              Eio.Stream.add rz "3";
              (* there's a conflict, the transaction is restarted so need to feed a
                 new value *)
              Eio.Stream.add rz "4";
              Eio.Stream.take wz |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "test-and-set z" "4" a);
          ]
      in
      let merge () =
        let rx = Eio.Stream.create 1 in
        let wx = Eio.Stream.create 1 in
        let ry = Eio.Stream.create 1 in
        let wy = Eio.Stream.create 1 in
        let rz = Eio.Stream.create 1 in
        let wz = Eio.Stream.create 1 in
        S.set_exn t ~info:(infof "init") [ "a" ] "0";
        Eio.Fiber.all
          [
            update [ "a" ] ~retries:0 `Merge rx wx;
            update [ "a" ] ~retries:0 `Merge ry wy;
            update [ "a" ] ~retries:1 `Merge rz wz;
            (fun () ->
              Eio.Stream.add rx "1";
              Eio.Stream.take wx |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "merge x" "1" a;
              Eio.Stream.add ry "2";
              Eio.Stream.take wy |> check_conflict;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "merge y" a "1";
              Eio.Stream.add rz "3";
              (* there's a conflict, the transaction is restarted so need to feed a
                 new value *)
              Eio.Stream.add rz "4";
              Eio.Stream.take wz |> check_ok;
              let a = S.get t [ "a" ] in
              Alcotest.(check string) "merge z" a "4");
          ]
      in
      set () |> test_and_set |> merge;
      B.Repo.close repo
    in
    run x test

  let test_concurrent_head_updates x () =
    let test repo =
      let k i = [ "a"; "b"; "c"; string_of_int i ] in
      let v i = Fmt.str "X%d" i in
      let t1 = S.main repo in
      let t2 = S.main repo in
      let retry d fn =
        let rec aux i =
          fn () |> function
          | true -> [%log.debug "%d: ok!" d]
          | false ->
              [%log.debug "%d: conflict, retrying (%d)." d i];
              aux (i + 1)
        in
        aux 1
      in
      let write t n =
        write (fun i ->
            retry i (fun () ->
                let test = S.Head.find t in
                let tag = Fmt.str "tmp-%d-%d" n i in
                let m = S.clone ~src:t ~dst:tag in
                S.set_exn m ~info:(infof "update") (k i) (v i);
                let set = S.Head.find m in
                Eio.Fiber.yield ();
                S.Head.test_and_set t ~test ~set))
      in
      let read t =
        read
          (fun i -> S.get t (k i))
          (fun i -> check S.contents_t (Fmt.str "update: multi %d" i) (v i))
      in
      S.set_exn t1 ~info:(infof "update") (k 0) (v 0);
      perform (write t1 1 5 @ write t2 2 5);
      perform (read t1 5);
      B.Repo.close repo
    in
    run x test

  let test_shallow_objects x () =
    let test repo =
      (* NOTE: A store of type `Irmin.Generic_key.S` does not currently expose
         functions for building nodes / commits with non-existent children, due to
         the need to have _keys_ for all store pointers.

          A future version of this API may support such operations (e.g. for
          constructing Merkle proofs), but until then we must synthesise test keys
          by adding test values to the correponding backend stores directly. *)
      let contents (s : string) : S.contents_key =
        with_contents repo (fun c -> B.Contents.add c s)
      in
      let node (s : string) : S.node_key =
        with_node repo (fun n ->
            let contents = contents s in
            let node = B.Node.Val.(add (empty ())) s (normal contents) in
            B.Node.add n node)
      in
      let commit (s : string) : S.commit_key =
        with_commit repo (fun c ->
            let node = node s in
            let commit = B.Commit.Val.v ~info:(info "") ~node ~parents:[] in
            B.Commit.add c commit)
      in
      let foo_k = node "foo" in
      let bar_k = node "bar" in
      let tree_1 = S.Tree.shallow repo (`Node foo_k) in
      let tree_2 = S.Tree.shallow repo (`Node bar_k) in
      let node_3 =
        let contents_foo = contents "foo" in
        S.Backend.Node.Val.of_list
          [
            ("foo", `Contents (contents_foo, S.Metadata.default));
            ("bar", `Node bar_k);
          ]
      in
      let tree_3 = S.Tree.of_node (S.of_backend_node repo node_3) in
      let _ =
        S.Backend.Repo.batch repo (fun c n _ -> S.save_tree repo c n tree_3)
      in
      let key_3 = get_node_key (Option.get (S.Tree.key tree_3)) in
      let info () = info "shallow" in
      let t = S.main repo in
      S.set_tree_exn t [ "1" ] tree_1 ~info;
      S.set_tree_exn t [ "2" ] tree_2 ~info;
      let h = S.Head.get t in
      let commit_v =
        let commit_foo = commit "foo" in
        S.Backend.Commit.Val.v ~info:(info ()) ~node:key_3
          ~parents:[ S.Commit.key h; commit_foo ]
      in
      let commit_key = with_commit repo (fun c -> B.Commit.add c commit_v) in
      let commit = S.of_backend_commit repo commit_key commit_v in
      S.set_tree_exn t [ "3" ] ~parents:[ commit ] tree_3 ~info;
      let t1 = S.find_tree t [ "1" ] in
      Alcotest.(check (option tree_t)) "shallow tree" (Some tree_1) t1;
      B.Repo.close repo
    in
    run x test

  let test_pre_hash_collisions x () =
    let pre_hash_of ty =
      let f = Irmin.Type.(pre_hash ty |> unstage) in
      fun x ->
        let buf = Buffer.create 0 in
        f x (Buffer.add_string buf);
        Buffer.contents buf
    in
    let rec add_entries acc = function
      | 0 -> acc
      | i ->
          let s = string_of_int i in
          let acc = S.Tree.add acc [ s ] s in
          add_entries acc (i - 1)
    in
    let equal_hash = Irmin.Type.(equal S.Hash.t |> unstage) in
    let test create_tree repo =
      let tree = create_tree () in
      let c = S.Commit.v repo ~info:S.Info.empty ~parents:[] tree in

      let node_b =
        S.Tree.destruct tree
        |> (function `Contents _ -> assert false | `Node n -> n)
        |> S.to_backend_node
      in
      let node_ph = pre_hash_of S.Backend.Node.Val.t node_b in
      let node_h = S.Backend.Node.Hash.hash node_b in

      let commit_b = S.to_backend_commit c in
      let commit_ph = pre_hash_of S.Backend.Commit.Val.t commit_b in
      let commit_h = S.Backend.Commit.Hash.hash commit_b in

      let blob_k =
        with_contents repo (fun t -> S.Backend.Contents.add t node_ph)
      in
      let blob_h = S.Backend.Contents.Key.to_hash blob_k in
      if equal_hash node_h blob_h then
        Alcotest.failf
          "node pre-hash attack succeeded. pre-hash is \"%s\". backend node is \
           %a."
          (String.escaped node_ph)
          (Irmin.Type.pp S.Backend.Node.Val.t)
          node_b;

      let blob_k =
        with_contents repo (fun t -> S.Backend.Contents.add t commit_ph)
      in
      let blob_h = S.Backend.Contents.Key.to_hash blob_k in
      if equal_hash commit_h blob_h then
        Alcotest.failf
          "commit pre-hash attack succeeded. pre-hash is \"%s\". backend \
           commit is %a."
          (String.escaped commit_ph)
          (Irmin.Type.pp S.Backend.Commit.Val.t)
          commit_b;

      S.Backend.Repo.close repo
    in
    (* Test collisions with the empty node (and its commit), *)
    run x (test @@ fun () -> S.Tree.empty ());
    (* with a length one node, *)
    run x (test @@ fun () -> add_entries (S.Tree.empty ()) 1);
    (* and with a length >256 node (which is the threshold for unstable inodes
       in irmin pack). *)
    run x (test @@ fun () -> add_entries (S.Tree.empty ()) 260)
end

let suite' l ?(prefix = "") (_, x) =
  let (module S) = Suite.store_generic_key x in
  let module T = Make (S) in
  (prefix ^ x.name, l)

let when_ b x = if b then x else []

let suite sleep (speed, x) =
  let (module S) = Suite.store_generic_key x in
  let module Zzz = struct
    let sleep = sleep
  end in
  let module T = Make (S) in
  let module T_graph = Store_graph.Make (S) in
  let module T_watch = Store_watch.Make (Log) (Zzz) (S) in
  let with_tree_enabled =
    (* Disabled for flakiness. See https://github.com/mirage/irmin/issues/1090. *)
    not
      (List.mem ~equal:String.equal (Suite.name x)
         [
           "FS";
           "FS.UNIX";
           "GIT";
           "GIT.UNIX";
           "HTTP.FS";
           "HTTP.FS.UNIX";
           "HTTP.GIT";
           "HTTP.GIT.UNIX";
         ])
  in
  suite'
    ([
       ("High-level operations on trees", speed, T.test_trees x);
       ("Basic operations on contents", speed, T.test_contents x);
       ("Basic operations on nodes", speed, T.test_nodes x);
       ("Basic operations on commits", speed, T.test_commits x);
       ("Basic operations on branches", speed, T.test_branches x);
       ("Hash operations on trees", speed, T.test_tree_hashes x);
       ("Basic merge operations", speed, T.test_simple_merges x);
       ("Test merges on tree updates", speed, T.test_merge_outdated_tree x);
       ("Tree caches and hashconsing", speed, T.test_tree_caches x);
       ("Tree proofs", speed, T.test_proofs x);
       ("Complex histories", speed, T.test_history x);
       ("Empty stores", speed, T.test_empty x);
       ("Backend node manipulation", speed, T.test_backend_nodes x);
       ("High-level store operations", speed, T.test_stores x);
       ("High-level atomic store operations", speed, T.test_atomic x);
       ("High-level store merges", speed, T.test_merge x);
       ("Unrelated merges", speed, T.test_merge_unrelated x);
       ("Low-level concurrency", speed, T.test_concurrent_low x);
       ("Concurrent updates", speed, T.test_concurrent_updates x);
       ("Concurrent head updates", speed, T.test_concurrent_head_updates x);
       ("Concurrent merges", speed, T.test_concurrent_merges x);
       ("Shallow objects", speed, T.test_shallow_objects x);
       ("Closure with disconnected commits", speed, T.test_closure x);
       ("Prehash collisions", speed, T.test_pre_hash_collisions x);
     ]
    @ when_ x.import_supported
        [
          ("Basic operations on slices", speed, T.test_slice x);
          ("High-level store synchronisation", speed, T.test_sync x);
        ]
    @ when_ with_tree_enabled
        [ ("with_tree strategies", speed, T.test_with_tree x) ]
    @ List.map (fun (n, test) -> ("Graph." ^ n, speed, test x)) T_graph.tests
    @ List.map (fun (n, test) -> ("Watch." ^ n, speed, test x)) T_watch.tests)
    (speed, x)

let slow_suite (speed, x) =
  let (module S) = Suite.store_generic_key x in
  let module T = Make (S) in
  suite' ~prefix:"SLOW_"
    [
      ("Commit wide node", speed, T.test_commit_wide_node x);
      ("Wide nodes", `Slow, T.test_wide_nodes x);
    ]
    (speed, x)

let run name ?(slow = false) ?random_seed ~sleep ~misc tl =
  let () =
    match random_seed with
    | Some x -> Random.init x
    | None -> Random.self_init ()
  in
  Printexc.record_backtrace true;
  (* Ensure that failures occuring in async lwt threads are raised. *)
  let tl1 = List.map (suite sleep) tl in
  let tl1 = if slow then tl1 @ List.map slow_suite tl else tl1 in
  Alcotest.run ~bail:true name (misc @ tl1)
