(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open OUnit
open Test_common
open Lwt
open Core_kernel.Std

let urandom = Cryptokit.Random.device_rng "/dev/urandom"
let long_random_string = Cryptokit.Random.string urandom 1024

module Make (S: Irmin.S) = struct

  module Common = Make(S)
  open Common
  open S

  let run x test =
    try Lwt_unix.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_unix.run (x.clean ());
      raise e

  type e = {
    v1: B.t;
    v2: B.t;
    kv1: K.t Lwt.t Lazy.t;
    kv2: K.t Lwt.t Lazy.t;
    r1: R.t;
    r2: R.t;
  }

  let random_value ~kind ~value =
    let str = Cryptokit.(Random.string urandom value) in
    match kind with
    | `String -> B.of_string str
    | `JSON   -> B.of_string (Ezjsonm.to_string (`A [ IrminMisc.json_encode str ]))

  let random_path ~label ~path =
    let short () = Cryptokit.(Random.string urandom label) in
    let rec aux = function
      | 0 -> []
      | n -> short () :: aux (n-1) in
    aux path

  let random_node ~label ~path ~value ~kind =
    random_path ~label ~path, random_value ~kind ~value

  let random_nodes ?(label=8) ?(path=5) ?(value=1024) kind n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (random_node ~label ~path ~value ~kind :: acc) (n-1) in
    aux [] n

  let mk k t =
    let v1 = match k with
      | `String -> B.of_string long_random_string
      | `JSON   -> B.of_string (
          Ezjsonm.to_string (`O [ "foo", IrminMisc.json_encode long_random_string ])
        ) in
    let v2 = match k with
      | `String -> B.of_string ""
      | `JSON   -> B.of_string (Ezjsonm.to_string (`A[])) in
    let kv1 = lazy (S.Internal.add (S.internal t) (IrminValue.Contents v1)) in
    let kv2 = lazy (S.Internal.add (S.internal t) (IrminValue.Contents v2)) in
    let r1 = R.of_string "refs/foo" in
    let r2 = R.of_string "refs/bar" in
    return { v1; v2; kv1; kv2; r1; r2 }

  let test_contents x () =
    let test () =
      create ()   >>= fun t                         ->
      mk x.kind t >>= function { v1; v2; kv1; kv2 } ->

      Lazy.force kv1 >>= fun kv1 ->
      Lazy.force kv2 >>= fun kv2 ->

      let v = contents t in
      Contents.add v v1                >>= fun k1'  ->
      assert_key_equal "kv1" kv1 k1';
      Contents.add v v1                >>= fun k1'' ->
      assert_key_equal "kv1" kv1 k1'';
      Contents.add v v2                >>= fun k2'  ->
      assert_key_equal "kv2" kv2 k2';
      Contents.add v v2                >>= fun k2'' ->
      assert_key_equal "kv2" kv2 k2'';
      Contents.read v kv1              >>= fun v1'  ->
      assert_contents_opt_equal "v1" (Some v1) v1';
      Contents.read v kv2              >>= fun v2'  ->
      assert_contents_opt_equal "v2" (Some v2) v2';
      return_unit
    in
    run x test

  let test_nodes x () =
    let test () =
      create () >>= fun t          ->
      mk x.kind t >>= function { v1; v2 } ->
      let node = node t in

      (* Create a node containing t1(v1) *)
      Node.node node ~contents:v1 () >>= fun (k1 , _) ->
      Node.node node ~contents:v1 () >>= fun (k1', _) ->
      assert_key_equal "k1.1" k1 k1';
      Node.read_exn node k1       >>= fun t1  ->
      Node.add node t1            >>= fun k1''->
      assert_key_equal "k1.2" k1 k1'';

      (* Create the node  t2 -b-> t1(v1) *)
      Node.node node ~succ:["b", t1] () >>= fun (k2 , _) ->
      Node.node node ~succ:["b", t1] () >>= fun (k2', _) ->
      assert_key_equal "k2.1" k2 k2';
      Node.read_exn node k2             >>= fun t2  ->
      Node.add node t2                  >>= fun k2''->
      assert_key_equal "k2.2" k2 k2'';
      Node.sub_exn node t2 ["b"]        >>= fun t1' ->
      assert_node_equal "t1.1" t1 t1';
      Node.add node t1'                 >>= fun k1''->
      assert_key_equal "k1.3" k1 k1'';

      (* Create the node t3 -a-> t2 -b-> t1(v1) *)
      Node.node node ~succ:["a", t2] () >>= fun (k3 , _) ->
      Node.node node ~succ:["a", t2] () >>= fun (k3', _) ->
      assert_key_equal "k3.1" k3 k3';
      Node.read_exn node k3             >>= fun t3  ->
      Node.add node t3                  >>= fun k3''->
      assert_key_equal "k3.2" k3 k3'';
      Node.sub_exn node t3 ["a"]        >>= fun t2' ->
      assert_node_equal "t2.1" t2 t2';
      Node.add node t2'                 >>= fun k2''->
      assert_key_equal "k2.3" k2 k2'';
      Node.sub_exn node t2' ["b"]       >>= fun t1' ->
      assert_node_equal "t1.2" t1 t1';
      Node.sub node t3 ["a";"b"]        >>= fun t1' ->
      assert_node_opt_equal "t1.3" (Some t1) t1';

      Node.find node t1 []              >>= fun v11 ->
      assert_contents_opt_equal "v1.1" (Some v1) v11;
      Node.find node t2 ["b"]           >>= fun v12 ->
      assert_contents_opt_equal "v1.2" (Some v1) v12;
      Node.find node t3 ["a";"b"]       >>= fun v13 ->
      assert_contents_opt_equal "v1" (Some v1) v13;

      (* Create the node t6 -a-> t5 -b-> t1(v1)
                                   \-c-> t4(v2) *)
      Node.node node ~contents:v2 ()               >>= fun (k4, _) ->
      Node.read_exn node k4                        >>= fun t4  ->
      Node.node node ~succ:[("b",t1); ("c",t4)] () >>= fun (k5, _) ->
      Node.read_exn node k5                        >>= fun t5  ->
      Node.node node ~succ:["a",t5] ()             >>= fun (k6, _) ->
      Node.read_exn node k6                        >>= fun t6  ->
      Node.update node t3 ["a";"c"] v2             >>= fun t6' ->
      assert_node_equal "node" t6 t6';

      return_unit
    in
    run x test

  let test_commits x () =
    let test () =
      create () >>= fun t      ->
      mk x.kind t >>= function { v1 } ->

      let node = node t in
      let commit = commit t in

      (* t3 -a-> t2 -b-> t1(v1) *)
      Node.node node ~contents:v1 ()    >>= fun (k1, _) ->
      Node.read_exn node k1             >>= fun t1 ->
      Node.node node ~succ:["a", t1] () >>= fun (k2, _) ->
      Node.read_exn node k2             >>= fun t2 ->
      Node.node node ~succ:["b", t2] () >>= fun (k3, _) ->
      Node.read_exn node k3             >>= fun t3 ->

      (* r1 : t2 *)
      Commit.commit commit ~date:0. ~origin:"test" ~node:t2 ~parents:[] >>= fun (kr1 , r1 ) ->
      Commit.commit commit ~date:0. ~origin:"test" ~node:t2 ~parents:[] >>= fun (kr1', r1') ->
      assert_key_equal "kr1" kr1 kr1';
      assert_commit_equal "r1" r1 r1';

      (* r1 -> r2 : t3 *)
      Commit.commit commit ~date:0. ~origin:"test" ~node:t3 ~parents:[r1]
      >>= fun (kr2 , r2) ->
      Commit.commit commit ~date:0. ~origin:"test" ~node:t3 ~parents:[r1]
      >>= fun (kr2', r2') ->
      assert_key_equal "kr2" kr2 kr2';
      assert_commit_equal "r2" r2 r2';

      Commit.list commit [kr1] >>= fun kr1s ->
      assert_keys_equal "g1" [kr1] kr1s;

      Commit.list commit [kr2] >>= fun kr2s ->
      assert_keys_equal "g2" [kr1; kr2] kr2s;

     return_unit
    in
    run x test

  let test_references x () =
    let test () =
      create () >>= fun t                    ->
      mk x.kind t >>= function { kv1; kv2; r1; r2 } ->

      Lazy.force kv1 >>= fun kv1 ->
      Lazy.force kv2 >>= fun kv2 ->

      let reference = reference t in
      Reference.update reference r1 kv1 >>= fun ()  ->
      Reference.read   reference r1     >>= fun k1' ->
      assert_key_opt_equal "r1" (Some kv1) k1';
      Reference.update reference r2 kv2 >>= fun ()  ->
      Reference.read   reference r2     >>= fun k2' ->
      assert_key_opt_equal "r2" (Some kv2) k2';
      Reference.update reference r1 kv2 >>= fun ()   ->
      Reference.read   reference r1     >>= fun k2'' ->
      assert_key_opt_equal "r1-after-update" (Some kv2) k2'';
      Reference.list reference [r1]     >>= fun ts ->
      assert_references_equal "list" [r1; r2] ts;
      Reference.remove reference r1     >>= fun () ->
      Reference.read   reference r1     >>= fun empty ->
      assert_key_opt_equal "empty" None empty;
      Reference.list reference [r1]     >>= fun r2' ->
      assert_references_equal "all-after-remove" [r2] r2';
      return_unit
    in
    run x test

  let test_merges x () =
    let test () =
      create () >>= fun t                    ->
      mk x.kind t >>= function { v1; v2; kv1; kv2 } ->

      Lazy.force kv1 >>= fun kv1 ->
      Lazy.force kv2 >>= fun kv2 ->

      (* merge contents *)

      let v = contents t in
      IrminMerge.merge (Contents.merge v) ~old:kv1 kv1 kv1 >>= fun kv1'  ->
      assert_key_equal "merge kv1" kv1 kv1';
      IrminMerge.merge (Contents.merge v) ~old:kv1 kv1 kv2 >>= fun kv2'  ->
      assert_key_equal "merge kv2" kv2 kv2';

      (* merge nodes *)

      let node = node t in
      (* The empty node *)
      Node.node node ()                 >>= fun (k0, t0) ->
      (* Create a node containing t1(v1) *)
      Node.node node ~contents:v1 ()    >>= fun (k1, t1) ->
      (* Create the node  t2 -b-> t1(v1) *)
      Node.node node ~succ:["b", t1] () >>= fun (k2, t2) ->
      (* Create the node  t3 -c-> t1(v1) *)
      Node.node node ~succ:["c", t1] () >>= fun (k3, t3) ->
      (* Should create the node:
                          t4 -b-> t1(v1)
                             \c/  *)

      IrminMerge.merge (Node.merge node) ~old:k0 k2 k3 >>= fun k4 ->
      Node.read_exn node k4 >>= fun t4 ->
      let succ = Map.to_alist (Node.succ node t4) in
      Lwt_list.map_p (fun (l, v) -> v >>= fun v -> return (l, v)) succ
      >>= fun succ ->
      assert_succ_equal "k4" succ [ ("b", t1); ("c", t1) ];

      (* merge commits *)

      let commit = commit t in
      Commit.commit commit ~date:0. ~origin:"test" ~node:t0 ~parents:[] >>= fun (kr0, r0) ->
      Commit.commit commit ~date:1. ~origin:"test" ~node:t2 ~parents:[r0]
      >>= fun (kr1, r1) ->
      Commit.commit commit ~date:2. ~origin:"test" ~node:t3 ~parents:[r0]
      >>= fun (kr2, r2) ->
      IrminMerge.merge
        (Commit.merge commit ~date:3. ~origin:"test") ~old:kr0 kr1 kr2
      >>= fun kr3 ->
      Commit.read_exn commit kr3 >>= fun r3 ->
      Commit.commit commit ~date:3. ~origin:"test" ~node:t4 ~parents:[r1; r2]
      >>= fun (kr3', r3') ->
      assert_key_equal "kr3" kr3 kr3';
      assert_commit_equal "r3" r3 r3';

      return_unit
    in
    run x test

  let test_stores x () =
    let test () =
      create () >>= fun t          ->
      mk x.kind t >>= function { v1; v2 } ->
      update t ["a";"b"] v1 >>= fun ()  ->

      mem t ["a";"b"]       >>= fun b1  ->
      assert_bool_equal "mem1" true b1;
      mem t ["a"]           >>= fun b2  ->
      assert_bool_equal "mem2" false b2;
      read_exn t ["a";"b"]  >>= fun v1' ->
      assert_contents_equal "v1.1" v1 v1';
      snapshot t            >>= fun r1  ->

      update t ["a";"c"] v2 >>= fun ()  ->
      mem t ["a";"b"]       >>= fun b1  ->
      assert_bool_equal "mem3" true b1;
      mem t ["a"]           >>= fun b2  ->
      assert_bool_equal "mem4" false b2;
      read_exn t ["a";"b"]  >>= fun v1' ->
      assert_contents_equal "v1.1" v1 v1';
      mem t ["a";"c"]       >>= fun b1  ->
      assert_bool_equal "mem5" true b1;
      read_exn t ["a";"c"]  >>= fun v2' ->
      assert_contents_equal "v1.1" v2 v2';

      remove t ["a";"b"]    >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_contents_opt_equal "v1.2" None v1'';
      revert t r1           >>= fun ()  ->
      read t ["a";"b"]      >>= fun v1''->
      assert_contents_opt_equal "v1.3" (Some v1) v1'';
      list t [["a"]]        >>= fun ks  ->
      assert_paths_equal "path" [["a";"b"]] ks;
      update t [long_random_string] v1 >>= fun () ->
      return_unit
    in
    run x test

  let test_views x () =
    let test () =
      create () >>= fun t ->
      let nodes = random_nodes x.kind 100 in

      View.create () >>= fun view ->
      Lwt_list.iter_s (fun (k,v) -> View.update view k v) nodes >>= fun () ->

      updates t ["b"] view >>= fun () ->
      updates t ["a"] view  >>= fun () ->

      Lwt_list.iteri_s (fun i (k, v) ->
          read_exn t ("a"::k) >>= fun v' ->
          assert_contents_equal ("a"^string_of_int i) v v';
          read_exn t ("b"::k) >>= fun v' ->
          assert_contents_equal ("b"^string_of_int i) v v';
          return_unit
        ) nodes >>= fun () ->

      return_unit
    in
    run x test

  let test_sync x () =
    let test () =
      create () >>= fun t1          ->
      mk x.kind t1 >>= function { v1; v2 } ->

      update t1 ["a";"b"] v1 >>= fun () ->
      snapshot t1            >>= fun r1 ->
      update t1 ["a";"c"] v2 >>= fun () ->
      snapshot t1            >>= fun r2 ->
      update t1 ["a";"d"] v1 >>= fun () ->
      snapshot t1            >>= fun r3 ->

      export t1 [r3]         >>= fun partial ->
      export t1 []           >>= fun full    ->

      (* Restart a fresh store and import everything in there. *)
      x.clean ()             >>= fun () ->
      x.init ()              >>= fun () ->
      create ()              >>= fun t2 ->

      let branch = R.of_string "import" in
      import t2 branch partial >>= fun () ->
      revert t2 r3             >>= fun () ->

      mem t2 ["a";"b"]       >>= fun b1 ->
      assert_bool_equal "mem-ab" true b1;

      mem t2 ["a";"c"]       >>= fun b2 ->
      assert_bool_equal "mem-ac" true b2;

      mem t2 ["a";"d"]       >>= fun b3  ->
      assert_bool_equal "mem-ad" true b3;
      read_exn t2 ["a";"d"]  >>= fun v1' ->
      assert_contents_equal "v1" v1' v1;

      catch
        (fun () ->
           revert t2 r2      >>= fun () ->
           OUnit.assert_bool "revert" false;
           return_unit)
        (fun e ->
           import t2 branch full >>= fun () ->
           revert t2 r2          >>= fun () ->
           mem t2 ["a";"d"]      >>= fun b4 ->
           assert_bool_equal "mem-ab" false b4;
           return_unit)
    in
    run x test

  let test_merge_api x () =
    let test () =
      let mk str =
        match x.kind with
        | `String -> B.of_string str
        | `JSON   -> B.of_string (
            Ezjsonm.to_string (`A [ IrminMisc.json_encode str ])
          ) in
      let v1 = mk "X1" in
      let v2 = mk "X2" in
      let v3 = mk "X3" in

      create ()                  >>= fun t1  ->

      update t1 ["a";"b";"a"] v1 >>= fun () ->
      update t1 ["a";"b";"b"] v2 >>= fun () ->
      update t1 ["a";"b";"c"] v3 >>= fun () ->

      let test = R.of_string "refs/heads/test" in

      branch t1 test >>= fun t2 ->

      update t1 ["a";"b";"b"] v1 >>= fun () ->
      update t1 ["a";"b";"b"] v3 >>= fun () ->

      update t2 ["a";"b";"c"] v1 >>= fun () ->

      output t1 "before" >>= fun () ->
      merge t1 ~into:t2 >>= fun () ->
      output t1 "after" >>= fun () ->

      read_exn t1 ["a";"b";"c"] >>= fun v1'  ->
      read_exn t2 ["a";"b";"b"] >>= fun v2'  ->
      read_exn t1 ["a";"b";"b"] >>= fun v3' ->

      assert_contents_equal "v1" v1 v1;
      assert_contents_equal "v2" v2 v2';
      assert_contents_equal "v3" v3 v3';

      return_unit
    in
    run x test

end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Basic operations on contents"    , speed, T.test_contents   x;
    "Basic operations on nodes"       , speed, T.test_nodes      x;
    "Basic operations on commits"     , speed, T.test_commits    x;
    "Basic operations on references"  , speed, T.test_references x;
    "Basic merge operations"          , speed, T.test_merges     x;
    "High-level operations in views"  , speed, T.test_views      x;
    "High-level store operations"     , speed, T.test_stores     x;
    "High-level store synchronisation", speed, T.test_sync       x;
    "High-level store merges"         , speed, T.test_merge_api  x;
  ]

let run name tl =
  let tl = List.map ~f:suite tl in
  Alcotest.run name tl
