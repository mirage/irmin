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

open Lwt
open Test_common
open Irmin_unix

let random_string n =
  let t  = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs;
  Cstruct.to_string (Nocrypto.Rng.generate n)

let long_random_string =
  random_string 1024_000

module Make (S: Irmin.S) = struct

  module Common = Make(S)
  open Common

  module Contents = S.Private.Contents
  module Graph = Irmin.Node.Graph(Contents)(S.Private.Node)
  module History = Irmin.Commit.History(Graph.Store)(S.Private.Commit)

  let v t a = S.Private.contents_t (t a)
  let n t a = S.Private.node_t (t a)
  let ct t a = S.Private.commit_t (t a)
  let g t a = let t = t a in S.Private.contents_t t, S.Private.node_t t
  let h t a =
    let t = t a in
    (S.Private.contents_t t, S.Private.node_t t), S.Private.commit_t t

  module Tag = S.Private.Tag

  let l = S.Key.Step.of_hum

  let create x = S.create x.config task

  let create_dummy x =
    let task () =
      Irmin.Task.create ~date:0L ~owner:"test" "Very useful tracking information"
    in
    S.create x.config task

  let string x str = match x.kind with
    | `String -> Tc.read_string (module V) str
    | `Json -> V.of_json (
        (`O [ "foo", Ezjsonm.encode_string str ])
      )
  let v1 x = string x long_random_string

  let v2 x = match x.kind with
    | `String -> Tc.read_string (module V) ""
    | `Json -> V.of_json (`A[])

  let kv1 x =
    create x >>= fun t ->
    Contents.add (S.Private.contents_t (t "contents_t")) (v1 x)

  let kv2 x =
    create x >>= fun t ->
    Contents.add (S.Private.contents_t (t "contents_t")) (v2 x)

  let t1 = T.of_hum "foo"
  let t2 = T.of_hum "bar"

  let n1 x =
    create x >>= fun t ->
    kv1 x >>= fun kv1 ->
    Graph.node (g t "n1") [l "x", `Contents kv1]

  let n2 x =
    n1 x >>= fun kn1 ->
    create x >>= fun t ->
    Graph.node (g t "n2") [l "b", `Node kn1]

  let n3 x =
    n2 x >>= fun kn2 ->
    create x >>= fun t ->
    Graph.node (g t "n3") [l "a", `Node kn2]

  let n4 x =
    n1 x >>= fun kn1 ->
    create x >>= fun t ->
    kv2 x >>= fun kv2 ->
    Graph.node (g t "n4") [l "x", `Contents kv2] >>= fun kn4 ->
    Graph.node (g t "n5") [l "b", `Node kn1; l "c", `Node kn4] >>= fun kn5 ->
    Graph.node (g t "n6") [l "a", `Node kn5]

  let r1 x =
    n2 x >>= fun kn2 ->
    create_dummy x >>= fun t ->
    History.commit (h t ()) ~node:kn2 ~parents:[]

  let r2 x =
    n3 x >>= fun kn3 ->
    r1 x >>= fun kr1 ->
    create_dummy x >>= fun t ->
    History.commit (h t ()) ~node:kn3 ~parents:[kr1]

  let run x test =
    try Lwt_unix.run (x.init () >>= test >>= x.clean)
    with e ->
      Lwt_unix.run (x.clean ());
      raise e

  let random_value x value =
    string x (random_string value)

  let random_path ~label ~path =
    let short () = random_string label in
    let rec aux = function
      | 0 -> []
      | n -> S.Key.Step.of_hum (short ()) :: aux (n-1) in
    aux path

  let random_node x ~label ~path ~value =
    random_path ~label ~path, random_value x value

  let random_nodes x ?(label=8) ?(path=5) ?(value=1024) n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (random_node x ~label ~path ~value :: acc) (n-1) in
    aux [] n

  let test_contents x () =
    let test () =
      create x >>= fun t ->
      let v1 = v1 x in
      let v2 = v2 x in
      kv1 x >>= fun kv1 ->
      kv2 x >>= fun kv2 ->
      let t = S.Private.contents_t (t "get contents handle") in
      Contents.add t v1 >>= fun k1' ->
      assert_equal (module KV) "kv1" kv1 k1';
      Contents.add t v1 >>= fun k1'' ->
      assert_equal (module KV) "kv1" kv1 k1'';
      Contents.add t v2 >>= fun k2' ->
      assert_equal (module KV) "kv2" kv2 k2';
      Contents.add t v2 >>= fun k2'' ->
      assert_equal (module KV) "kv2" kv2 k2'';
      Contents.read t kv1 >>= fun v1' ->
      assert_equal (module Tc.Option(V)) "v1" (Some v1) v1';
      Contents.read t kv2 >>= fun v2' ->
      assert_equal (module Tc.Option(V)) "v2" (Some v2) v2';
      return_unit
    in
    run x test

  let test_nodes x () =
    let test () =
      create x >>= fun t ->
      let g = g t and n = n t in
      kv1 x >>= fun kv1 ->

      (* Create a node containing t1 -x-> (v1) *)
      Graph.node (g "k1")  [l "x", `Contents kv1] >>= fun k1 ->
      Graph.node (g "k1'") [l "x", `Contents kv1] >>= fun k1' ->
      assert_equal (module KN) "k1.1" k1 k1';
      Node.read_exn (n "t1") k1 >>= fun t1 ->
      Node.add (n "k1''") t1 >>= fun k1''->
      assert_equal (module KN) "k1.2" k1 k1'';

      (* Create the node  t2 -b-> t1 -x-> (v1) *)
      Graph.node (g "k2")  [l "b", `Node k1] >>= fun k2 ->
      Graph.node (g "k2'") [l "b", `Node k1] >>= fun k2' ->
      assert_equal (module KN) "k2.1" k2 k2';
      Node.read_exn (n "t2") k2 >>= fun t2 ->
      Node.add (n "k2''") t2 >>= fun k2''->
      assert_equal (module KN) "k2.2" k2 k2'';
      Graph.read_node_exn (g "k1'''") k2 [l "b"] >>= fun k1''' ->
      assert_equal (module KN) "k1.3" k1 k1''';

      (* Create the node t3 -a-> t2 -b-> t1 -x-> (v1) *)
      Graph.node (g "k3")  [l "a", `Node k2] >>= fun k3 ->
      Graph.node (g "k3'") [l "a", `Node k2] >>= fun k3' ->
      assert_equal (module KN) "k3.1" k3 k3';
      Node.read_exn (n "t3") k3 >>= fun t3 ->
      Node.add (n "k3''") t3 >>= fun k3''->
      assert_equal (module KN) "k3.2" k3 k3'';
      Graph.read_node_exn (g "t2'") k3 [l "a"] >>= fun k2'' ->
      assert_equal (module KN) "k2.3" k2 k2'';
      Graph.read_node_exn (g "t1'") k2' [l "b"] >>= fun k1'''' ->
      assert_equal (module KN) "t1.2" k1 k1'''';
      Graph.read_node (g "t1'") k3 [l "a";l "b"] >>= fun k1'''''->
      assert_equal (module Tc.Option(KN)) "t1.3" (Some k1) k1''''';

      Graph.read_contents (g "read_contents k1:/x") k1 [l "x"] >>= fun kv11 ->
      assert_equal (module Tc.Option(KV)) "v1.1" (Some kv1) kv11;
      Graph.read_contents (g "read_contents k2:/b/x") k2 [l "b"; l "x"] >>= fun kv12 ->
      assert_equal (module Tc.Option(KV)) "v1.2" (Some kv1) kv12;
      Graph.read_contents (g "read_contents k3:a/b/x") k3 [l "a"; l "b"; l "x"] >>= fun kv13 ->
      assert_equal (module Tc.Option(KV)) "v1" (Some kv1) kv13;

      (* Create the node t6 -a-> t5 -b-> t1 -x-> (v1)
                                   \-c-> t4 -x-> (v2) *)
      kv2 x >>= fun kv2 ->
      Graph.node (g "k4") [l "x", `Contents kv2] >>= fun k4 ->
      Graph.node (g "k5") [l "b", `Node k1; l "c", `Node k4] >>= fun k5 ->
      Graph.node (g "k6") [l "a", `Node k5] >>= fun k6 ->
      Graph.add_contents (g "k6") k3 [l "a"; l "c";l "x"] kv2 >>= fun k6' ->
      Node.read_exn (n "") k6' >>= fun n6' ->
      Node.read_exn (n "") k6  >>= fun n6 ->
      assert_equal (module N) "node n6" n6 n6';
      assert_equal (module KN) "node k6" k6 k6';

      return_unit
    in
    run x test

  let test_commits x () =
    let test () =

      let task date =
        let i = Int64.of_int date in
        Irmin.Task.create ~date:i ~owner:"test" "Test commit" ~uid:i
      in
      S.create x.config task >>= fun t ->

      kv1 x >>= fun kv1 ->
      let g = g t and h = h t and c x = S.Private.commit_t (t x) in

      (* t3 -a-> t2 -b-> t1 -x-> (v1) *)
      Graph.node (g 0) [l "x", `Contents kv1] >>= fun kt1 ->
      Graph.node (g 1) [l "a", `Node kt1] >>= fun kt2 ->
      Graph.node (g 2) [l "b", `Node kt2] >>= fun kt3 ->

      (* r1 : t2 *)
      History.commit (h 3) ~node:kt2 ~parents:[] >>= fun kr1 ->
      History.commit (h 3) ~node:kt2 ~parents:[] >>= fun kr1' ->
      Commit.read_exn (c 0) kr1  >>= fun t1 ->
      Commit.read_exn (c 0) kr1' >>= fun t1' ->
      assert_equal (module C) "t1" t1 t1';
      assert_equal (module KC) "kr1" kr1 kr1';

      (* r1 -> r2 : t3 *)
      History.commit (h 4) ~node:kt3 ~parents:[kr1] >>= fun kr2 ->
      History.commit (h 4) ~node:kt3 ~parents:[kr1] >>= fun kr2' ->
      assert_equal (module KC) "kr2" kr2 kr2';

      History.closure (h 5) ~min:[] ~max:[kr1] >>= fun kr1s ->
      assert_equal (module Set(KC)) "g1" [kr1] kr1s;

      History.closure (h 6) ~min:[] ~max:[kr2] >>= fun kr2s ->
      assert_equal (module Set(KC)) "g2" [kr1; kr2] kr2s;

      return_unit
    in
    run x test

  let test_tags x () =
    let test () =
      create x >>= fun t ->

      let tag = S.Private.tag_t (t "tag handle") in

      r1 x >>= fun kv1 ->
      r2 x >>= fun kv2 ->

      line "pre-update";
      Tag.update tag t1 kv1 >>= fun () ->
      line "post-update";
      Tag.read   tag t1 >>= fun k1' ->
      assert_equal (module Tc.Option(KC)) "r1" (Some kv1) k1';
      Tag.update tag t2 kv2 >>= fun () ->
      Tag.read   tag t2 >>= fun k2' ->
      assert_equal (module Tc.Option(KC)) "r2" (Some kv2) k2';
      Tag.update tag t1 kv2 >>= fun () ->
      Tag.read   tag t1 >>= fun k2'' ->
      assert_equal (module Tc.Option(KC)) "r1-after-update" (Some kv2) k2'';

      let list t =
        let tags = ref [] in
        Tag.iter t (fun t -> tags := t :: !tags; return_unit) >>= fun () ->
        return !tags
      in
      list tag >>= fun ts ->
      assert_equal (module Set(T)) "list" [t1; t2] ts;
      Tag.remove tag t1 >>= fun () ->
      Tag.read   tag t1 >>= fun empty ->
      assert_equal (module Tc.Option(KC)) "empty" None empty;
      list tag >>= fun r2' ->
      assert_equal (module Set(T)) "all-after-remove" [t2] r2';
      return_unit
    in
    run x test

  let test_merges x () =
    let test () =
      create x >>= fun t ->

      kv1 x >>= fun kv1 ->
      kv2 x >>= fun kv2 ->

      (* merge contents *)

      let v = S.Private.contents_t (t "contents_t") in
      Contents.merge v ~old:kv1 kv1 kv1 >>= fun kv1' ->
      assert_equal (module RV) "merge kv1" (`Ok kv1) kv1';
      Contents.merge v ~old:kv1 kv1 kv2 >>= fun kv2' ->
      assert_equal (module RV) "merge kv2" (`Ok kv2) kv2';

      (* merge nodes *)

      let g = g t in

      (* The empty node *)
      Graph.node (g "k0") [] >>= fun k0 ->

      (* Create the node t1 -x-> (v1) *)
      Graph.node (g "k1") [l "x", `Contents kv1] >>= fun k1 ->

      (* Create the node t2 -b-> t1 -x-> (v1) *)
      Graph.node (g "k2") [l "b", `Node k1] >>= fun k2 ->

      (* Create the node t3 -c-> t1 -x-> (v1) *)
      Graph.node (g "k3") [l "c", `Node k1] >>= fun k3 ->

      (* Should create the node:
                          t4 -b-> t1 -x-> (v1)
                             \c/ *)
      Graph.merge (g "merge: k4") ~old:k0 k2 k3 >>= fun k4 ->
      Irmin.Merge.exn k4 >>= fun k4 ->

      let succ = ref [] in
      Graph.iter_succ (g "iter") k4 (fun l v -> succ := (l, v) :: !succ) >>= fun () ->
      assert_equal (module Succ) "k4"!succ [ (l "b", k1); (l "c", k1) ];

      (* merge commits *)

      let task date =
        let i = Int64.of_int date in
        Irmin.Task.create ~date:i ~uid:i ~owner:"test" "Test commit"
      in
      S.create x.config task >>= fun t ->

      let h = h t and c a = S.Private.commit_t (t a) in

      History.commit (h 0) ~node:k0 ~parents:[] >>= fun kr0 ->
      History.commit (h 1) ~node:k2 ~parents:[kr0] >>= fun kr1 ->
      History.commit (h 2) ~node:k3 ~parents:[kr0] >>= fun kr2 ->
      History.merge (h 3) ~old:kr0 kr1 kr2 >>= fun kr3 ->
      Irmin.Merge.exn kr3 >>= fun kr3 ->
      History.commit (h 3) ~node:k4 ~parents:[kr1; kr2] >>= fun kr3' ->

      Commit.read_exn (c 0) kr3 >>= fun r3 ->
      Commit.read_exn (c 0) kr3' >>= fun r3' ->
      assert_equal (module C) "r3" r3 r3';
      assert_equal (module KC) "kr3" kr3 kr3';

      return_unit
    in
    run x test

  module Snapshot = Irmin.Snapshot(S)

  let test_stores x () =
    let test () =
      create x >>= fun t ->
      let v1 = v1 x in
      S.update (t "update") [l "a";l "b"] v1 >>= fun () ->

      S.mem (t "mem1") [l "a";l "b"] >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem1" true b1;
      S.mem (t "mem2") [l "a"] >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem2" false b2;
      S.read_exn (t "read1") [l "a";l "b"] >>= fun v1' ->
      assert_equal (module V) "v1.1" v1 v1';

      Snapshot.create (t "snapshot") >>= fun r1 ->

      let v2 = v2 x in
      S.update (t "update") [l "a";l "c"] v2 >>= fun () ->
      S.mem (t "mem3") [l "a";l "b"] >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem3" true b1;
      S.mem (t "mem4") [l "a"] >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem4" false b2;
      S.read_exn (t "read2") [l "a";l "b"] >>= fun v1' ->
      assert_equal (module V) "v1.1" v1 v1';
      S.mem (t "mem5") [l "a";l "c"] >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem5" true b1;
      S.read_exn (t "read3") [l "a";l "c"] >>= fun v2' ->
      assert_equal (module V) "v1.1" v2 v2';

      S.remove (t "remove") [l "a";l "b"] >>= fun () ->
      S.read (t "read4") [l "a";l "b"] >>= fun v1''->
      assert_equal (module Tc.Option(V)) "v1.2" None v1'';
      Snapshot.revert (t "revert") r1 >>= fun () ->
      S.read (t "read") [l "a";l "b"] >>= fun v1''->
      assert_equal (module Tc.Option(V)) "v1.3" (Some v1) v1'';
      S.list (t "list") [l "a"] >>= fun ks ->
      assert_equal (module Set(K)) "path" [[l "a";l "b"]] ks;

      S.update (t "update2") [l "a"; l long_random_string] v1 >>= fun () ->

      S.remove_rec (t "remove rec") [l "a"] >>= fun () ->
      S.list (t "list") [] >>= fun dirs ->
      assert_equal (module Set(K)) "remove rec" [] dirs;

      S.update (t "update root") [] v1 >>= fun () ->
      S.read_exn (t "read root") [] >>= fun v1' ->
      assert_equal (module V) "read root" v1 v1';

      S.update (t "update") [l "a"] v1 >>= fun () ->
      S.remove_rec (t "remove rec --all") [] >>= fun () ->
      S.list (t "list") [] >>= fun dirs ->


      assert_equal (module Set(K)) "remove rec root" [] dirs;

      return_unit
    in
    run x test

  module View = Irmin.View(S)

  let test_views x () =
    let test () =
      create x >>= fun t ->
      let nodes = random_nodes x 100 in
      let foo1 = random_value x 10 in
      let foo2 = random_value x 10 in

      View.create x.config task >>= fun v0 ->

      View.update (v0 "/") [] foo1 >>= fun () ->
      View.read (v0 "read /") [] >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "read /" (Some foo1) foo1';

      View.update (v0 "foo/1") [l "foo";l "1"] foo1 >>= fun () ->
      View.read (v0 "read foo/1") [l "foo"; l "1"] >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "read foo/1" (Some foo1) foo1';

      View.update (v0 "foo/2") [l "foo";l "2"] foo2 >>= fun () ->
      View.read (v0 "read foo/2") [l "foo"; l "2"] >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "read foo/2" (Some foo2) foo2';

      let check_view view =
        View.list view [l "foo"] >>= fun ls ->
        assert_equal (module Set(K)) "path1" [ [l "foo";l "1"]; [l "foo";l "2"] ] ls;
        View.read view [l "foo";l "1"] >>= fun foo1' ->
        assert_equal (module Tc.Option(V)) "foo1" (Some foo1) foo1';
        View.read view [l "foo";l "2"] >>= fun foo2' ->
        assert_equal (module Tc.Option(V)) "foo2" (Some foo2) foo2';
        return_unit in

      Lwt_list.iter_s (fun (k,v) ->
          View.update (v0 "init") k v
        ) nodes >>= fun () ->
      check_view (v0 "check v0") >>= fun () ->

      View.update_path (t "update_path b/") [l "b"] (v0 "export") >>= fun () ->
      View.update_path (t "update_path a/") [l "a"] (v0 "export") >>= fun () ->

      S.list (t "list") [l "b";l "foo"] >>= fun ls ->
      assert_equal (module Set(K)) "path2" [ [l "b";l "foo";l "1"]; [l "b";l "foo";l "2"] ] ls;
      S.read (t "read foo1") [l "b";l "foo";l "1"] >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "foo1" (Some foo1) foo1';
      S.read (t "read foo2") [l "a";l "foo";l "2"] >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "foo2" (Some foo2) foo2';

      View.of_path (t "of_path") [l "b"] >>= fun v1 ->
      check_view v1 >>= fun () ->

      S.update (t "update b/x") [l "b";l "x"] foo1 >>= fun () ->
      View.update v1 [l "y"] foo2 >>= fun () ->
      View.merge_path_exn (t "merge_path") [l "b"] v1 >>= fun () ->
      S.read (t "read b/x") [l "b";l "x"] >>= fun foo1' ->
      S.read (t "read b/y") [l"b";l "y"] >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "merge: b/x" (Some foo1) foo1';
      assert_equal (module Tc.Option(V)) "merge: b/y" (Some foo2) foo2';

      Lwt_list.iteri_s (fun i (k, v) ->
          let p = String.concat "/" (List.map S.Key.Step.to_hum k) in
          S.read_exn (t @@ "read a/"^p) (l "a"::k) >>= fun v' ->
          assert_equal (module V) ("a"^string_of_int i) v v';
          S.read_exn (t @@ "read b/"^p) (l "b"::k) >>= fun v' ->
          assert_equal (module V) ("b"^string_of_int i) v v';
          return_unit
        ) nodes >>= fun () ->

      return_unit
    in
    run x test

  module Sync = Irmin.Sync(S)

  let test_sync x () =
    let test () =
      create x >>= fun t1 ->
      let v1 = v1 x in
      let v2 = v2 x in

      S.update (t1 "update a/b") [l "a";l "b"] v1 >>= fun () ->
      Snapshot.create (t1 "snapshot 1") >>= fun _r1 ->
      S.update (t1 "update a/c") [l "a";l "c"] v2 >>= fun () ->
      Snapshot.create (t1 "snapshot 2") >>= fun r2 ->
      S.update (t1 "update a/d") [l "a";l "d"] v1 >>= fun () ->
      Snapshot.create (t1 "snapshot 3") >>= fun _r3 ->

      let remote = Sync.store (module S) (t1 "remote") in

      Sync.fetch_exn (t1 "partial fetch") ~depth:0 remote >>= fun partial ->
      Sync.fetch_exn (t1 "total fetch") remote >>= fun full ->

      (* Restart a fresh store and import everything in there. *)
      let tag = S.Tag.of_hum "export" in
      S.of_tag x.config task tag >>= fun t2 ->
      S.update_head (t2 "partial update") partial >>= fun () ->

      S.mem (t2 "mem a/b") [l "a";l "b"] >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem-ab" true b1;

      S.mem (t2 "mem a/c") [l "a";l "c"] >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem-ac" true b2;

      S.mem (t2 "mem a/d") [l "a";l "d"] >>= fun b3 ->
      assert_equal (module Tc.Bool) "mem-ad" true b3;
      S.read_exn (t2 "read a/d") [l "a";l "d"] >>= fun v1' ->
      assert_equal (module V) "v1" v1' v1;

      Snapshot.revert (t2 "revert to t2") r2 >>= fun () ->
      S.mem (t2 "mem a/b") [l "a";l "d"] >>= fun b4 ->
      assert_equal (module Tc.Bool) "mem-ab" false b4;

      S.update_head (t2 "full update") full >>= fun () ->
      Snapshot.revert (t2 "revert to r2") r2 >>= fun () ->
      S.mem (t2 "mem a/d") [l "a";l "d"] >>= fun b4 ->
      assert_equal (module Tc.Bool) "mem-ad" false b4;
      return_unit
    in
    run x test

  module Dot = Irmin.Dot(S)

  let output_file t file =
    let buf = Buffer.create 1024 in
    let date d =
      let tm = Unix.localtime (Int64.to_float d) in
      Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Dot.output_buffer t ~date buf >>= fun () ->
    let oc = open_out_bin (file ^ ".dot") in
    output_string oc (Buffer.contents buf);
    close_out oc;
    return_unit

  let test_merge_api x () =
    let test () =
      let v1 = string x "X1" in
      let v2 = string x "X2" in
      let v3 = string x "X3" in

      create x >>= fun t1 ->

      S.update (t1 "update a/b/a") [l "a";l "b";l "a"] v1 >>= fun () ->
      S.update (t1 "update a/b/b") [l "a";l "b";l "b"] v2 >>= fun () ->
      S.update (t1 "update a/b/c") [l "a";l "b";l "c"] v3 >>= fun () ->

      let test = S.Tag.of_hum "test" in

      S.clone_force (t1 "clone master into test") task test >>= fun t2 ->

      S.update (t1 "update master:a/b/b") [l "a";l "b";l "b"] v1 >>= fun () ->
      S.update (t1 "update master:a/b/b") [l "a";l "b";l "b"] v3 >>= fun () ->
      S.update (t2 "update test:a/b/c")   [l "a";l "b";l "c"] v1 >>= fun () ->

      output_file (t1 "before.dot") "before" >>= fun () ->
      S.merge_exn (t1 "merge test into master") test >>= fun () ->
      output_file (t1 "after.dot") "after" >>= fun () ->

      S.read_exn (t1 "read master:a/b/c") [l "a";l "b";l "c"] >>= fun v1' ->
      S.read_exn (t2 "read test:a/b/c")   [l "a";l "b";l "b"] >>= fun v2' ->
      S.read_exn (t1 "read master:a/b/b") [l "a";l "b";l "b"] >>= fun v3' ->

      assert_equal (module V) "v1" v1 v1';
      assert_equal (module V) "v2" v2 v2';
      assert_equal (module V) "v3" v3 v3';

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
    "Basic operations on tags"        , speed, T.test_tags x;
    "Basic merge operations"          , speed, T.test_merges     x;
    "High-level store operations"     , speed, T.test_stores     x;
    "High-level operations in views"  , speed, T.test_views      x;
    "High-level store synchronisation", speed, T.test_sync       x;
    "High-level store merges"         , speed, T.test_merge_api  x;
  ]

let run name tl =
  let tl = List.map suite tl in
  Alcotest.run name tl
