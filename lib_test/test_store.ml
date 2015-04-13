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
open Printf

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
  module Graph = Irmin.Private.Node.Graph(Contents)(S.Private.Node)
  module History = Irmin.Private.Commit.History(Graph.Store)(S.Private.Commit)

  let v t a = S.Private.contents_t (t a)
  let n t a = S.Private.node_t (t a)
  let ct t a = S.Private.commit_t (t a)
  let g t a = let t = t a in S.Private.contents_t t, S.Private.node_t t
  let h t a =
    let t = t a in
    (S.Private.contents_t t, S.Private.node_t t), S.Private.commit_t t

  module Tag = S.Private.Tag

  let l = S.Key.Step.of_hum
  let p k = S.Key.create (List.map l k)

  let create x = S.create x.config task

  let dummy_task =
    let t = Irmin.Task.empty in
    fun () -> t

  let create_dummy x =
    S.create x.config dummy_task

  let string x str = match x.cont with
    | `String -> Tc.read_string (module V) str
    | `Json -> V.of_json (
        (`O [ "foo", Ezjsonm.encode_string str ])
      )
  let v1 x = string x long_random_string

  let v2 x = match x.cont with
    | `String -> Tc.read_string (module V) ""
    | `Json -> V.of_json (`A[])

  let kv1 x =
    create x >>= fun t ->
    Contents.add (S.Private.contents_t (t "contents_t")) (v1 x)

  let kv2 x =
    create x >>= fun t ->
    Contents.add (S.Private.contents_t (t "contents_t")) (v2 x)

  let t1 = T.of_hum "foo"
  let t2 = T.of_hum "bar/toto"

  let n1 x =
    create x >>= fun t ->
    kv1 x >>= fun kv1 ->
    Graph.create (g t "n1") [l "x", `Contents kv1]

  let n2 x =
    n1 x >>= fun kn1 ->
    create x >>= fun t ->
    Graph.create (g t "n2") [l "b", `Node kn1]

  let n3 x =
    n2 x >>= fun kn2 ->
    create x >>= fun t ->
    Graph.create (g t "n3") [l "a", `Node kn2]

  let n4 x =
    n1 x >>= fun kn1 ->
    create x >>= fun t ->
    kv2 x >>= fun kv2 ->
    Graph.create (g t "n4") [l "x", `Contents kv2] >>= fun kn4 ->
    Graph.create (g t "n5") [l "b", `Node kn1; l "c", `Node kn4] >>= fun kn5 ->
    Graph.create (g t "n6") [l "a", `Node kn5]

  let r1 x =
    n2 x >>= fun kn2 ->
    create_dummy x >>= fun t ->
    History.create (h t ()) ~node:kn2 ~parents:[]

  let r2 x =
    n3 x >>= fun kn3 ->
    r1 x >>= fun kr1 ->
    create_dummy x >>= fun t ->
    History.create (h t ()) ~node:kn3 ~parents:[kr1]

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
    S.Key.create (aux path)

  let random_node x ~label ~path ~value =
    random_path ~label ~path, random_value x value

  let random_nodes x ?(label=8) ?(path=5) ?(value=1024) n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (random_node x ~label ~path ~value :: acc) (n-1) in
    aux [] n

  let old k () = Lwt.return (`Ok (Some k))

  let test_contents x () =
    let test () =
      create x >>= fun t ->
      let t = S.Private.contents_t (t "get contents handle") in

      let v2 = v2 x in
      kv2 x >>= fun kv2 ->
      Contents.add t v2 >>= fun k2' ->
      assert_equal (module KV) "kv2" kv2 k2';
      Contents.read t k2' >>= fun v2' ->
      assert_equal (module Tc.Option(V)) "v2" (Some v2) v2';

      Contents.add t v2 >>= fun k2'' ->
      assert_equal (module KV) "kv2" kv2 k2'';

      let v1 = v1 x in
      kv1 x >>= fun kv1 ->
      Contents.add t v1 >>= fun k1' ->
      assert_equal (module KV) "kv1" kv1 k1';
      Contents.add t v1 >>= fun k1'' ->
      assert_equal (module KV) "kv1" kv1 k1'';
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
      Graph.create (g "k1")  [l "x", `Contents kv1] >>= fun k1 ->
      Graph.create (g "k1'") [l "x", `Contents kv1] >>= fun k1' ->
      assert_equal (module KN) "k1.1" k1 k1';
      Node.read_exn (n "t1") k1 >>= fun t1 ->
      Node.add (n "k1''") t1 >>= fun k1''->
      assert_equal (module KN) "k1.2" k1 k1'';

      (* Create the node  t2 -b-> t1 -x-> (v1) *)
      Graph.create (g "k2")  [l "b", `Node k1] >>= fun k2 ->
      Graph.create (g "k2'") [l "b", `Node k1] >>= fun k2' ->
      assert_equal (module KN) "k2.1" k2 k2';
      Node.read_exn (n "t2") k2 >>= fun t2 ->
      Node.add (n "k2''") t2 >>= fun k2''->
      assert_equal (module KN) "k2.2" k2 k2'';
      Graph.read_node_exn (g "k1'''") k2 (p ["b"]) >>= fun k1''' ->
      assert_equal (module KN) "k1.3" k1 k1''';

      (* Create the node t3 -a-> t2 -b-> t1 -x-> (v1) *)
      Graph.create (g "k3")  [l "a", `Node k2] >>= fun k3 ->
      Graph.create (g "k3'") [l "a", `Node k2] >>= fun k3' ->
      assert_equal (module KN) "k3.1" k3 k3';
      Node.read_exn (n "t3") k3 >>= fun t3 ->
      Node.add (n "k3''") t3 >>= fun k3''->
      assert_equal (module KN) "k3.2" k3 k3'';
      Graph.read_node_exn (g "t2'") k3 (p ["a"]) >>= fun k2'' ->
      assert_equal (module KN) "k2.3" k2 k2'';
      Graph.read_node_exn (g "t1'") k2' (p ["b"]) >>= fun k1'''' ->
      assert_equal (module KN) "t1.2" k1 k1'''';
      Graph.read_node (g "t1'") k3 (p ["a";"b"]) >>= fun k1'''''->
      assert_equal (module Tc.Option(KN)) "t1.3" (Some k1) k1''''';

      Graph.read_contents (g "read_contents k1:/x") k1 (p ["x"])
      >>= fun kv11 ->
      assert_equal (module Tc.Option(KV)) "v1.1" (Some kv1) kv11;
      Graph.read_contents (g "read_contents k2:/b/x") k2 (p ["b";"x"])
      >>= fun kv12 ->
      assert_equal (module Tc.Option(KV)) "v1.2" (Some kv1) kv12;
      Graph.read_contents (g "read_contents k3:a/b/x") k3 (p ["a";"b";"x"])
      >>= fun kv13 ->
      assert_equal (module Tc.Option(KV)) "v1" (Some kv1) kv13;

      (* Create the node t6 -a-> t5 -b-> t1 -x-> (v1)
                                   \-c-> t4 -x-> (v2) *)
      kv2 x >>= fun kv2 ->
      Graph.create (g "k4") [l "x", `Contents kv2] >>= fun k4 ->
      Graph.create (g "k5") [l "b", `Node k1; l "c", `Node k4] >>= fun k5 ->
      Graph.create (g "k6") [l "a", `Node k5] >>= fun k6 ->
      Graph.add_contents (g "k6") k3 (p ["a";"c";"x"]) kv2 >>= fun k6' ->
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
      Graph.create (g 0) [l "x", `Contents kv1] >>= fun kt1 ->
      Graph.create (g 1) [l "a", `Node kt1] >>= fun kt2 ->
      Graph.create (g 2) [l "b", `Node kt2] >>= fun kt3 ->

      (* r1 : t2 *)
      History.create (h 3) ~node:kt2 ~parents:[] >>= fun kr1 ->
      History.create (h 3) ~node:kt2 ~parents:[] >>= fun kr1' ->
      Commit.read_exn (c 0) kr1  >>= fun t1 ->
      Commit.read_exn (c 0) kr1' >>= fun t1' ->
      assert_equal (module C) "t1" t1 t1';
      assert_equal (module KC) "kr1" kr1 kr1';

      (* r1 -> r2 : t3 *)
      History.create (h 4) ~node:kt3 ~parents:[kr1] >>= fun kr2 ->
      History.create (h 4) ~node:kt3 ~parents:[kr1] >>= fun kr2' ->
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
        Tag.iter t (fun t _ -> tags := t :: !tags; return_unit) >>= fun () ->
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

  let test_watches x () =
    let test () =
      create x >>= fun t1 ->
      create x >>= fun t2 ->

      let adds    = ref 0 in
      let updates = ref 0 in
      let removes = ref 0 in
      let sleep_t = 0.02 in
      let process head =
        Lwt_unix.sleep sleep_t >>= fun () ->
        let () = match head with
          | `Added _h  -> adds    := !adds + 1
          | `Updated _ -> updates := !updates + 1
          | `Removed _ -> removes := !removes + 1
        in
        Lwt.return_unit
      in
      let stops_0 = ref [] in
      let stops_1 = ref [] in
      let rec loop = function
        | 0 -> Lwt.return_unit
        | n ->
          let t = if n mod 2 = 0 then t1 else t2 in
          S.watch_head (t "watch") process >>= fun s ->
          if n mod 2 = 0 then stops_0 := s :: !stops_0
          else stops_1 := s :: !stops_1;
          loop (n-1)
      in
      let sleep () =
        (* sleep duration is 2*max(polling time, callback sleep time) *)
        let sleep_t = 3. *. (max sleep_t Test_fs.polling) in
        Lwt_unix.sleep sleep_t
      in
      let check msg w a b =
        let printer (a, u, r) =
          Printf.sprintf "{ adds=%d; updates=%d; removes=%d }" a u r
        in
        let w = if x.kind <> `Mem || w = 0 then w else 1 in
        let w_msg = sprintf "%s: %d worker(s)" msg w in
        assert_equal Tc.int w_msg w (Irmin.Private.Watch.workers ());
        line msg;
        if a <> b then error msg (printer a) (printer b)
      in
      let state () = !adds, !updates, !removes in
      let v1 = string x "X1" in
      let v2 = string x "X2" in

      S.update (t1 "update") (p ["a";"b"]) v1 >>= fun () ->
      S.remove_tag (t1 "remove-tag") Tag.Key.master >>= fun () ->
      sleep () >>= fun () ->
      check "init" 0 (0, 0, 0) (state ());

      loop 100 >>= fun () ->

      check "watches on" 0 (0, 0, 0) (state ());

      S.update (t1 "update") (p ["a";"b"]) v1 >>= fun () ->
      sleep () >>= fun () ->
      check "adds" 2 (100, 0, 0) (state ());

      S.update (t2 "update") (p ["a";"c"]) v1 >>= fun () ->
      sleep () >>= fun () ->
      check "updates" 2 (100, 100, 0) (state ());

      S.remove_tag (t1 "remove-tag") Tag.Key.master >>= fun () ->
      sleep () >>= fun () ->
      check "removes" 2 (100, 100, 100) (state ());

      Lwt_list.iter_s (fun f -> f ()) !stops_0 >>= fun () ->
      S.update (t2 "update") (p ["a"]) v1 >>= fun () ->
      sleep () >>= fun () ->
      check "watches half off" 1 (150, 100, 100) (state ());

      Lwt_list.iter_s (fun f -> f ()) !stops_1 >>= fun () ->
      S.update (t1 "update") (p ["a"]) v2 >>= fun () ->
      sleep () >>= fun () ->
      check "watches off" 0 (150, 100, 100) (state ());

      Lwt.return_unit
    in
    run x test

  let test_simple_merges x () =

    (* simple merges *)
    let check () =
      let open Irmin.Merge.OP in
      let merge_skip  ~old:_ _ _ = ok None in
      let merge_left ~old:_ x _ = ok x in
      let merge_right ~old:_ _ y = ok y in
      let merge_default = Irmin.Merge.default (module Tc.Option(Tc.Int)) in
      let merge = function
        | "left" -> merge_left
        | "right" -> merge_right
        | "skip" -> merge_skip
        | _ -> merge_default
      in
      let module X = Tc.List(Tc.Pair(Tc.String)(Tc.Int)) in
      let merge_x =
        Irmin.Merge.alist (module Tc.String) (module Tc.Int) merge
      in
      let old () = ok (Some [ "left", 1; "foo", 2; ]) in
      let x =   [ "left", 2; "right", 0] in
      let y =   [ "left", 1; "bar"  , 3; "skip", 0 ] in
      let m =   [ "left", 2; "bar"  , 3] in
      merge_x ~old x y >>= function
      | `Ok m' ->
        assert_equal (module X) "compound merge" m m';
        return_unit
      | `Conflict c ->
        OUnit.assert_bool (sprintf "compound merge: %s" c) false;
        return_unit
    in

    let test () =
      check () >>= fun () ->
      create x >>= fun t ->
      kv1 x >>= fun kv1 ->
      kv2 x >>= fun kv2 ->

      (* merge contents *)

      let v = S.Private.contents_t (t "contents_t") in
      Contents.merge (p []) v ~old:(old (Some kv1)) (Some kv1) (Some kv1)
      >>= fun kv1' ->
      assert_equal (module RV) "merge kv1" (`Ok (Some kv1)) kv1';
      Contents.merge (p []) v ~old:(old (Some kv1)) (Some kv1) (Some kv2)
      >>= fun kv2' ->
      assert_equal (module RV) "merge kv2" (`Ok (Some kv2)) kv2';

      (* merge nodes *)

      let g = g t in

      (* The empty node *)
      Graph.create (g "k0") [] >>= fun k0 ->

      (* Create the node t1 -x-> (v1) *)
      Graph.create (g "k1") [l "x", `Contents kv1] >>= fun k1 ->

      (* Create the node t2 -b-> t1 -x-> (v1) *)
      Graph.create (g "k2") [l "b", `Node k1] >>= fun k2 ->

      (* Create the node t3 -c-> t1 -x-> (v1) *)
      Graph.create (g "k3") [l "c", `Node k1] >>= fun k3 ->

      (* Should create the node:
                          t4 -b-> t1 -x-> (v1)
                             \c/ *)
      Graph.merge (g "merge: k4") ~old:(old (Some k0)) (Some k2) (Some k3) >>= fun k4 ->
      Irmin.Merge.exn k4 >>= fun k4 ->
      let k4 = match k4 with Some k -> k | None -> failwith "k4" in

      let succ = ref [] in
      Graph.iter_succ (g "iter") k4 (fun l v -> succ := (l, v) :: !succ) >>= fun () ->
      assert_equal (module Succ) "k4"[ (l "b", k1); (l "c", k1) ] !succ;

      let task date =
        let i = Int64.of_int date in
        Irmin.Task.create ~date:i ~uid:i ~owner:"test" "Test commit"
      in
      S.create x.config task >>= fun t ->

      let h = h t and c a = S.Private.commit_t (t a) in

      History.create (h 0) ~node:k0 ~parents:[] >>= fun kr0 ->
      History.create (h 1) ~node:k2 ~parents:[kr0] >>= fun kr1 ->
      History.create (h 2) ~node:k3 ~parents:[kr0] >>= fun kr2 ->
      History.merge (h 3) ~old:(old kr0) kr1 kr2 >>= fun kr3 ->
      Irmin.Merge.exn kr3 >>= fun kr3 ->

      History.merge (h 4) ~old:(old kr2) kr2 kr3 >>= fun kr3_id' ->
      Irmin.Merge.exn kr3_id' >>= fun kr3_id' ->
      assert_equal (module KC) "kr3 id with immediate parent'" kr3 kr3_id';

      History.merge (h 5) ~old:(old kr0) kr0 kr3 >>= fun kr3_id ->
      Irmin.Merge.exn kr3_id >>= fun kr3_id ->
      assert_equal (module KC) "kr3 id with old parent" kr3 kr3_id;

      History.create (h 3) ~node:k4 ~parents:[kr1; kr2] >>= fun kr3' ->

      Commit.read_exn (c 0) kr3 >>= fun r3 ->
      Commit.read_exn (c 0) kr3' >>= fun r3' ->
      assert_equal (module C) "r3" r3 r3';
      assert_equal (module KC) "kr3" kr3 kr3';
      Lwt.return_unit
    in
    run x test

  let test_history x () =
    let test () =
      let task date =
        let i = Int64.of_int date in
        Irmin.Task.create ~date:i ~uid:i ~owner:"test" "Test commit"
      in
      S.create x.config task >>= fun t ->
      let h = h t in
      Graph.create (g t 0) [] >>= fun node ->
      let fail fmt =
        Printf.ksprintf (fun str -> OUnit.assert_string str; assert false) fmt
      in
      let assert_lcas_err msg err l2 =
        let str = function
          | `Too_many_lcas -> "Too_many_lcas"
          | `Max_depth_reached -> "Max_depth_reached"
        in
        let l2 = match l2 with
          | `Ok x -> fail "%s: %s" msg (Tc.show (module Tc.List(KC)) x)
          | `Too_many_lcas | `Max_depth_reached as x -> str x
        in
        assert_equal Tc.string msg (str err) l2
      in
      let assert_lcas msg l1 l2 =
        let l2 = match l2 with
          | `Ok x -> x
          | `Too_many_lcas -> fail "%s: Too many LCAs" msg
          | `Max_depth_reached -> fail"%s: max depth reached" msg
        in
        assert_equal (module Set(KC)) msg l1 l2
      in
      let assert_lcas msg ~max_depth n a b expected =
        S.of_head x.config task a >>= fun a ->
        S.of_head x.config task b >>= fun b ->
        S.lcas ~max_depth n a b >>= fun lcas ->
        assert_lcas msg expected lcas;
        S.lcas ~max_depth:(max_depth - 1) n a b >>= fun lcas ->
        let msg = Printf.sprintf "%s [max-depth=%d]" msg (max_depth - 1) in
        assert_lcas_err msg `Max_depth_reached lcas;
        Lwt.return_unit
      in

      (* test that we don't compute too many lcas

         0->1->2->3->4

      *)
      History.create (h 0) ~node ~parents:[]   >>= fun k0 ->
      History.create (h 1) ~node ~parents:[k0] >>= fun k1 ->
      History.create (h 2) ~node ~parents:[k1] >>= fun k2 ->
      History.create (h 3) ~node ~parents:[k2] >>= fun k3 ->
      History.create (h 4) ~node ~parents:[k3] >>= fun k4 ->

      assert_lcas "line lcas 1" ~max_depth:0 3 k3 k4 [k3] >>= fun () ->
      assert_lcas "line lcas 2" ~max_depth:1 3 k2 k4 [k2] >>= fun () ->
      assert_lcas "line lcas 3" ~max_depth:2 3 k1 k4 [k1] >>= fun () ->

      (* test for multiple lca

         4->10--->11-->13-->15
             |      \______/___
             |       ____/     \
             |      /           \
             \--->12-->14-->16-->17

      *)
      History.create (h 10) ~node ~parents:[k4]       >>= fun k10 ->
      History.create (h 11) ~node ~parents:[k10]      >>= fun k11 ->
      History.create (h 12) ~node ~parents:[k10]      >>= fun k12 ->
      History.create (h 13) ~node ~parents:[k11]      >>= fun k13 ->
      History.create (h 14) ~node ~parents:[k12]      >>= fun k14 ->
      History.create (h 15) ~node ~parents:[k12; k13] >>= fun k15 ->
      History.create (h 16) ~node ~parents:[k14]      >>= fun k16 ->
      History.create (h 17) ~node ~parents:[k11; k16] >>= fun k17 ->

      assert_lcas "x lcas 0" ~max_depth:0 5 k10 k10 [k10]      >>= fun () ->
      assert_lcas "x lcas 1" ~max_depth:0 5 k14 k14 [k14]      >>= fun () ->
      assert_lcas "x lcas 2" ~max_depth:0 5 k10 k11 [k10]      >>= fun () ->
      assert_lcas "x lcas 3" ~max_depth:1 5 k12 k16 [k12]      >>= fun () ->
      assert_lcas "x lcas 4" ~max_depth:1 5 k10 k13 [k10]      >>= fun () ->
      assert_lcas "x lcas 5" ~max_depth:2 5 k13 k14 [k10]      >>= fun () ->
      assert_lcas "x lcas 6" ~max_depth:3 5 k15 k16 [k12]      >>= fun () ->
      assert_lcas "x lcas 7" ~max_depth:3 5 k15 k17 [k11; k12] >>= fun () ->

      (* lcas on non transitive reduced graphs

                  /->16
                 |
         4->10->11->12->13->14->15
                 |        \--|--/
                 \-----------/
      *)
      History.create (h 10) ~node ~parents:[k4]      >>= fun k10 ->
      History.create (h 11) ~node ~parents:[k10]     >>= fun k11 ->
      History.create (h 12) ~node ~parents:[k11]     >>= fun k12 ->
      History.create (h 13) ~node ~parents:[k12]     >>= fun k13 ->
      History.create (h 14) ~node ~parents:[k11;k13] >>= fun k14 ->
      History.create (h 15) ~node ~parents:[k13;k14] >>= fun k15 ->
      History.create (h 16) ~node ~parents:[k11]     >>= fun k16 ->

      assert_lcas "weird lcas 1" ~max_depth:0 3 k14 k15 [k14] >>= fun () ->
      assert_lcas "weird lcas 2" ~max_depth:0 3 k13 k15 [k13] >>= fun () ->
      assert_lcas "weird lcas 3" ~max_depth:1 3 k12 k15 [k12] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:1 3 k11 k15 [k11] >>= fun () ->
      assert_lcas "weird lcas 4" ~max_depth:3 3 k15 k16 [k11] >>= fun () ->

      (* fast-forward *)
      S.of_head x.config task k12     >>= fun t12  ->
      S.fast_forward_head (t12 0) k16 >>= fun b1 ->
      assert_equal Tc.bool "ff 1.1" false b1;
      S.head_exn (t12 0)              >>= fun k12' ->
      assert_equal (module S.Head) "ff 1.2" k12 k12';

      S.fast_forward_head (t12 0) ~n:1 k14 >>= fun b2 ->
      assert_equal Tc.bool "ff 2.1" false b2;
      S.head_exn (t12 0)              >>= fun k12'' ->
      assert_equal (module S.Head) "ff 2.3" k12 k12'';

      S.fast_forward_head (t12 0) k14 >>= fun b3 ->
      assert_equal Tc.bool "ff 2.2" true b3;
      S.head_exn (t12 0)              >>= fun k14' ->
      assert_equal (module S.Head) "ff 2.3" k14 k14';

      return_unit
    in
    run x test

  let test_empty x () =
    let test () =
      S.empty x.config dummy_task >>= fun t ->

      S.head (t ()) >>= fun h ->
      assert_equal (module Tc.Option(S.Head)) "empty" None h;

      let v1 = v1 x in
      r1 x >>= fun r1 ->

      S.update (t ()) (p ["b"; "x"]) v1 >>= fun () ->

      S.head (t ()) >>= fun h ->
      assert_equal (module Tc.Option(S.Head)) "not empty" (Some r1) h;

      Lwt.return_unit

    in
    run x test

  let test_stores x () =
    let test () =
      create x >>= fun t ->

      S.clone_force task (t "clone") (S.Tag.of_hum "test") >>= fun t ->

      let v1 = v1 x in
      S.update (t "update") (p ["a";"b"]) v1 >>= fun () ->

      S.iter (t "iter") (fun k v ->
          v >>= fun v ->
          assert_equal (module K) "iter key" (p ["a";"b"]) k;
          assert_equal (module V) "iter value" v1 v;
          Lwt.return_unit;
        ) >>= fun () ->

      S.mem (t "mem1") (p ["a";"b"]) >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem1" true b1;
      S.mem (t "mem2") (p ["a"]) >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem2" false b2;
      S.read_exn (t "read1") (p ["a";"b"]) >>= fun v1' ->
      assert_equal (module V) "v1.1" v1 v1';

      S.head_exn (t "snapshot") >>= fun r1 ->

      S.clone_force task (t "clone") (S.Tag.of_hum "test") >>= fun t ->

      let v2 = v2 x in
      S.update (t "update") (p ["a";"c"]) v2 >>= fun () ->
      S.mem (t "mem3") (p ["a";"b"]) >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem3" true b1;
      S.mem (t "mem4") (p ["a"]) >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem4" false b2;
      S.read_exn (t "read2") (p ["a";"b"]) >>= fun v1' ->
      assert_equal (module V) "v1.1" v1 v1';
      S.mem (t "mem5") (p ["a";"c"]) >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem5" true b1;
      S.read_exn (t "read3") (p ["a";"c"]) >>= fun v2' ->
      assert_equal (module V) "v1.1" v2 v2';

      S.remove (t "remove") (p ["a";"b"]) >>= fun () ->
      S.read (t "read4") (p ["a";"b"]) >>= fun v1''->
      assert_equal (module Tc.Option(V)) "v1.2" None v1'';
      S.update_head (t "revert") r1 >>= fun () ->
      S.read (t "read") (p ["a";"b"]) >>= fun v1''->
      assert_equal (module Tc.Option(V)) "v1.3" (Some v1) v1'';
      S.list (t "list") (p ["a"]) >>= fun ks ->
      assert_equal (module Set(K)) "path" [p ["a";"b"]] ks;

      S.update (t "update2") (p ["a"; long_random_string]) v1 >>= fun () ->

      S.remove_rec (t "remove rec") (p ["a"]) >>= fun () ->
      S.list (t "list") (p []) >>= fun dirs ->
      assert_equal (module Set(K)) "remove rec" [] dirs;

      S.update (t "update root") (p []) v1 >>= fun () ->
      S.read_exn (t "read root") (p []) >>= fun v1' ->
      assert_equal (module V) "read root" v1 v1';

      S.update (t "update") (p ["a"]) v1 >>= fun () ->
      S.remove_rec (t "remove rec --all") (p []) >>= fun () ->
      S.list (t "list") (p []) >>= fun dirs ->


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

      View.empty () >>= fun v1 ->

      View.update v1 (p ["foo";"1"]) foo1 >>= fun () ->
      View.update v1 (p ["foo";"2"]) foo2 >>= fun () ->
      View.remove v1 (p ["foo";"1"]) >>= fun () ->
      View.remove v1 (p ["foo";"2"]) >>= fun () ->

      View.update_path (t "empty view") (p []) v1 >>= fun () ->
      S.head_exn (t "empty view") >>= fun head   ->
      Commit.read_exn (ct t "empty view") head >>= fun commit ->
      let node = match Commit.Val.node commit with
        | None -> failwith "empty node"
        | Some n -> n
      in
      Node.read_exn (n t "empty view") node >>= fun node ->
      assert_equal (module Node.Val) "empty view" Node.Val.empty node;

      View.empty () >>= fun v0 ->

      View.update v0 (p []) foo1 >>= fun () ->
      View.read   v0 (p []) >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "read /" (Some foo1) foo1';

      View.update v0 (p ["foo";"1"]) foo1 >>= fun () ->
      View.read   v0 (p ["foo";"1"]) >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "read foo/1" (Some foo1) foo1';

      View.update v0 (p ["foo";"2"]) foo2 >>= fun () ->
      View.read   v0 (p ["foo";"2"]) >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "read foo/2" (Some foo2) foo2';

      let check_view v =
        View.list v (p ["foo"]) >>= fun ls ->
        assert_equal (module Set(K)) "path1" [p ["foo";"1"]; p ["foo";"2"] ] ls;
        View.read v (p ["foo";"1"]) >>= fun foo1' ->
        assert_equal (module Tc.Option(V)) "foo1" (Some foo1) foo1';
        View.read v (p ["foo";"2"]) >>= fun foo2' ->
        assert_equal (module Tc.Option(V)) "foo2" (Some foo2) foo2';
        return_unit in

      Lwt_list.iter_s (fun (k,v) ->
          View.update v0 k v
        ) nodes >>= fun () ->
      check_view v0 >>= fun () ->

      View.update_path (t "update_path b/") (p ["b"]) v0 >>= fun () ->
      View.update_path (t "update_path a/") (p ["a"]) v0 >>= fun () ->

      S.list (t "list") (p ["b";"foo"]) >>= fun ls ->
      assert_equal (module Set(K)) "path2" [ p ["b";"foo";"1"];
                                             p ["b";"foo";"2"] ] ls;
      S.read (t "read foo1") (p ["b";"foo";"1"]) >>= fun foo1' ->
      assert_equal (module Tc.Option(V)) "foo1" (Some foo1) foo1';
      S.read (t "read foo2") (p ["a";"foo";"2"]) >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "foo2" (Some foo2) foo2';

      View.of_path (t "of_path") (p ["b"]) >>= fun v1 ->
      check_view v1 >>= fun () ->

      S.update (t "update b/x") (p ["b";"x"]) foo1 >>= fun () ->
      View.update v1 (p ["y"]) foo2 >>= fun () ->
      View.merge_path_exn (t "merge_path") (p ["b"]) v1 >>= fun () ->
      S.read (t "read b/x") (p ["b";"x"]) >>= fun foo1' ->
      S.read (t "read b/y") (p ["b";"y"]) >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "merge: b/x" (Some foo1) foo1';
      assert_equal (module Tc.Option(V)) "merge: b/y" (Some foo2) foo2';

      Lwt_list.iteri_s (fun i (k, v) ->
          let path = String.concat "/" (S.Key.map k S.Key.Step.to_hum) in
          S.read_exn (t @@ "read a/"^path) (S.Key.cons (l "a") k) >>= fun v' ->
          assert_equal (module V) ("a"^string_of_int i) v v';
          S.read_exn (t @@ "read b/"^path) (S.Key.cons (l "b") k) >>= fun v' ->
          assert_equal (module V) ("b"^string_of_int i) v v';
          return_unit
        ) nodes >>= fun () ->

      View.of_path (t "v2") (p ["b"]) >>= fun v2 ->
      View.read   v2 (p ["foo"; "1"]) >>= fun _ ->
      View.update v2 (p ["foo"; "1"]) foo2 >>= fun () ->
      View.update_path (t "v2") (p ["b"]) v2 >>= fun () ->
      S.read (t "read after v2") (p ["b";"foo";"1"]) >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "update view" (Some foo2) foo2';

      View.of_path (t "v3") (p ["b"]) >>= fun v3 ->
      View.read   v3 (p ["foo"; "1"]) >>= fun _ ->
      View.remove v3 (p ["foo"; "1"]) >>= fun () ->
      View.update_path (t "v3") (p ["b"]) v3 >>= fun () ->
      S.read (t "read after v3") (p ["b";"foo";"1"]) >>= fun foo2' ->
      assert_equal (module Tc.Option(V)) "remove view" None foo2';

      r1 x >>= fun r1 ->
      r2 x >>= fun r2 ->
      let ta = Irmin.Task.empty in
      View.make_head (t "mk-head") ta ~parents:[r1;r2] ~contents:v3 >>= fun h ->

      S.task_of_head (t "task") h >>= fun ta' ->
      assert_equal (module Irmin.Task) "task" ta ta';

      S.of_head x.config task h >>= fun tt ->
      S.history (tt "history") >>= fun g ->
      let pred = S.History.pred g h in
      let s = List.sort S.Head.compare in
      assert_equal (module Tc.List(S.Head)) "head" (s [r1;r2]) (s pred);

      S.read (tt "read tt") (p ["b";"foo";"1"]) >>= fun foo2'' ->
      assert_equal (module (Tc.Option(V))) "remove tt" None foo2'';

      return_unit
    in
    run x test

  module Sync = Irmin.Sync(S)

  let test_sync x () =
    let test () =
      create x >>= fun t1 ->
      let v1 = v1 x in
      let v2 = v2 x in


      S.update (t1 "update a/b") (p ["a";"b"]) v1 >>= fun () ->
      S.head_exn (t1 "head") >>= fun h ->
      S.head_exn (t1 "snapshot 1") >>= fun _r1 ->
      S.update (t1 "update a/c") (p ["a";"c"]) v2 >>= fun () ->
      S.head_exn (t1 "snapshot 2") >>= fun r2 ->
      S.update (t1 "update a/d") (p ["a";"d"]) v1 >>= fun () ->
      S.head_exn (t1 "snapshot 3") >>= fun _r3 ->

      S.history (t1 "history") ~min:[h] >>= fun h ->
      assert_equal (module Tc.Int) "history-v" 3 (S.History.nb_vertex h);
      assert_equal (module Tc.Int) "history-e" 2 (S.History.nb_edges h);

      let remote = Irmin.remote_store (module S) (t1 "remote") in

      Sync.fetch_exn (t1 "partial fetch") ~depth:0 remote >>= fun partial ->
      Sync.fetch_exn (t1 "total fetch") remote >>= fun full ->

      (* Restart a fresh store and import everything in there. *)
      let tag = S.Tag.of_hum "export" in
      S.of_tag x.config task tag >>= fun t2 ->
      S.update_head (t2 "partial update") partial >>= fun () ->

      S.mem (t2 "mem a/b") (p ["a";"b"]) >>= fun b1 ->
      assert_equal (module Tc.Bool) "mem-ab" true b1;

      S.mem (t2 "mem a/c") (p ["a";"c"]) >>= fun b2 ->
      assert_equal (module Tc.Bool) "mem-ac" true b2;

      S.mem (t2 "mem a/d") (p ["a";"d"]) >>= fun b3 ->
      assert_equal (module Tc.Bool) "mem-ad" true b3;
      S.read_exn (t2 "read a/d") (p ["a";"d"]) >>= fun v1' ->
      assert_equal (module V) "v1" v1' v1;

      S.update_head (t2 "revert to t2") r2 >>= fun () ->
      S.mem (t2 "mem a/b") (p ["a";"d"]) >>= fun b4 ->
      assert_equal (module Tc.Bool) "mem-ab" false b4;

      S.update_head (t2 "full update") full >>= fun () ->
      S.update_head (t2 "revert to r2") r2 >>= fun () ->
      S.mem (t2 "mem a/d") (p ["a";"d"]) >>= fun b4 ->
      assert_equal (module Tc.Bool) "mem-ad" false b4;
      return_unit
    in
    run x test

  module Dot = Irmin.Dot(S)

  let output_file t file =
    let buf = Buffer.create 1024 in
    let date d =
      let tm = Unix.localtime (Int64.to_float d) in
      sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    Dot.output_buffer t ~date buf >>= fun () ->
    let oc = open_out_bin (file ^ ".dot") in
    output_string oc (Buffer.contents buf);
    close_out oc;
    return_unit

  let test_merge x () =
    let test () =
      let v1 = string x "X1" in
      let v2 = string x "X2" in
      let v3 = string x "X3" in

      create x >>= fun t1 ->

      S.update (t1 "update a/b/a") (p ["a";"b";"a"]) v1 >>= fun () ->
      S.update (t1 "update a/b/b") (p ["a";"b";"b"]) v2 >>= fun () ->
      S.update (t1 "update a/b/c") (p ["a";"b";"c"]) v3 >>= fun () ->

      let test = S.Tag.of_hum "test" in

      S.clone_force task (t1 "clone master into test") test >>= fun t2 ->

      S.update (t1 "update master:a/b/b") (p ["a";"b";"b"]) v1 >>= fun () ->
      S.update (t1 "update master:a/b/b") (p ["a";"b";"b"]) v3 >>= fun () ->
      S.update (t2 "update test:a/b/c")   (p ["a";"b";"c"]) v1 >>= fun () ->

      output_file (t1 "before.dot") "before" >>= fun () ->
      S.merge_exn "merge test into master" t2 ~into:t1 >>= fun () ->
      output_file (t1 "after.dot") "after" >>= fun () ->

      S.read_exn (t1 "read master:a/b/c") (p ["a";"b";"c"]) >>= fun v1' ->
      S.read_exn (t2 "read test:a/b/c")   (p ["a";"b";"b"]) >>= fun v2' ->
      S.read_exn (t1 "read master:a/b/b") (p ["a";"b";"b"]) >>= fun v3' ->

      assert_equal (module V) "v1" v1 v1';
      assert_equal (module V) "v2" v2 v2';
      assert_equal (module V) "v3" v3 v3';

      return_unit
    in
    run x test

  let rec write fn = function
    | 0 -> return_unit
    | i -> (fn i >>= Lwt_unix.yield) <&> write fn (i-1)

  let rec read fn check = function
    | 0 -> return_unit
    | i ->
      fn i >>= fun v ->
      check i v;
      read fn check (i-1)

  let test_concurrent_low x () =
    let test_tags () =
      let k = t1 in
      r1 x >>= fun v ->
      create x >>= fun t ->
      let t = S.Private.tag_t (t "tag") in
      let write = write (fun _i -> Tag.update t k v) in
      let read =
        read
          (fun _i -> Tag.read_exn t k)
          (fun i  -> assert_equal (module S.Head) (sprintf "tag %d" i) v)
      in
      write 1 >>= fun () ->
      Lwt.join [ write 10; read 10; write 10; read 10; ]
    in
    let test_contents () =
      kv2 x >>= fun k ->
      let v = v2 x in
      create x >>= fun t ->
      let t = S.Private.contents_t (t "contents") in
      let write =
        write (fun _i -> Contents.add t v >>= fun _ -> Lwt.return_unit)
      in
      let read =
        read
          (fun _i -> Contents.read_exn t k)
          (fun i  -> assert_equal (module V) (sprintf "contents %d" i) v)
      in
      write 1 >>= fun () ->
      Lwt.join [ write 10; read 10; write 10; read 10; ]
    in
    run x (fun () -> Lwt.join [test_tags (); test_contents ()])

  let test_concurrent_updates x () =
    let test_one () =
      let k = p ["a";"b";"d"] in
      let v = string x "X1" in
      create x >>= fun t1 ->
      create x >>= fun t2 ->
      let mk t x = ksprintf t x in
      let write t = write (fun i -> S.update (mk t "update: one %d" i) k v) in
      let read t =
        read
          (fun i -> S.read_exn (mk t "read %d" i) k)
          (fun i -> assert_equal (module V) (sprintf "update: one %d" i) v)
      in
      Lwt.join [ write t1 10; write t2 10 ] >>= fun () ->
      Lwt.join [ read t1 10 ]
    in
    let test_multi () =
      let k i = p ["a";"b";"c"; string_of_int i ] in
      let v i = string x (sprintf "X%d" i) in
      create x >>= fun t1 ->
      create x >>= fun t2 ->
      let mk t x = ksprintf t x in
      let write t =
        write (fun i -> S.update (mk t "update: multi %d" i) (k i) (v i))
      in
      let read t =
        read
          (fun i -> S.read_exn (mk t "read %d" i) (k i))
          (fun i -> assert_equal (module V) (sprintf "update: multi %d" i) (v i))
      in
      Lwt.join [ write t1 10; write t2 10 ] >>= fun () ->
      Lwt.join [ read t1 10 ]
    in
    run x (fun () ->
        test_one   () >>= fun () ->
        test_multi () >>= fun () ->
        Lwt.return_unit
      )

  let test_concurrent_merges x () =
    let test () =
      let k i = p ["a";"b";"c"; string_of_int i ] in
      let v i = string x (sprintf "X%d" i) in
      create x >>= fun t1 ->
      create x >>= fun t2 ->
      let mk t x = ksprintf t x in
      let write t n =
        write (fun i ->
            let tag = S.Tag.of_hum (sprintf "tmp-%d-%d" n i) in
            S.clone_force task (mk t "cloning") tag >>= fun m ->
            S.update (m "update") (k i) (v i) >>= fun () ->
            Lwt_unix.yield () >>= fun () ->
            S.merge (sprintf "update: multi %d" i) m ~into:t >>=
            Irmin.Merge.exn
          )
      in
      let read t =
        read
          (fun i -> S.read_exn (mk t "read %d" i) (k i))
          (fun i -> assert_equal (module V) (sprintf "update: multi %d" i) (v i))
      in
      S.update (t1 "update") (k 0) (v 0) >>= fun () ->
      Lwt.join [ write t1 1 10; write t2 2 10 ] >>= fun () ->
      Lwt.join [ read t1 10 ]
    in
    run x test

  let test_concurrent_head_updates x () =
    let test () =
      let k i = p ["a";"b";"c"; string_of_int i ] in
      let v i = string x (sprintf "X%d" i) in
      create x >>= fun t1 ->
      create x >>= fun t2 ->
      let mk t x = ksprintf t x in
      let retry d fn =
        let rec aux i =
          fn () >>= function
          | true  -> Log.debug "%d: ok!" d; Lwt.return_unit
          | false -> Log.debug "%d: conflict, retrying (%d)." d i; aux (i+1)
        in
        aux 1
      in
      let write t n =
        write (fun i -> retry i (fun () ->
            S.head (t "head") >>= fun test ->
            let tag = S.Tag.of_hum (sprintf "tmp-%d-%d" n i) in
            S.clone_force task (mk t "cloning") tag >>= fun m ->
            S.update (m "update") (k i) (v i) >>= fun () ->
            S.head (m "head") >>= fun set ->
            Lwt_unix.yield () >>= fun () ->
            S.compare_and_set_head (t "compare_and_set") ~test ~set
          ))
      in
      let read t =
        read
          (fun i -> S.read_exn (mk t "read %d" i) (k i))
          (fun i -> assert_equal (module V) (sprintf "update: multi %d" i) (v i))
      in
      S.update (t1 "update") (k 0) (v 0) >>= fun () ->
      Lwt.join [ write t1 1 5; write t2 2 5 ] >>= fun () ->
      Lwt.join [ read t1 5 ]
    in
    run x test

end

let suite (speed, x) =
  let (module S) = x.store in
  let module T = Make(S) in
  x.name,
  [
    "Basic operations on contents"    , speed, T.test_contents x;
    "Basic operations on nodes"       , speed, T.test_nodes x;
    "Basic operations on commits"     , speed, T.test_commits x;
    "Basic operations on tags"        , speed, T.test_tags x;
    "Basic operations on watches"     , speed, T.test_watches x;
    "Basic merge operations"          , speed, T.test_simple_merges x;
    "Complex histories"               , speed, T.test_history x;
    "Empty stores"                    , speed, T.test_empty x;
    "High-level store operations"     , speed, T.test_stores x;
    "High-level operations on views"  , speed, T.test_views x;
    "High-level store synchronisation", speed, T.test_sync x;
    "High-level store merges"         , speed, T.test_merge x;
    "Low-level concurrency"           , speed, T.test_concurrent_low x;
    "Concurrent updates"              , speed, T.test_concurrent_updates x;
    "Concurrent head updates"         , speed, T.test_concurrent_head_updates x;
    "Concurrent merges"               , speed, T.test_concurrent_merges x;
  ]

let run name tl =
  let tl = List.map suite tl in
  Alcotest.run name tl
