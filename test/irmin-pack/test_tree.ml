(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

let root = Filename.concat "_build" "test-tree"

module Store = struct
  module Maker = Irmin_pack.Maker (Irmin_pack.Version.V2) (Conf)
  include Maker.Make (Schema)
end

let config ?(readonly = false) ?(fresh = true) root =
  Irmin_pack.config ~readonly ?index_log_size ~fresh root

module Tree = Store.Tree

type bindings = string list list [@@deriving irmin]

let equal_ordered_slist ~msg l1 l2 = Alcotest.check_repr bindings_t msg l1 l2

let equal_slist ~msg l1 l2 =
  Alcotest.(check (slist (list string) Stdlib.compare)) msg l1 l2

type context = { repo : Store.repo; tree : Store.tree }

let persist_tree tree =
  let* repo = Store.Repo.v (config root) in
  let* store = Store.empty repo in
  let* () = Store.set_tree_exn ~info:Store.Info.none store [] tree in
  let+ tree = Store.tree store in
  { repo; tree }

let close { repo; _ } = Store.Repo.close repo

let fold ~order t ~init ~f =
  Tree.fold ~order ~force:`True ~cache:false ~uniq:`False
    ~contents:(fun k _v acc -> if k = [] then Lwt.return acc else f k acc)
    t init

let steps =
  ["00"; "01"; "02"; "03"; "05"; "06"; "07"; "09"; "0a"; "0b"; "0c";
   "0e"; "0f"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "19";
   "1a"; "1b"; "1c"; "1d"; "1e"; "1f"; "20"; "22"; "23"; "25"; "26";
   "27"; "28"; "2a"; "2b"; "2f"; "30"; "31"; "32"; "33"; "35"; "36";
   "37"; "3a"; "3b"; "3c"; "3d"; "3e"; "3f"; "40"; "42"; "43"; "45";
   "46"; "47"; "48"; "4a"; "4b"; "4c"; "4e"; "4f"; "50"; "52"; "53";
   "54"; "55"; "56"; "57"; "59"; "5b"; "5c"; "5f"; "60"; "61"; "62";
   "63"; "64"; "65"; "66"; "67"; "69"; "6b"; "6c"; "6d"; "6e"; "6f";
   "71"; "72"; "73"; "74"; "75"; "78"; "79"; "7a"; "7b"; "7c"; "7d";
   "7e"; "80"; "82"; "83"; "84"; "85"; "86"; "88"; "8b"; "8c"; "8d";
   "8f"; "92"; "93"; "94"; "96"; "97"; "99"; "9a"; "9b"; "9d"; "9e";
   "9f"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "aa";
   "ab"; "ac"; "ad"; "ae"; "af"; "b0"; "b1"; "b2"; "b3"; "b4"; "b6";
   "b8"; "b9"; "bb"; "bc"; "bf"; "c0"; "c1"; "c2"; "c3"; "c4"; "c5";
   "c8"; "c9"; "cb"; "cc"; "cd"; "ce"; "d0"; "d1"; "d2"; "d4"; "d5";
   "d7"; "d8"; "d9"; "da"; "e0"; "e3"; "e6"; "e8"; "e9"; "ea"; "ec";
   "ee"; "ef"; "f0"; "f1"; "f5"; "f7"; "f8"; "f9"; "fb"; "fc"; "fd";
   "fe"; "ff"; "g0"; "g1"; "g2"; "g3"; "g4"; "g5"; "g6"; "g7"; "g8";
   "h0"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "h7"; "h8"; "h9"; "ha";
   "i0"; "i1"; "i2"; "i3"; "i4"; "i5"; "i6"; "i7"; "i8"; "i9"; "ia";
   "j0"; "j1"; "j2"; "j3"; "j4"; "j5"; "j6"; "j7"; "j8"; "j9"; "ja";
   "k0"; "k1"; "k2"; "k3"; "k4"; "k5"; "k6"; "k7"; "k8"; "k9"; "ka";
   "l0"; "l1"; "l2"; "l3"; "l4"; "l5"; "l6"; "l7"; "l8"; "l9"; "la";
   "m0"; "m1"; "m2"; "m3"; "m4"; "m5"; "m6"; "m7"; "m8"; "m9"; "ma";
   "n0"; "n1"; "n2"; "n3"; "n4"; "n5"; "n6"; "n7"; "n8"; "n9"; "na";
   "p0"; "p1"; "p2"; "p3"; "p4"; "p5"; "p6"; "p7"; "p8"; "p9"; "pa";
   "q0"; "q1"; "q2"; "q3"; "q4"; "q5"; "q6"; "q7"; "q8"; "q9"; "qa";
   "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "ra";]
[@@ocamlformat "disable"]

let some_steps = [ "0g"; "1g"; "0h"; "2g"; "1h"; "2h" ]

let some_random_steps =
  [ [ "2g" ]; [ "1h" ]; [ "0h" ]; [ "2h" ]; [ "0g" ]; [ "1g" ] ]

let another_random_steps =
  [ [ "1g" ]; [ "2h" ]; [ "1h" ]; [ "0g" ]; [ "0h" ]; [ "2g" ] ]

let bindings steps =
  let zero = String.make 10 '0' in
  List.map (fun x -> ([ x ], zero)) steps

let test_fold ~order bindings expected =
  let tree = Tree.empty () in
  let* tree =
    Lwt_list.fold_left_s (fun tree (k, v) -> Tree.add tree k v) tree bindings
  in
  let* ctxt = persist_tree tree in
  let* keys =
    fold ~order ctxt.tree ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
  in
  let keys = List.rev keys in
  let msg, equal_lists =
    match order with
    | `Sorted -> ("sorted", equal_ordered_slist)
    | `Random _ -> ("random", equal_ordered_slist)
    | `Undefined -> ("undefined", equal_slist)
  in
  equal_lists ~msg:(Fmt.str "Visit elements in %s order" msg) expected keys;
  close ctxt

let test_fold_sorted () =
  let bindings = bindings steps in
  let expected = List.map fst bindings in
  test_fold ~order:`Sorted bindings expected

let test_fold_random () =
  let bindings = bindings some_steps in
  let state = Random.State.make [| 0 |] in
  let* () = test_fold ~order:(`Random state) bindings some_random_steps in
  let state = Random.State.make [| 1 |] in
  test_fold ~order:(`Random state) bindings another_random_steps

let test_fold_undefined () =
  let bindings = bindings steps in
  let expected = List.map fst bindings in
  test_fold ~order:`Undefined bindings expected

let tests =
  [
    Alcotest.test_case "fold over keys in sorted order" `Quick (fun () ->
        Lwt_main.run (test_fold_sorted ()));
    Alcotest.test_case "fold over keys in random order" `Quick (fun () ->
        Lwt_main.run (test_fold_random ()));
    Alcotest.test_case "fold over keys in undefined order" `Quick (fun () ->
        Lwt_main.run (test_fold_undefined ()));
  ]
