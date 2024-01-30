(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
let src = Logs.Src.create "tests.tree" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)
module Hash = Irmin.Hash.SHA1

type ('key, 'value) op =
  | Add of 'key * 'value
  | Del of 'key
  | Find of 'key
  | Find_tree of 'key
  | Length of 'key * int

module Make (Conf : Irmin_pack.Conf.S) = struct
  module Store = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema)
  end

  let config ?(readonly = false) ?(fresh = true) root =
    Irmin_pack.config ~readonly ?index_log_size ~fresh root

  let info () = Store.Info.empty

  module Tree = Store.Tree

  type context = { repo : Store.repo; tree : Store.tree }

  let export_tree_to_store ~sw tree =
    let repo = Store.Repo.v ~sw (config ~fresh:true root) in
    let store = Store.empty repo in
    let () = Store.set_tree_exn ~info store [] tree in
    let tree = Store.tree store in
    { repo; tree }

  let close { repo; _ } = Store.Repo.close repo

  let fold ~order t ~init ~f =
    Tree.fold ~order ~force:`True ~cache:false ~uniq:`False
      ~contents:(fun k _v acc -> if k = [] then acc else f k acc)
      t init

  let init_bindings n =
    let zero = String.make 10 '0' in
    List.init n (fun n ->
        let h = Store.Contents.hash (string_of_int n) in
        let h = Irmin.Type.to_string Store.Hash.t h in
        ([ h ], zero))

  let init_tree ~sw bindings =
    let tree = Tree.empty () in
    let tree =
      List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    export_tree_to_store ~sw tree

  let find_tree tree k =
    let t = Tree.find_tree tree k in
    match t with None -> tree | Some t -> t

  let find tree k =
    let _ = Tree.find tree k in
    tree

  let run_one tree = function
    | Add (k, v) -> Tree.add tree k v
    | Del k -> Tree.remove tree k
    | Find k -> find tree k
    | Find_tree k -> find_tree tree k
    | Length (k, len_expected) ->
        let len_tree = Tree.length tree k in
        Alcotest.(check int)
          (Fmt.str "expected tree length at %a" Fmt.(Dump.list string) k)
          len_expected len_tree;
        tree

  let run_disjoint ops tree =
    let run_one op =
      let _ = run_one tree op in
      ()
    in
    let () = List.iter run_one ops in
    (tree, ())

  let run ops tree =
    let tree = List.fold_left run_one tree ops in
    (tree, ())

  let proof_of_ops repo hash ops : _ =
    let t, () = Store.Tree.produce_proof repo hash (run ops) in
    t

  let stream_of_ops repo hash ops : _ =
    let t, () = Store.Tree.produce_stream repo hash (run ops) in
    t

  let tree_proof_t = Tree.Proof.t Tree.Proof.tree_t
  let stream_proof_t = Tree.Proof.t Tree.Proof.stream_t
  let bin_of_proof = Irmin.Type.(unstage (to_bin_string tree_proof_t))
  let proof_of_bin = Irmin.Type.(unstage (of_bin_string tree_proof_t))
  let bin_of_stream = Irmin.Type.(unstage (to_bin_string stream_proof_t))
end

module Default = Make (Conf)
open Default

type bindings = string list list [@@deriving irmin]

let equal_ordered_slist ~msg l1 l2 = Alcotest.check_repr bindings_t msg l1 l2

let fold ~order ~force t ~init ~f =
  Tree.fold ~order ~force ~cache:false ~uniq:`False
    ~contents:(fun k _v acc -> if k = [] then acc else f k acc)
    t init

let equal_slist ~msg l1 l2 =
  Alcotest.(check (slist (list string) Stdlib.compare)) msg l1 l2

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

let version =
  let version = Sys.ocaml_version in
  Char.code version.[0] - 48

let some_steps = [ "0g"; "1g"; "0h"; "2g"; "1h"; "2h" ]

let some_random_steps =
  if version >= 5 then
    [ [ "1g" ]; [ "0h" ]; [ "2h" ]; [ "1h" ]; [ "2g" ]; [ "0g" ] ]
  else [ [ "2g" ]; [ "1h" ]; [ "0h" ]; [ "2h" ]; [ "0g" ]; [ "1g" ] ]

let another_random_steps =
  if version >= 5 then
    [ [ "0g" ]; [ "0h" ]; [ "1h" ]; [ "2h" ]; [ "2g" ]; [ "1g" ] ]
  else [ [ "1g" ]; [ "2h" ]; [ "1h" ]; [ "0g" ]; [ "0h" ]; [ "2g" ] ]

let zero = String.make 10 '0'
let bindings steps = List.map (fun x -> ([ x ], zero)) steps

let test_fold ?export_tree_to_store:(export_tree_to_store' = true) ~order
    bindings expected =
  Eio.Switch.run @@ fun sw ->
  let tree = Tree.empty () in
  let tree =
    List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree bindings
  in
  let close =
    match export_tree_to_store' with
    | true ->
        let ctxt = export_tree_to_store ~sw tree in
        fun () -> close ctxt
    | false -> fun () -> ()
  in
  let keys =
    fold
      ~force:
        (if export_tree_to_store' then `True else `False (Fun.const Fun.id))
      ~order tree ~init:[]
      ~f:(fun k acc -> k :: acc)
  in
  let keys = List.rev keys in
  let msg, equal_lists =
    match order with
    | `Sorted -> ("sorted", equal_ordered_slist)
    | `Random _ -> ("random", equal_ordered_slist)
    | `Undefined -> ("undefined", equal_slist)
  in
  equal_lists ~msg:(Fmt.str "Visit elements in %s order" msg) expected keys;
  close ()

let test_fold_sorted () =
  let bindings = bindings steps in
  let expected = List.map fst bindings in
  test_fold ~order:`Sorted bindings expected

let test_fold_random () =
  let bindings = bindings some_steps in
  let state = Random.State.make [| 0 |] in
  let () = test_fold ~order:(`Random state) bindings some_random_steps in
  let state = Random.State.make [| 1 |] in
  let () = test_fold ~order:(`Random state) bindings another_random_steps in

  (* Random fold order should still be respected if [~force:`False]. This is a
     regression test for a bug in which the fold order of in-memory nodes during
     a non-forcing traversal was always sorted. *)
  let state = Random.State.make [| 1 |] in
  let () =
    test_fold ~order:(`Random state) ~export_tree_to_store:false bindings
      another_random_steps
  in
  ()

let test_fold_undefined () =
  let bindings = bindings steps in
  let expected = List.map fst bindings in
  test_fold ~order:`Undefined bindings expected

let proof_of_bin s =
  match proof_of_bin s with Ok s -> s | Error (`Msg e) -> Alcotest.fail e

let check_equivalence tree proof op =
  match op with
  | Add (k, v) ->
      let tree = Tree.add tree k v in
      let proof = Tree.add proof k v in
      Alcotest.(check_repr Store.Hash.t)
        (Fmt.str "same hash add %a" Fmt.(Dump.list string) k)
        (Tree.hash tree) (Tree.hash proof);
      (tree, proof)
  | Del k ->
      let tree = Tree.remove tree k in
      let proof = Tree.remove proof k in
      Alcotest.(check_repr Store.Hash.t)
        (Fmt.str "same hash del %a" Fmt.(Dump.list string) k)
        (Tree.hash tree) (Tree.hash proof);
      (tree, proof)
  | Find k ->
      let v_tree = Tree.find tree k in
      let v_proof = Tree.find proof k in
      Alcotest.(check (option string))
        (Fmt.str "same value at %a" Fmt.(Dump.list string) k)
        v_tree v_proof;
      (tree, proof)
  | Find_tree k ->
      let v_tree = Tree.find_tree tree k in
      let v_proof = Tree.find_tree tree k in
      Alcotest.(check_repr [%typ: Store.tree option])
        (Fmt.str "same tree at %a" Fmt.(Dump.list string) k)
        v_tree v_proof;
      (tree, proof)
  | Length (k, len_expected) ->
      let len_tree = Tree.length tree k in
      Alcotest.(check int)
        (Fmt.str "expected tree length at %a" Fmt.(Dump.list string) k)
        len_expected len_tree;
      let len_proof = Tree.length proof k in
      Alcotest.(check int)
        (Fmt.str "same tree length at %a" Fmt.(Dump.list string) k)
        len_tree len_proof;
      (tree, proof)

let test_proofs ctxt ops =
  let tree = ctxt.tree in
  let key =
    match Tree.key tree with Some (`Node h) -> h | _ -> assert false
  in
  let hash = Tree.hash tree in

  (* Create a compressed parital Merle proof for ops *)
  let proof = proof_of_ops ctxt.repo (`Node key) ops in

  (* test encoding *)
  let enc = bin_of_proof proof in
  let dec = proof_of_bin enc in
  Alcotest.(check_repr tree_proof_t) "same proof" proof dec;

  (* test equivalence *)
  let tree_proof = Tree.Proof.to_tree proof in

  Alcotest.(check_repr Store.Hash.t)
    "same initial hash" hash (Tree.hash tree_proof);

  let _ =
    List.fold_left
      (fun (tree, proof) op -> check_equivalence tree proof op)
      (tree, tree_proof)
      (ops
      @ [
          Add ([ "00" ], "0");
          Add ([ "00" ], "1");
          Del [ "00" ];
          Find [ "00" ];
          Add ([ "00" ], "0");
          Add ([ "00" ], "1");
          Find [ "00" ];
          Find_tree [ "01" ];
          Find_tree [ "z"; "o"; "o" ];
        ])
  in
  ()

let test_large_inode () =
  Eio.Switch.run @@ fun sw ->
  let bindings = bindings steps in
  let ctxt = init_tree ~sw bindings in
  let ops = [ Add ([ "00" ], "3"); Del [ "01" ] ] in
  test_proofs ctxt ops

let fewer_steps =
["00"; "01"; "02"; "03"; "05"; "06"; "07"; "09"; "0a"; "0b"; "0c";
"0e"; "0f"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "19";
"1a"; "1b"; "1c"; "1d"; "1e"; "1f"; "20"; "22"; "23"; "25"; "26";
"27"; "28"; "2a"; ][@@ocamlformat "disable"]

let test_small_inode () =
  Eio.Switch.run @@ fun sw ->
  let bindings = bindings fewer_steps in
  let ctxt = init_tree ~sw bindings in
  let ops = [ Add ([ "00" ], ""); Del [ "01" ] ] in
  test_proofs ctxt ops

let test_length_proof () =
  Eio.Switch.run @@ fun sw ->
  let bindings = bindings fewer_steps in
  let size = List.length fewer_steps in
  let ctxt = init_tree ~sw bindings in
  let ops =
    [
      Length ([], size) (* initial size *);
      Add ([ "01" ], "0");
      Length ([], size) (* "01" was already accounted for *);
      Add ([ "01" ], "1");
      Length ([], size) (* adding it again doesn't change the length *);
      Add ([ "new" ], "0");
      Length ([], size + 1) (* "new" is a new file, so the length changes *);
      Add ([ "new" ], "1");
      Length ([], size + 1) (* adding it again doesn't change the length *);
      Del [ "inexistant" ];
      Length ([], size + 1)
      (* removing an inexistant object doesn't change the length *);
      Del [ "00" ];
      Length ([], size) (* but removing the existing "00" does *);
      Del [ "00" ];
      Length ([], size) (* removing "00" twice doesn't change the length *);
      Del [ "new" ];
      Length ([], size - 1) (* removing the fresh "new" does *);
      Del [ "new" ];
      Length ([], size - 1) (* but only once *);
      Add ([ "new" ], "2");
      Length ([], size) (* adding "new" again does *);
      Add ([ "00" ], "2");
      Length ([], size + 1) (* adding "00" again does too *);
    ]
  in
  test_proofs ctxt ops

let test_deeper_proof () =
  Eio.Switch.run @@ fun sw ->
  let ctxt =
    let tree = Tree.empty () in
    let level_one =
      let bindings = bindings fewer_steps in
      List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    let level_two =
      let tree = Tree.add_tree tree [ "0g" ] level_one in
      let bindings = bindings steps in
      List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    let level_three =
      let tree = Tree.add_tree tree [ "1g" ] level_two in
      let bindings = bindings fewer_steps in
      List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    export_tree_to_store ~sw level_three
  in
  let ops =
    [
      Find [ "1g"; "0g"; "00" ];
      Del [ "1g"; "0g"; "01" ];
      Find [ "02" ];
      Find_tree [ "1g"; "02" ];
    ]
  in
  test_proofs ctxt ops

module Binary = Make (struct
  let entries = 2
  let stable_hash = 2
  let inode_child_order = `Hash_bits
  let contents_length_header = Some `Varint
  let forbid_empty_dir_persistence = false
end)

(* test large compressed proofs *)
let test_large_proofs () =
  (* Build a proof on a large store (branching factor = 32) *)
  let bindings = init_bindings 100_000 in
  let ops n =
    bindings
    |> List.to_seq
    |> Seq.take n
    |> Seq.map (fun (s, _) -> Find_tree s)
    |> List.of_seq
  in

  let compare_proofs n =
    Eio.Switch.run @@ fun sw ->
    let ops = ops n in
    let ctxt = init_tree ~sw bindings in
    let key =
      match Tree.key ctxt.tree with Some (`Node k) -> k | _ -> assert false
    in
    let proof = proof_of_ops ctxt.repo (`Node key) ops in
    let enc_32 = bin_of_proof proof in
    let () = close ctxt in

    (* Build a stream proof *)
    let ctxt = init_tree ~sw bindings in
    let key =
      match Tree.key ctxt.tree with Some (`Node k) -> k | _ -> assert false
    in
    let proof = stream_of_ops ctxt.repo (`Node key) ops in
    let s_enc_32 = bin_of_stream proof in
    let () = close ctxt in

    (* Build a proof on a large store (branching factor = 2) *)
    let ctxt = Binary.init_tree ~sw bindings in
    let key =
      match Binary.Store.Tree.key ctxt.tree with
      | Some (`Node k) -> k
      | _ -> assert false
    in
    let proof = Binary.proof_of_ops ctxt.repo (`Node key) ops in
    let enc_2 = Binary.bin_of_proof proof in
    let () = Binary.close ctxt in

    (* Build a stream proof *)
    let ctxt = Binary.init_tree ~sw bindings in
    let key =
      match Binary.Store.Tree.key ctxt.tree with
      | Some (`Node k) -> k
      | _ -> assert false
    in
    let proof = Binary.stream_of_ops ctxt.repo (`Node key) ops in
    let s_enc_2 = Binary.bin_of_stream proof in
    let () = Binary.close ctxt in

    ( n,
      String.length enc_32 / 1024,
      String.length s_enc_32 / 1024,
      String.length enc_2 / 1024,
      String.length s_enc_2 / 1024 )
  in
  let a = compare_proofs 1 in
  let b = compare_proofs 100 in
  let c = compare_proofs 1_000 in
  let d = compare_proofs 10_000 in
  List.iter
    (fun (n, k32, sk32, k2, sk2) ->
      Fmt.pr "Size of Merkle proof for %d operations:\n" n;
      Fmt.pr "- Merkle B-trees (32 children)       : %dkB\n%!" k32;
      Fmt.pr "- stream Merkle B-trees (32 children): %dkB\n%!" sk32;
      Fmt.pr "- binary Merkle trees                : %dkB\n%!" k2;
      Fmt.pr "- stream binary Merkle trees         : %dkB\n%!" sk2)
    [ a; b; c; d ]

module Custom = Make (struct
  let entries = 2
  let stable_hash = 2

  let index ~depth step =
    let ascii_code = Bytes.get step depth |> Char.code in
    ascii_code - 48

  let inode_child_order = `Custom index
  let contents_length_header = Some `Varint
  let forbid_empty_dir_persistence = false
end)

module P = Custom.Tree.Proof

let pp_proof = Irmin.Type.pp (P.t P.tree_t)
let pp_stream = Irmin.Type.pp (P.t P.stream_t)

let check_hash h s =
  let s' = Irmin.Type.(to_string Hash.t) h in
  Alcotest.(check string) "check hash" s s'

let check_contents_hash h s =
  match h with
  | `Node _ -> Alcotest.failf "Expected kinded hash to be contents"
  | `Contents (h, ()) ->
      let s' = Irmin.Type.(to_string Hash.t) h in
      Alcotest.(check string) "check hash" s s'

let test_extenders () =
  let bindings =
    [ ([ "00000" ], "x"); ([ "00001" ], "y"); ([ "00010" ], "z") ]
  in
  let bindings2 = ([ "10000" ], "x1") :: bindings in
  let bindings3 = ([ "10001" ], "y") :: bindings2 in

  let f t =
    let v = Custom.Tree.get t [ "00000" ] in
    Alcotest.(check string) "00000" "x" v;
    (t, ())
  in

  let check_proof bindings =
    Eio.Switch.run @@ fun sw ->
    let ctxt = Custom.init_tree ~sw bindings in
    let key = Custom.Tree.key ctxt.tree |> Option.get in
    let p, () = Custom.Tree.produce_proof ctxt.repo key f in
    [%log.debug "Verifying proof %a" pp_proof p];
    let r = Custom.Tree.verify_proof p f in
    match r with
    | Ok (_, ()) -> ()
    | Error e ->
        Alcotest.failf "check_proof: %a"
          (Irmin.Type.pp Custom.Tree.verifier_error_t)
          e
  in
  let () = List.iter check_proof [ bindings; bindings2; bindings3 ] in

  let check_stream bindings =
    Eio.Switch.run @@ fun sw ->
    let ctxt = Custom.init_tree ~sw bindings in
    let key = Custom.Tree.key ctxt.tree |> Option.get in
    let p, () = Custom.Tree.produce_stream ctxt.repo key f in
    [%log.debug "Verifying stream %a" pp_stream p];
    let r = Custom.Tree.verify_stream p f in
    match r with
    | Ok (_, ()) -> ()
    | Error e ->
        Alcotest.failf "check_stream: %a"
          (Irmin.Type.pp Custom.Tree.verifier_error_t)
          e
  in
  List.iter check_stream [ bindings; bindings2; bindings3 ]

let test_hardcoded_stream () =
  let bindings =
    [ ([ "00100" ], "x"); ([ "00101" ], "y"); ([ "00110" ], "z") ]
  in
  let fail elt =
    Alcotest.failf "Unexpected elt in stream %a" (Irmin.Type.pp P.elt_t) elt
  in
  Eio.Switch.run @@ fun sw ->
  let ctxt = Custom.init_tree ~sw bindings in
  let key = Custom.Tree.key ctxt.tree |> Option.get in
  let f t =
    let path = [ "00100" ] in
    let v = Custom.Tree.get t path in
    Alcotest.(check ~pos:__POS__ string) "" (List.assoc path bindings) v;
    (t, ())
  in
  let p, () = Custom.Tree.produce_stream ctxt.repo key f in
  let state = P.state p in
  let counter = ref 0 in
  Seq.iter
    (fun elt ->
      (match !counter with
      | 0 -> (
          match elt with
          | P.Inode_extender { length; segments = [ 0; 0; 1 ]; proof = h }
            when length = 3 ->
              check_hash h "25c1a3d3bb7e5124cf61954851d0c9ccf5113d4e"
          | _ -> fail elt)
      | 1 -> (
          match elt with
          | P.Inode { length; proofs = [ (0, h1); (1, h0) ] } when length = 3 ->
              check_hash h0 "8410f4d1be1d571f0d63638927d42c7c1c6f3df1";
              check_hash h1 "580c8955c438ca5b1f94d2f4eb712a85e2634b70"
          | _ -> fail elt)
      | 2 -> (
          match elt with
          | P.Node [ ("00100", h0); ("00101", h1) ] ->
              check_contents_hash h0 "11f6ad8ec52a2984abaafd7c3b516503785c2072";
              check_contents_hash h1 "95cb0bfd2977c761298d9624e4b4d4c72a39974a"
          | _ -> fail elt)
      | 3 -> ( match elt with P.Contents "x" -> () | _ -> fail elt)
      | _ -> fail elt);
      incr counter)
    state;
  if !counter <> 4 then Alcotest.fail "Not enough elements in the stream"

let test_hardcoded_proof () =
  let bindings =
    [ ([ "00000" ], "x"); ([ "00001" ], "y"); ([ "00010" ], "z") ]
  in
  let fail_with_tree elt =
    Alcotest.failf "Unexpected elt in proof %a" (Irmin.Type.pp P.tree_t) elt
  in
  let fail_with_inode_tree elt =
    Alcotest.failf "Unexpected elt in proof %a"
      (Irmin.Type.pp P.inode_tree_t)
      elt
  in
  Eio.Switch.run @@ fun sw ->
  let ctxt = Custom.init_tree ~sw bindings in
  let key = Custom.Tree.key ctxt.tree |> Option.get in
  let f t =
    let v = Custom.Tree.get t [ "00000" ] in
    Alcotest.(check string) "00000" "x" v;
    (t, ())
  in
  let p, () = Custom.Tree.produce_proof ctxt.repo key f in
  let state = P.state p in

  let check_depth_2 = function
    | P.Inode_values
        [ ("00000", Contents ("x", ())); ("00001", Blinded_contents (h1, ())) ]
      ->
        check_hash h1 "95cb0bfd2977c761298d9624e4b4d4c72a39974a"
    | t -> fail_with_inode_tree t
  in
  let check_depth_1 = function
    | P.Inode_tree { length = 3; proofs = [ (0, t); (1, P.Blinded_inode h1) ] }
      ->
        check_hash h1 "4295267989ab4c4a036eb78f0610a57042e2b49f";
        check_depth_2 t
    | t -> fail_with_inode_tree t
  in
  let () =
    match (state : P.tree) with
    | P.Extender { length = 3; segments = [ 0; 0; 0 ]; proof = t } ->
        check_depth_1 t
    | _ -> fail_with_tree state
  in
  ()

let tree_of_list ls =
  let tree = Tree.empty () in
  List.fold_left (fun tree (k, v) -> Tree.add tree k v) tree ls

let test_proof_exn _ =
  let x = "x" in
  let y = "y" in
  let hx = Store.Contents.hash x in
  let hy = Store.Contents.hash y in
  let stream_elt1 : P.elt = Contents y in
  let stream_elt2 : P.elt = Contents x in
  let stream_elt3 : P.elt =
    Node [ ("bx", `Contents (hx, ())); ("by", `Contents (hy, ())) ]
  in
  let tree = tree_of_list [ ([ "bx" ], "x"); ([ "by" ], "y") ] in
  let hash = Tree.hash tree in

  let stream_all =
    P.v ~before:(`Node hash) ~after:(`Node hash)
      (List.to_seq [ stream_elt3; stream_elt2; stream_elt1 ])
  in
  let stream_short =
    P.v ~before:(`Node hash) ~after:(`Node hash)
      (List.to_seq [ stream_elt3; stream_elt2 ])
  in
  let f_all t =
    let _ = Custom.Tree.find t [ "bx" ] in
    let _ = Custom.Tree.find t [ "by" ] in
    (t, ())
  in
  let f_short t =
    let _ = Custom.Tree.find t [ "bx" ] in
    (t, ())
  in
  (* Test the Stream_too_long error. *)
  let r = Custom.Tree.verify_stream stream_all f_short in
  let () =
    match r with
    | Error (`Stream_too_long _) -> ()
    | _ -> Alcotest.fail "expected Stream_too_long error"
  in
  (* Test the Stream_too_short error. *)
  let r = Custom.Tree.verify_stream stream_short f_all in
  let () =
    match r with
    | Error (`Stream_too_short _) -> ()
    | _ -> Alcotest.fail "expected Stream_too_short error"
  in
  (* Test the correct usecase. *)
  let r = Custom.Tree.verify_stream stream_all f_all in
  let () =
    match r with
    | Ok (_, ()) -> ()
    | Error e -> (
        match e with
        | `Proof_mismatch str ->
            Alcotest.failf "unexpected Proof_mismatch error: %s" str
        | `Stream_too_long str ->
            Alcotest.failf "unexpected Stream_too_long error: %s" str
        | `Stream_too_short str ->
            Alcotest.failf "unexpected Stream_too_short error: %s" str)
  in
  ()

let test_reexport_node () =
  Eio.Switch.run @@ fun sw ->
  let tree = Store.Tree.add (Store.Tree.empty ()) [ "foo"; "a" ] "a" in
  let repo1 = Store.Repo.v ~sw (config ~fresh:true root) in
  let _ =
    Store.Backend.Repo.batch repo1 (fun c n _ -> Store.save_tree repo1 c n tree)
  in
  let () = Store.Repo.close repo1 in
  (* Re-export the same tree using a different repo. *)
  let repo2 = Store.Repo.v ~sw (config ~fresh:false root) in
  let _ =
    Alcotest.check_raises "re-export tree from another repo"
      (Failure "Can't export the node key from another repo") (fun () ->
        Store.Backend.Repo.batch repo2 (fun c n _ ->
            Store.save_tree repo2 c n tree))
  in
  let () = Store.Repo.close repo2 in
  (* Re-export a fresh tree using a different repo. *)
  let repo2 = Store.Repo.v ~sw (config ~fresh:false root) in
  let tree = Store.Tree.add (Store.Tree.empty ()) [ "foo"; "a" ] "a" in
  let _ = Store.Tree.hash tree in
  let c1 = Store.Tree.get_tree tree [ "foo" ] in
  let _ =
    Store.Backend.Repo.batch repo2 (fun c n _ -> Store.save_tree repo2 c n c1)
  in
  let () =
    match Store.Tree.destruct c1 with
    | `Contents _ -> Alcotest.fail "got `Contents, expected `Node"
    | `Node node ->
        let _v = Store.to_backend_node node in
        ()
  in
  Store.Repo.close repo2

let tests =
  [
    Alcotest.test_case "fold over keys in sorted order" `Quick test_fold_sorted;
    Alcotest.test_case "fold over keys in random order" `Quick test_fold_random;
    Alcotest.test_case "fold over keys in undefined order" `Quick
      test_fold_undefined;
    Alcotest.test_case "test Merkle proof for large inodes" `Quick
      test_large_inode;
    Alcotest.test_case "test Merkle proof for small inodes" `Quick
      test_small_inode;
    Alcotest.test_case "test Merkle proof for Tree.length" `Quick
      test_length_proof;
    Alcotest.test_case "test deeper Merkle proof" `Quick test_deeper_proof;
    Alcotest.test_case "test large Merkle proof" `Slow test_large_proofs;
    Alcotest.test_case "test extenders in stream proof" `Quick test_extenders;
    Alcotest.test_case "test hardcoded stream proof" `Quick
      test_hardcoded_stream;
    Alcotest.test_case "test hardcoded proof" `Quick test_hardcoded_proof;
    Alcotest.test_case "test stream proof exn" `Quick test_proof_exn;
    Alcotest.test_case "test reexport node" `Quick test_reexport_node;
  ]
