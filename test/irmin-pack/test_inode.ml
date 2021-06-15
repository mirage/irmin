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

let root = Filename.concat "_build" "test-inode"
let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Conf = struct
  let entries = 2
  let stable_hash = 3
end

let log_size = 1000

module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Node = Irmin.Private.Node.Make (H) (Path) (Metadata)
module Index = Irmin_pack.Index.Make (H)
module Inter = Irmin_pack.Inode.Make_internal (Conf) (H) (Node)
module Inode = Irmin_pack.Inode.Make_persistent (H) (Node) (Inter) (P)

module Context = struct
  type t = {
    index : Index.t;
    store : read Inode.t;
    clone : readonly:bool -> read Inode.t Lwt.t;
  }

  let get_store ?(lru_size = 0) () =
    rm_dir root;
    let index = Index.v ~log_size ~fresh:true root in
    let+ store = Inode.v ~fresh:true ~lru_size ~index root in
    let clone ~readonly =
      Inode.v ~lru_size ~fresh:false ~readonly ~index root
    in

    { index; store; clone }

  let close t =
    Index.close t.index;
    Inode.close t.store
end

type pred = [ `Contents of H.t | `Inode of H.t | `Node of H.t ]
[@@deriving irmin]

let pp_pred = Irmin.Type.pp pred_t

module H_contents =
  Irmin.Hash.Typed
    (H)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

let normal x = `Contents (x, Metadata.default)
let node x = `Node x
let foo = H_contents.hash "foo"
let bar = H_contents.hash "bar"
let check_hash = Alcotest.check_repr Inode.Val.hash_t
let check_values = Alcotest.check_repr Inode.Val.t

(* Exhaustive inode structure generator *)
module Inode_permutations_generator = struct
  type step = string
  type content = Inode.Val.value
  type inode = Inode.value

  module StepMap = Map.Make (struct
    type t = step

    let compare = compare
  end)

  module StepSet = Set.Make (struct
    type t = Path.step

    let compare = compare
  end)

  module StepSetMap = Map.Make (struct
    type t = StepSet.t

    let compare = StepSet.compare
  end)

  type t = {
    steps : step list;
    content_per_step : content StepMap.t;
    steps_per_tree : StepSet.t list;
    trees : inode list;
    tree_per_steps : inode StepSetMap.t;
  }

  (** [gen_step index_list] uses brute force to generate a step such that
      [Inter.Val.index ~depth:i] maps to the ith index in the [index_list]. *)
  let gen_step : int list -> Path.step =
    let tbl = Hashtbl.create 10 in
    let max_brute_force_iterations = 100 in
    let letters_per_step = (max_brute_force_iterations + 25) / 26 in
    fun indices ->
      let rec aux i =
        if i > max_brute_force_iterations then
          failwith "Could not quickly generate a step"
        else
          let s = Common.random_letters letters_per_step in
          let is_valid =
            indices
            |> List.mapi (fun depth i -> (depth, i))
            |> List.for_all (fun (depth, i) -> Inter.Val.index ~depth s = i)
          in
          if is_valid then s else aux (i + 1)
      in
      match Hashtbl.find_opt tbl indices with
      | Some s -> s
      | None ->
          let s = aux 0 in
          Hashtbl.add tbl indices s;
          s

  (** List all the steps that would fill a tree of depth [maxdepth_of_test]. *)
  let gen_steps entries maxdepth_of_test : step list =
    let ( ** ) a b = float_of_int a ** float_of_int b |> int_of_float in
    List.init (entries ** maxdepth_of_test) (fun i ->
        List.init maxdepth_of_test (fun j ->
            let j = entries ** (maxdepth_of_test - j - 1) in
            (* In the binary case (Conf.entries = 2), [j] is now the mask of the
               bit to look at *)
            i / j mod entries))
    |> List.map gen_step

  let powerset xs =
    List.fold_left
      (fun acc x -> acc @ List.map (fun ys -> x :: ys) acc)
      [ [] ] xs

  let v ~entries ~maxdepth_of_test =
    let ( ** ) a b = float_of_int a ** float_of_int b |> int_of_float in
    let steps = gen_steps entries maxdepth_of_test in
    let content_per_step =
      List.map (fun s -> (s, H_contents.hash s |> normal)) steps
      |> List.to_seq
      |> StepMap.of_seq
    in
    let steps_per_tree : StepSet.t list =
      powerset steps |> List.map List.to_seq |> List.map StepSet.of_seq
    in
    Alcotest.(check int)
      "Size of the powerset"
      (List.length steps_per_tree)
      (2 ** entries ** maxdepth_of_test);
    let trees : Inode.value list =
      List.map
        (fun steps ->
          let steps = StepSet.elements steps in
          let contents =
            List.map (fun s -> StepMap.find s content_per_step) steps
          in
          List.combine steps contents |> Inode.Val.v)
        steps_per_tree
    in
    let tree_per_steps : Inode.value StepSetMap.t =
      List.combine steps_per_tree trees |> List.to_seq |> StepSetMap.of_seq
    in
    { steps; content_per_step; steps_per_tree; trees; tree_per_steps }

  (** [steps t] is a list of length [entries ^ maxdepth_of_test] (8) containing
      the necessary steps to fill a tree of depth equal to [maxdepth_of_test]
      (3). *)
  let steps : t -> step list = fun { steps; _ } -> steps

  let content_of_step : t -> step -> content =
   fun { content_per_step; _ } s -> StepMap.find s content_per_step

  (** [trees t] is a list of length [2 ^ (entries ^ maxdepth_of_test)] (256)
      containing pairs of steps set/inode tree. This list is formed from the
      powerset of [steps t], it contains all the possible structural
      permutations for an inode tree of depth equal to [maxdepth_of_test] and
      width equal to [entries]. *)
  let trees : t -> (StepSet.t * Inode.value) list =
   fun { trees; steps_per_tree; _ } -> List.combine steps_per_tree trees

  (** [tree_of_steps t ss] is the inode tree associated to [ss] in [trees t].

      E.g, [tree_of_steps t StepSet.empty] is the empty inode. *)
  let tree_of_steps : t -> StepSet.t -> Inode.value =
   fun { tree_per_steps; _ } steps -> StepSetMap.find steps tree_per_steps
end

let check_node msg v t =
  let h = Inter.Val.hash v in
  let+ h' = Inode.batch t.Context.store (fun i -> Inode.add i v) in
  check_hash msg h h'

let check_hardcoded_hash msg h v =
  h |> Irmin.Type.of_string Inode.Val.hash_t |> function
  | Error (`Msg str) -> Alcotest.failf "hash of string failed: %s" str
  | Ok hash -> check_hash msg hash (Inter.Val.hash v)

(** Test add values from an empty node. *)
let test_add_values () =
  rm_dir root;
  let* t = Context.get_store () in
  check_node "hash empty node" Inode.Val.empty t >>= fun () ->
  let v1 = Inode.Val.add Inode.Val.empty "x" (normal foo) in
  let v2 = Inode.Val.add v1 "y" (normal bar) in
  check_node "node x+y" v2 t >>= fun () ->
  check_hardcoded_hash "hash v2" "d4b55db5d2d806283766354f0d7597d332156f74" v2;
  let v3 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  check_values "add x+y vs v x+y" v2 v3;
  Context.close t

let integrity_check ?(stable = true) v =
  Alcotest.(check bool) "check stable" (Inter.Val.stable v) stable;
  if not (Inter.Val.integrity_check v) then
    Alcotest.failf "node does not satisfy stability invariants %a"
      (Irmin.Type.pp Inode.Val.t)
      v

(** Test add to inodes. *)
let test_add_inodes () =
  rm_dir root;
  let* t = Context.get_store () in
  let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.add v1 "z" (normal foo) in
  let v3 =
    Inode.Val.v [ ("x", normal foo); ("z", normal foo); ("y", normal bar) ]
  in
  check_values "add x+y+z vs v x+z+y" v2 v3;
  check_hardcoded_hash "hash v3" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v3;
  integrity_check v1;
  integrity_check v2;
  let v4 = Inode.Val.add v2 "a" (normal foo) in
  let v5 =
    Inode.Val.v
      [
        ("x", normal foo);
        ("z", normal foo);
        ("a", normal foo);
        ("y", normal bar);
      ]
  in
  check_values "add x+y+z+a vs v x+z+a+y" v4 v5;
  check_hardcoded_hash "hash v4" "c330c08571d088141dfc82f644bffcfcf6696539" v4;
  integrity_check v4 ~stable:false;
  Context.close t

(** Test remove values on an empty node. *)
let test_remove_values () =
  rm_dir root;
  let* t = Context.get_store () in
  let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.remove v1 "y" in
  let v3 = Inode.Val.v [ ("x", normal foo) ] in
  check_values "node x obtained two ways" v2 v3;
  check_hardcoded_hash "hash v2" "a1996f4309ea31cc7ba2d4c81012885aa0e08789" v2;
  let v4 = Inode.Val.remove v2 "x" in
  check_node "remove results in an empty node" Inode.Val.empty t >>= fun () ->
  let v5 = Inode.Val.remove v4 "x" in
  check_values "remove on an already empty node" v4 v5;
  check_hardcoded_hash "hash v4" "5ba93c9db0cff93f52b521d7420e43f6eda2784f" v4;
  Alcotest.(check bool) "v5 is empty" (Inode.Val.is_empty v5) true;
  Context.close t

(** Test remove and add values to go from stable to unstable inodes. *)
let test_remove_inodes () =
  rm_dir root;
  let* t = Context.get_store () in
  let v1 =
    Inode.Val.v [ ("x", normal foo); ("y", normal bar); ("z", normal foo) ]
  in
  check_hardcoded_hash "hash v1" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v1;
  let v2 = Inode.Val.remove v1 "x" in
  let v3 = Inode.Val.v [ ("y", normal bar); ("z", normal foo) ] in
  check_values "node y+z obtained two ways" v2 v3;
  check_hardcoded_hash "hash v2" "ea22a2936eed53978bde62f0185cee9d8bbf9489" v2;
  let v4 =
    Inode.Val.v
      [
        ("x", normal foo);
        ("z", normal foo);
        ("a", normal foo);
        ("y", normal bar);
      ]
  in
  let v5 = Inode.Val.remove v4 "a" in
  check_values "node x+y+z obtained two ways" v1 v5;
  integrity_check v1;
  integrity_check v5;
  Context.close t

(** For each of the 256 possible inode trees with [depth <= 3] and
    [width = Conf.entries = 2] built by [Inode.Val.v], assert that
    independently, all the possible [Inode.Val.add]/[Inode.Val.remove]
    operations yield a tree computable by [Inode.Val.v].

    In other words. Let [T] be the set of all possible trees (256). Let [O] be
    the set of unitary [tree -> tree] operations (8). If all the combinations of
    [T] and [O] yield trees in [T] then, by induction, the representation is
    unique.

    Caveats

    If something breaks at [depth > 3 || entries <> 2], this won't be caught
    here.

    If a corrupted tree is constructed using [Elt.decode_bin] and [Val.of_bin],
    this won't be caught here.

    If a corrupted subtree is loaded through the [find] function when an inode
    lazily loads subtrees, this won't be caught here. *)
let test_representation_uniqueness_maxdepth_3 () =
  let module P = Inode_permutations_generator in
  let p = P.v ~entries:Conf.entries ~maxdepth_of_test:3 in
  let f steps tree s =
    (* [steps, tree] is one of the known pair built using [Val.v]. Let's try to
       add or remove [s] from it and see if something breaks. *)
    if P.StepSet.mem s steps then
      let steps' = P.StepSet.remove s steps in
      let tree'_ref = P.tree_of_steps p steps' in
      let tree'_new = Inode.Val.remove tree s in
      check_values
        "The representation of the received tree obtained through [remove] \
         differs from the expected one obtained through [v]."
        tree'_ref tree'_new
    else
      let steps' = P.StepSet.add s steps in
      let c = P.content_of_step p s in
      let tree'_ref = P.tree_of_steps p steps' in
      let tree'_new = Inode.Val.add tree s c in
      check_values
        "The representation of the received tree obtained through [remove] \
         differs from the expected one obtained through [v]."
        tree'_ref tree'_new
  in
  List.iter
    (fun (ss, t) -> List.iter (fun s -> f ss t s) (P.steps p))
    (P.trees p);
  Lwt.return_unit

let test_truncated_inodes () =
  let to_truncated inode =
    let encode, decode =
      let t = Inode.Val.t in
      Irmin.Type.(encode_bin t |> unstage, decode_bin t |> unstage)
    in
    let encode inode =
      let buf = Buffer.create 0 in
      encode inode (Buffer.add_string buf);
      Buffer.contents buf
    in
    let decode str = decode str 0 |> snd in
    inode |> encode |> decode
  in
  let with_failure f =
    Alcotest.check_raises
      "Iteration on that Truncated inode with broken pointers was expected to \
       fail."
      (Failure
         "Impossible to load the subtree on an inode deserialized using Repr") f
  in
  let s00, s01, s11, s10 =
    Inode_permutations_generator.
      ( gen_step [ 0; 0 ],
        gen_step [ 0; 1 ],
        gen_step [ 1; 1 ],
        gen_step [ 1; 0 ] )
  in
  let iter_steps f =
    List.iter (fun step -> f step |> ignore) [ s00; s01; s11; s10 ]
  in
  let iter_steps_with_failure f =
    List.iter
      (fun step -> with_failure (fun () -> f step |> ignore))
      [ s00; s01; s11; s10 ]
  in
  (* v1 is a Truncated inode of tag Values. No pointers. *)
  let v1 =
    Inode.Val.v [ (s00, normal foo); (s10, normal foo) ] |> to_truncated
  in
  Inode.Val.list v1 |> ignore;
  (iter_steps @@ fun step -> Inode.Val.find v1 step);
  (iter_steps @@ fun step -> Inode.Val.add v1 step (normal bar));
  (iter_steps @@ fun step -> Inode.Val.remove v1 step);
  (* v2 is just a Truncated inode of tag Tree. The pointers are built after
     the call to [to_truncated], they are [Intact]. *)
  let v2 = Inode.Val.add v1 s01 (normal foo) in
  Inode.Val.list v2 |> ignore;
  (iter_steps @@ fun step -> Inode.Val.find v1 step);
  (iter_steps @@ fun step -> Inode.Val.add v1 step (normal bar));
  (iter_steps @@ fun step -> Inode.Val.remove v1 step);
  (* v3 is just a Truncated inode of tag Tree. The pointers are built before
     the call to [to_truncated], they are [Broken]. *)
  let v3 =
    Inode.Val.v [ (s00, normal foo); (s10, normal bar); (s01, normal bar) ]
    |> to_truncated
  in
  (with_failure @@ fun () -> Inode.Val.list v3 |> ignore);
  (iter_steps_with_failure @@ fun step -> Inode.Val.find v3 step);
  (iter_steps_with_failure @@ fun step -> Inode.Val.add v3 step (normal bar));
  (iter_steps_with_failure @@ fun step -> Inode.Val.remove v3 step);
  Lwt.return_unit

let test_intermediate_inode_as_root () =
  rm_dir root;
  let* t = Context.get_store () in
  let s000, s001, s010 =
    Inode_permutations_generator.
      (gen_step [ 0; 0; 0 ], gen_step [ 0; 0; 1 ], gen_step [ 0; 1; 0 ])
  in
  let v0 =
    Inode.Val.v [ (s000, normal foo); (s001, normal bar); (s010, normal foo) ]
  in
  let* h_depth0 = Inode.batch t.store @@ fun store -> Inode.add store v0 in
  let (`Inode h_depth1) =
    match Inode.Val.pred v0 with
    | [ (`Inode _ as pred) ] -> pred
    | l ->
        Alcotest.failf
          "Expected one `Inode predecessors, got [%a], a list of length %d."
          Fmt.(list ~sep:(any " ; ") pp_pred)
          l (List.length l)
  in

  (* On inode with depth=0 *)
  let* v =
    Inode.find t.store h_depth0 >|= function
    | None -> Alcotest.fail "Could not fetch inode from backend"
    | Some v -> v
  in
  if Inode.Val.list v |> List.length <> 3 then
    Alcotest.fail "Failed to list entries of loaded inode";
  let _ = Inode.Val.remove v s000 in
  let _ = Inode.Val.add v s000 (normal foo) in
  let* _ = Inode.batch t.store @@ fun store -> Inode.add store v in

  (* On inode with depth=1 *)
  let* v =
    Inode.find t.store h_depth1 >|= function
    | None -> Alcotest.fail "Could not fetch inode from backend"
    | Some v -> v
  in
  if Inode.Val.list v |> List.length <> 3 then
    Alcotest.fail "Failed to list entries of loaded inode";
  let with_exn f =
    Alcotest.check_raises
      "Write-only operation is forbiden on intermediate inode"
      (Failure "Cannot perform operation on non-root inode value.") (fun () ->
        f () |> ignore)
  in
  with_exn (fun () -> Inode.Val.remove v s000);
  with_exn (fun () -> Inode.Val.add v s000 (normal foo));
  let* () =
    Inode.batch t.store (fun store ->
        with_exn (fun () -> Inode.add store v);
        Lwt.return_unit)
  in
  Lwt.return_unit

let test_concrete_inodes () =
  rm_dir root;
  let* t = Context.get_store () in
  let pp_concrete = Irmin.Type.pp_json ~minify:false Inter.Val.Concrete.t in
  let result_t = Irmin.Type.result Inode.Val.t Inter.Val.Concrete.error_t in
  let testable =
    Alcotest.testable
      (Irmin.Type.pp_json ~minify:false result_t)
      Irmin.Type.(unstage (equal result_t))
  in
  let check v =
    let len = Inter.Val.length v in
    integrity_check ~stable:(len <= Conf.stable_hash) v;
    let c = Inter.Val.to_concrete v in
    let r = Inter.Val.of_concrete c in
    let msg = Fmt.str "%a" pp_concrete c in
    Alcotest.check testable msg (Ok v) r
  in
  let v = Inode.Val.v [ ("a", normal foo) ] in
  check v;
  let v = Inode.Val.v [ ("a", normal foo); ("y", node bar) ] in
  check v;
  let v = Inode.Val.v [ ("x", node foo); ("a", normal foo); ("y", node bar) ] in
  check v;
  let v =
    Inode.Val.v
      [
        ("x", normal foo); ("z", normal foo); ("a", normal foo); ("y", node bar);
      ]
  in
  check v;
  Context.close t

let tests =
  [
    Alcotest.test_case "add values" `Quick (fun () ->
        Lwt_main.run (test_add_values ()));
    Alcotest.test_case "add values to inodes" `Quick (fun () ->
        Lwt_main.run (test_add_inodes ()));
    Alcotest.test_case "remove values" `Quick (fun () ->
        Lwt_main.run (test_remove_values ()));
    Alcotest.test_case "remove inodes" `Quick (fun () ->
        Lwt_main.run (test_remove_inodes ()));
    Alcotest.test_case "test concrete inodes" `Quick (fun () ->
        Lwt_main.run (test_concrete_inodes ()));
    Alcotest.test_case "test representation uniqueness" `Quick (fun () ->
        Lwt_main.run (test_representation_uniqueness_maxdepth_3 ()));
    Alcotest.test_case "test truncated inodes" `Quick (fun () ->
        Lwt_main.run (test_truncated_inodes ()));
    Alcotest.test_case "test intermediate inode as root" `Quick (fun () ->
        Lwt_main.run (test_intermediate_inode_as_root ()));
  ]
