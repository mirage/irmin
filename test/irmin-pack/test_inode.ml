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

let root = Filename.concat "_build" "test-inode"
let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let check_iter = Test_hashes.check_iter

(* bad value that causes Invalid_depth to be raised *)
let wrong_depth = 100

module Inode_modules
    (Conf : Irmin_pack.Conf.S)
    (Schema : Irmin.Schema.S)
    (Contents : sig
      val foo : Schema.Contents.t
      val bar : Schema.Contents.t
    end) =
struct
  module Key = Irmin_pack_unix.Pack_key.Make (Schema.Hash)

  module Node =
    Irmin.Node.Generic_key.Make_v2 (Schema.Hash) (Schema.Path) (Schema.Metadata)
      (Key)
      (Key)

  module Index = Irmin_pack_unix.Index.Make (Schema.Hash)

  module Inter =
    Irmin_pack.Inode.Make_internal (Conf) (Schema.Hash) (Key) (Node)

  module Inter_mock = struct
    include Inter

    module Raw = struct
      include Inter.Raw

      let depth _v = Some wrong_depth
    end
  end

  module Io = Irmin_pack_unix.Io.Unix
  module Errs = Irmin_pack_unix.Io_errors.Make (Io)
  module File_manager = Irmin_pack_unix.File_manager.Make (Io) (Index) (Errs)
  module Dict = File_manager.Dict
  module Dispatcher = Irmin_pack_unix.Dispatcher.Make (File_manager)

  module Pack =
    Irmin_pack_unix.Pack_store.Make (File_manager) (Dispatcher) (Schema.Hash)
      (Inter.Raw)
      (Errs)

  module Pack_mock =
    Irmin_pack_unix.Pack_store.Make (File_manager) (Dispatcher) (Schema.Hash)
      (Inter_mock.Raw)
      (Errs)

  module Inode =
    Irmin_pack_unix.Inode.Make_persistent (Schema.Hash) (Node) (Inter) (Pack)

  module Inode_mock =
    Irmin_pack_unix.Inode.Make_persistent (Schema.Hash) (Node) (Inter_mock)
      (Pack_mock)

  module Contents_value =
    Irmin_pack.Pack_value.Of_contents (Conf) (Schema.Hash) (Key)
      (Schema.Contents)

  module Contents_store =
    Irmin_pack_unix.Pack_store.Make (File_manager) (Dispatcher) (Schema.Hash)
      (Contents_value)
      (Errs)

  module Context_make
      (Inode : Irmin_pack_unix.Inode.Persistent
                 with type file_manager = File_manager.t
                  and type dict = Dict.t
                  and type dispatcher = Dispatcher.t) =
  struct
    type t = {
      store : read Inode.t;
      store_contents : read Contents_store.t;
      fm : File_manager.t;
      (* Two contents values that are guaranteed to be read by {!store}: *)
      foo : Key.t;
      bar : Key.t;
    }

    let config ~indexing_strategy ~readonly ~fresh name =
      let module Index = Irmin_pack.Indexing_strategy in
      let indexing_strategy =
        if indexing_strategy = `always then Index.always else Index.minimal
      in
      Irmin_pack.Conf.init ~fresh ~readonly ~indexing_strategy ~lru_size:0 name

    (* TODO : remove duplication with irmin_pack/ext.ml *)
    let get_fm ~sw config =
      let readonly = Irmin_pack.Conf.readonly config in

      if readonly then File_manager.open_ro ~sw config |> Errs.raise_if_error
      else
        let fresh = Irmin_pack.Conf.fresh config in
        let root = Irmin_pack.Conf.root config in
        (* make sure the parent dir exists *)
        let () =
          match Sys.is_directory (Filename.dirname root) with
          | false -> Unix.mkdir (Filename.dirname root) 0o755
          | true -> ()
        in
        match (Io.classify_path root, fresh) with
        | `No_such_file_or_directory, _ ->
            File_manager.create_rw ~sw ~overwrite:false config
            |> Errs.raise_if_error
        | `Directory, true ->
            File_manager.create_rw ~sw ~overwrite:true config
            |> Errs.raise_if_error
        | `Directory, false ->
            File_manager.open_rw ~sw config |> Errs.raise_if_error
        | (`File | `Other), _ -> Errs.raise_error (`Not_a_directory root)

    let get_store ~sw ~indexing_strategy () =
      [%log.app "Constructing a fresh context for use by the test"];
      rm_dir root;
      let config = config ~indexing_strategy ~readonly:false ~fresh:true root in
      let fm = get_fm ~sw config in
      let dict = File_manager.dict fm in
      let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
      let lru = Irmin_pack_unix.Lru.create config in
      let store = Inode.v ~config ~fm ~dict ~dispatcher ~lru in
      let store_contents =
        Contents_store.v ~config ~fm ~dict ~dispatcher ~lru
      in
      let foo, bar =
        Contents_store.batch store_contents (fun writer ->
            let foo = Contents_store.add writer Contents.foo in
            let bar = Contents_store.add writer Contents.bar in
            (foo, bar))
      in
      [%log.app "Test context constructed"];
      { store; store_contents; fm; foo; bar }

    let close t = File_manager.close t.fm |> Errs.raise_if_error
    (* closes dict, inodes and contents store. *)
  end

  module Context = Context_make (Inode)
  module Context_mock = Context_make (Inode_mock)
end

module Conf = struct
  let entries = 2
  let stable_hash = 3
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = false
end

module String_contents = struct
  let foo = "foo"
  let bar = "bar"
end

module S = Inode_modules (Conf) (Schema) (String_contents)
open S
open Schema

type pred = [ `Contents of Key.t | `Inode of Key.t | `Node of Key.t ]
[@@deriving irmin]

let pp_pred = Irmin.Type.pp pred_t

module H_contents = Irmin.Hash.Typed (Hash) (Schema.Contents)

let normal x = `Contents (x, Metadata.default)
let node x = `Node x
let check_hash = Alcotest.check_repr Inode.Val.hash_t
let check_values = Alcotest.check_repr Inode.Val.t

let check_int pos ?(msg = "") ~expected actual =
  Alcotest.(check ~pos int) msg expected actual

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

  (** [gen_step Inter index_list] uses brute force to generate a step such that
      [Inter.Val.index ~depth:i] maps to the ith index in the [index_list]. *)
  let gen_step :
      (module Irmin_pack.Inode.Internal with type Val.step = Path.step) ->
      int list ->
      Path.step =
    let tbl = Hashtbl.create 10 in
    let max_brute_force_iterations = 100 in
    let letters_per_step = (max_brute_force_iterations + 25) / 26 in
    fun inter indices ->
      let module Inter =
        (val inter : Irmin_pack.Inode.Internal with type Val.step = Path.step)
      in
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
    |> List.map (gen_step (module Inter))

  let powerset xs =
    List.fold_left
      (fun acc x -> acc @ List.map (fun ys -> x :: ys) acc)
      [ [] ] xs

  let v ~entries ~maxdepth_of_test =
    let ( ** ) a b = float_of_int a ** float_of_int b |> int_of_float in
    let steps = gen_steps entries maxdepth_of_test in
    let content_per_step =
      List.map
        (fun s -> (s, H_contents.hash s |> Key.unfindable_of_hash |> normal))
        steps
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
          List.combine steps contents |> Inode.Val.of_list)
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
  let hash = Inter.Val.hash_exn v in
  let key = Inode.batch t.Context.store (fun i -> Inode.add i v) in
  let hash' = Key.to_hash key in
  check_hash msg hash hash'

let check_hardcoded_hash msg h v =
  h |> Irmin.Type.of_string Inode.Val.hash_t |> function
  | Error (`Msg str) -> Alcotest.failf "hash of string failed: %s" str
  | Ok hash -> check_hash msg hash (Inter.Val.hash_exn v)

(** Test add values from an empty node. *)
let test_add_values ~indexing_strategy =
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
  check_node "hash empty node" (Inode.Val.empty ()) t;
  let v1 = Inode.Val.add (Inode.Val.empty ()) "x" (normal foo) in
  let v2 = Inode.Val.add v1 "y" (normal bar) in
  check_node "node x+y" v2 t;
  check_hardcoded_hash "hash v2" "d4b55db5d2d806283766354f0d7597d332156f74" v2;
  let v3 = Inode.Val.of_list [ ("x", normal foo); ("y", normal bar) ] in
  check_values "add x+y vs v x+y" v2 v3;
  Context.close t

let test_add_values () =
  let () = test_add_values ~indexing_strategy:`always in
  test_add_values ~indexing_strategy:`minimal

let integrity_check ?(stable = true) v =
  Alcotest.(check bool) "check stable" (Inter.Val.stable v) stable;
  if not (Inter.Val.integrity_check v) then
    Alcotest.failf "node does not satisfy stability invariants %a"
      (Irmin.Type.pp Inode.Val.t)
      v

(** Test add to inodes. *)
let test_add_inodes ~indexing_strategy =
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
  let v1 = Inode.Val.of_list [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.add v1 "z" (normal foo) in
  let v3 =
    Inode.Val.of_list
      [ ("x", normal foo); ("z", normal foo); ("y", normal bar) ]
  in
  check_values "add x+y+z vs v x+z+y" v2 v3;
  check_hardcoded_hash "hash v3" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v3;
  integrity_check v1;
  integrity_check v2;
  let v4 = Inode.Val.add v2 "a" (normal foo) in
  let v5 =
    Inode.Val.of_list
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

let test_add_inodes () =
  let () = test_add_inodes ~indexing_strategy:`always in
  test_add_inodes ~indexing_strategy:`minimal

(** Test remove values on an empty node. *)
let test_remove_values ~indexing_strategy =
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
  let v1 = Inode.Val.of_list [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.remove v1 "y" in
  let v3 = Inode.Val.of_list [ ("x", normal foo) ] in
  check_values "node x obtained two ways" v2 v3;
  check_hardcoded_hash "hash v2" "a1996f4309ea31cc7ba2d4c81012885aa0e08789" v2;
  let v4 = Inode.Val.remove v2 "x" in
  check_node "remove results in an empty node" (Inode.Val.empty ()) t;
  let v5 = Inode.Val.remove v4 "x" in
  check_values "remove on an already empty node" v4 v5;
  check_hardcoded_hash "hash v4" "5ba93c9db0cff93f52b521d7420e43f6eda2784f" v4;
  Alcotest.(check bool) "v5 is empty" (Inode.Val.is_empty v5) true;
  Context.close t

let test_remove_values () =
  let () = test_remove_values ~indexing_strategy:`always in
  test_remove_values ~indexing_strategy:`minimal

(** Test remove and add values to go from stable to unstable inodes. *)
let test_remove_inodes ~indexing_strategy =
  rm_dir root;
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
  let v1 =
    Inode.Val.of_list
      [ ("x", normal foo); ("y", normal bar); ("z", normal foo) ]
  in
  check_hardcoded_hash "hash v1" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v1;
  let v2 = Inode.Val.remove v1 "x" in
  let v3 = Inode.Val.of_list [ ("y", normal bar); ("z", normal foo) ] in
  check_values "node y+z obtained two ways" v2 v3;
  check_hardcoded_hash "hash v2" "ea22a2936eed53978bde62f0185cee9d8bbf9489" v2;
  let v4 =
    Inode.Val.of_list
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

let test_remove_inodes () =
  let () = test_remove_inodes ~indexing_strategy:`always in
  test_remove_inodes ~indexing_strategy:`minimal

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
    (P.trees p)

let test_truncated_inodes ~indexing_strategy =
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
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
    let decode str = decode str (ref 0) in
    inode |> encode |> decode
  in
  let with_failure f =
    Alcotest.check_raises
      "Iteration on that Truncated inode with broken pointers was expected to \
       fail."
      (Failure
         "Impossible to load the subtree on an inode deserialized using Repr") f
  in
  let gen_step = Inode_permutations_generator.gen_step (module Inter) in
  let s00, s01, s11, s10 =
    (gen_step [ 0; 0 ], gen_step [ 0; 1 ], gen_step [ 1; 1 ], gen_step [ 1; 0 ])
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
    Inode.Val.of_list [ (s00, normal foo); (s10, normal foo) ] |> to_truncated
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
    Inode.Val.of_list
      [ (s00, normal foo); (s10, normal bar); (s01, normal bar) ]
    |> to_truncated
  in
  (with_failure @@ fun () -> Inode.Val.list v3 |> ignore);
  (iter_steps_with_failure @@ fun step -> Inode.Val.find v3 step);
  (iter_steps_with_failure @@ fun step -> Inode.Val.add v3 step (normal bar));
  (iter_steps_with_failure @@ fun step -> Inode.Val.remove v3 step);
  Context.close t

let test_truncated_inodes () =
  let () = test_truncated_inodes ~indexing_strategy:`always in
  test_truncated_inodes ~indexing_strategy:`minimal

let test_intermediate_inode_as_root ~indexing_strategy =
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
  let gen_step = Inode_permutations_generator.gen_step (module Inter) in
  let s000, s001, s010 =
    (gen_step [ 0; 0; 0 ], gen_step [ 0; 0; 1 ], gen_step [ 0; 1; 0 ])
  in
  let v0 =
    Inode.Val.of_list
      [ (s000, normal foo); (s001, normal bar); (s010, normal foo) ]
  in
  let h_depth0 = Inode.batch t.store @@ fun store -> Inode.add store v0 in
  let (`Inode h_depth1) =
    match Inode.Val.pred v0 with
    | [ (_, (`Inode _ as pred)) ] -> pred
    | l ->
        let l = List.map snd l in
        Alcotest.failf
          "Expected one `Inode predecessors, got [%a], a list of length %d."
          Fmt.(list ~sep:(any " ; ") pp_pred)
          l (List.length l)
  in

  (* On inode with depth=0 *)
  let v =
    match Inode.find t.store h_depth0 with
    | None -> Alcotest.fail "Could not fetch inode from backend"
    | Some v -> v
  in
  if Inode.Val.list v |> List.length <> 3 then
    Alcotest.fail "Failed to list entries of loaded inode";
  let _ = Inode.Val.remove v s000 in
  let _ = Inode.Val.add v s000 (normal foo) in
  let _ = Inode.batch t.store @@ fun store -> Inode.add store v in

  (* On inode with depth=1 *)
  let v =
    match Inode.find t.store h_depth1 with
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
  Inode.batch t.store (fun store -> with_exn (fun () -> Inode.add store v))

let test_invalid_depth_intermediate_inode ~indexing_strategy =
  Eio.Switch.run @@ fun sw ->
  let t = Context_mock.get_store ~sw ~indexing_strategy () in
  let { Context_mock.foo; bar; _ } = t in
  let gen_step = Inode_permutations_generator.gen_step (module Inter_mock) in
  let s000, s001, s010 =
    (gen_step [ 0; 0; 0 ], gen_step [ 0; 0; 1 ], gen_step [ 0; 1; 0 ])
  in

  let v0 =
    Inode_mock.Val.of_list
      [ (s000, normal foo); (s001, normal bar); (s010, normal foo) ]
  in
  let h_depth0 =
    Inode_mock.batch t.store @@ fun store -> Inode_mock.add store v0
  in

  (* On inode with depth=0 *)
  let v =
    match Inode_mock.find t.store h_depth0 with
    | None -> Alcotest.fail "Could not fetch inode from backend"
    | Some v -> v
  in

  (* list to make the inodes realised *)
  let _ =
    try
      let _ = Inode_mock.Val.list v in
      Alcotest.fail "Should have raised Invalid_depth"
    with
    | Inter_mock.Raw.Invalid_depth { expected; got; _ } ->
        let _ = Alcotest.(check int) "expected depth" expected 1 in
        let _ = Alcotest.(check int) "actual depth" got wrong_depth in
        ()
    | _ -> Alcotest.fail "Wrong exception - should have raised Invalid_depth"
  in
  Context_mock.close t

let test_intermediate_inode_as_root () =
  let () = test_invalid_depth_intermediate_inode ~indexing_strategy:`always in
  let () = test_invalid_depth_intermediate_inode ~indexing_strategy:`minimal in
  let () = test_intermediate_inode_as_root ~indexing_strategy:`always in
  test_intermediate_inode_as_root ~indexing_strategy:`minimal

let test_concrete_inodes ~indexing_strategy =
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in
  let { Context.foo; bar; _ } = t in
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
  let v = Inode.Val.of_list [ ("a", normal foo) ] in
  check v;
  let v = Inode.Val.of_list [ ("a", normal foo); ("y", node bar) ] in
  check v;
  let v =
    Inode.Val.of_list [ ("x", node foo); ("a", normal foo); ("y", node bar) ]
  in
  check v;
  let v =
    Inode.Val.of_list
      [
        ("x", normal foo); ("z", normal foo); ("a", normal foo); ("y", node bar);
      ]
  in
  check v;
  Context.close t

let test_invalid_depth_concrete_inodes ~indexing_strategy =
  let module C = Inter.Val.Concrete in
  Eio.Switch.run @@ fun sw ->
  let t = Context.get_store ~sw ~indexing_strategy () in

  (* idea is to try and directly construct a Concrete that has a bad depth structure ie *)
  (* "Tree": { *)
  (*   "depth": 0, *)
  (*   "length": 4, *)
  (*   "pointers": [ *)
  (*     { *)
  (*       "index": 1, *)
  (*       "pointer": "a551ad62ed063843207366e425c462c7b3eb1769", *)
  (*       "tree": { *)
  (*         "Tree": { *)
  (*           "depth": 2, <--- HERE wrong depth*)
  (*           "length": 4, *)
  (*           etc ...     *)

  (* subtrees need to be non-empty *)
  let v = C.Blinded in
  let ptr = { C.index = 0; pointer = H_contents.hash ""; tree = v } in
  let tree_bad_depth = C.Tree { depth = 2; length = 1; pointers = [ ptr ] } in
  let ptr =
    { C.index = 1; pointer = H_contents.hash ""; tree = tree_bad_depth }
  in
  let tree_top = C.Tree { depth = 0; length = 1; pointers = [ ptr ] } in
  let _ =
    match Inter.Val.of_concrete tree_top with
    | Error (`Invalid_depth _) -> ()
    | Ok _ -> Alcotest.fail "of_concrete - shouldnt be ok"
    | Error e ->
        let _ = [%log.warn "unexpected err: %a" C.pp_error e] in
        Alcotest.fail "of_concrete - should be Invalid_depth error"
  in

  Context.close t

let test_concrete_inodes () =
  let () = test_invalid_depth_concrete_inodes ~indexing_strategy:`always in
  let () = test_invalid_depth_concrete_inodes ~indexing_strategy:`minimal in
  let () = test_concrete_inodes ~indexing_strategy:`always in
  test_concrete_inodes ~indexing_strategy:`minimal

module Inode_tezos = struct
  module S =
    Inode_modules (Conf) (Irmin_tezos.Schema)
      (struct
        let foo = Bytes.make 10 '0'
        let bar = Bytes.make 10 '1'
      end)

  let encode_bin h v =
    let v1 = S.Inter.Val.to_raw v in
    S.Inter.Raw.encode_bin
      ~dict:(fun _ -> None)
      ~offset_of_key:(fun _ -> None)
      h v1

  let hex_encode s = Hex.of_string s |> Hex.show

  let test_encode_bin_values ~indexing_strategy =
    rm_dir root;
    Eio.Switch.run @@ fun sw ->
    let t = S.Context.get_store ~sw ~indexing_strategy () in
    let { S.Context.foo; _ } = t in
    let v = S.Inode.Val.of_list [ ("x", normal foo); ("z", normal foo) ] in
    let h = S.Inter.Val.hash_exn v in
    let hash_to_bin_string =
      Irmin.Type.(unstage (to_bin_string S.Inode.Val.hash_t))
    in
    let key_to_bin_string =
      Irmin.Type.(unstage (to_bin_string S.Inode.Key.t))
    in
    let hex_of_h = h |> hash_to_bin_string |> hex_encode in
    let hex_of_foo = foo |> key_to_bin_string |> hex_encode in
    let checks =
      [
        ("hash", hex_of_h);
        ("magic R", hex_encode "R");
        ("data length", "48");
        ("Values", "00");
        ("length", "02");
        ("contents-x-dd", "09");
        ("Direct", "01");
        ("x", "78");
        ("hash of contents", hex_of_foo);
        ("contents-x-dd", "09");
        ("Direct", "01");
        ("z", "7a");
        ("hash of contents", hex_of_foo);
      ]
    in
    check_iter "encode_bin" (encode_bin h) v checks;
    S.Context.close t

  let test_encode_bin_values () =
    let () = test_encode_bin_values ~indexing_strategy:`always in
    test_encode_bin_values ~indexing_strategy:`minimal

  let test_encode_bin_tree ~indexing_strategy =
    rm_dir root;
    Eio.Switch.run @@ fun sw ->
    let t = S.Context.get_store ~sw ~indexing_strategy () in
    let { S.Context.foo; bar; _ } = t in
    let v =
      S.Inode.Val.of_list
        [
          ("x", normal foo);
          ("z", normal foo);
          ("y", normal bar);
          ("w", normal bar);
          ("t", normal bar);
        ]
    in
    let h = S.Inter.Val.hash_exn v in
    let to_bin_string_hash =
      Irmin.Type.(unstage (to_bin_string S.Inode.Val.hash_t))
    in
    let hex_of_h = h |> to_bin_string_hash |> Hex.of_string |> Hex.show in
    let checks =
      [
        ("hash", hex_of_h);
        ("magic R", hex_encode "R");
        ("data length", "48");
        ("Tree", "01");
        ("depth", "00");
        ("nb of leaves", "05");
        ("length of entries", "02");
        ("index", "00");
        ("Direct", "01");
        ( "hash of entry",
          "8c81eb0a729858e10a8aed80f4ad638b26e80cf713be980a83620e22516001bf" );
        ("index", "01");
        ("Direct", "01");
        ( "hash of entry",
          "461a30b373e7d98e23dc963934a417d7c5aceb14fa2fb6da6950438fd54c9aa9" );
      ]
    in
    check_iter "encode_bin" (encode_bin h) v checks;
    S.Context.close t

  let test_encode_bin_tree () =
    let () = test_encode_bin_tree ~indexing_strategy:`always in
    test_encode_bin_tree ~indexing_strategy:`minimal
end

module Child_ordering = struct
  (** Tests of the relative ordering of Inode children (which can be configured
      by the user). *)

  module Step = struct
    type t = Schema.Path.step [@@deriving irmin ~short_hash]

    module Hash =
      Irmin.Hash.Typed
        (Schema.Hash)
        (struct
          type nonrec t = t

          let t = t
        end)

    type nonrec hash = Hash.t [@@deriving irmin ~to_bin_string]

    let hash : t -> string = fun s -> hash_to_bin_string (Hash.hash s)
  end

  module type S = Irmin_pack.Inode.Child_ordering with type step := Step.t

  let make ?entries:(entries' = Irmin_tezos.Conf.entries)
      (t : Irmin_pack.Conf.inode_child_order) : (module S) =
    let module Conf = struct
      include Irmin_tezos.Conf

      let entries = entries'
      let inode_child_order = t
    end in
    let module T = Inode_modules (Conf) (Schema) (String_contents) in
    (module T.Inter.Child_ordering)

  let check_child_index pos (module Order : S) ~expected ~step ~depth =
    let msg =
      Fmt.str "Short hash of child at { depth = %d; step = %S }" depth step
    in
    let actual = Order.key step |> Order.index ~depth in
    check_int pos ~msg ~expected actual

  let check_max_depth_exception pos (module Order : S) ~step ~depth =
    match Order.key step |> Order.index ~depth with
    | index ->
        Alcotest.failf ~pos
          "Expected [Max_depth %d] to be raised, but got a computed index of \
           %d instead"
          depth index
    | exception Irmin_pack.Inode.Max_depth _ -> ()

  (* Get the bit at index [n] in a string: *)
  let get_bit str n =
    let chosen_byte = Bytes.get_uint8 (Bytes.unsafe_of_string str) (n / 8) in
    let bit_index_in_byte = n mod 8 in
    (* Selects only the chosen bit from our byte: *)
    let mask = 1 lsl (7 - bit_index_in_byte) in
    let masked_byte = chosen_byte land mask in
    let chosen_bit = masked_byte lsr (7 - bit_index_in_byte) in
    assert (chosen_bit = 0 || chosen_bit = 1);
    chosen_bit

  let test_seeded_hash () =
    let entries = Irmin_tezos.Conf.entries in
    let reference ~depth step =
      abs (Step.short_hash ~seed:depth step) mod entries
    in
    let (module Order) = make `Seeded_hash in

    (* Test some hard-coded samples to ensure stablility: *)
    check_child_index __POS__ (module Order) ~expected:23 ~step:"a" ~depth:1;
    check_child_index __POS__ (module Order) ~expected:2 ~step:"b" ~depth:2;
    check_child_index __POS__ (module Order) ~expected:10 ~step:"foo" ~depth:42;

    (* Should match reference implementation for some random samples too: *)
    for _ = 1 to 1_000 do
      let step = random_string 8 and depth = Random.int 10 in
      let expected = reference ~depth step in
      check_child_index __POS__ (module Order) ~expected ~step ~depth
    done

  let hash_bits_max_depth ~log2_entries =
    (* For a given [depth], the final bit of the corresponding index is at
     * position [log2_entries * depth + log2_entries - 1] in the hash. If this
     * is out-of-bounds in the hash, then we expect computing the ordering to
     * fail (since we don't use modular indexing of the hash). *)
    let rec aux depth =
      if log2_entries * (depth + 1) > 8 * Hash.hash_size then depth - 1
      else aux (succ depth)
    in
    aux 0

  let test_hash_bits () =
    (* [entries] is required to be a power of 2 greater than 1 and less than
       2048, so we test every possible value here: *)
    for log2_entries = 1 to 10 do
      let entries = 1 lsl log2_entries in
      let max_depth = hash_bits_max_depth ~log2_entries in
      let (module Order) = make ~entries `Hash_bits in
      [%log.app
        "Testing hash_bits with { log_entries = %d; entries = %d; max_depth = \
         %d }"
        log2_entries entries max_depth];

      (* Index is computed by reading [log2_entries] consecutive bits from the
         hash of the step, starting at the [log2_entries * depth]-th byte. *)
      let reference ~depth step =
        let hash = Step.hash step in
        let index = ref 0 in
        for i = 0 to log2_entries - 1 do
          let selected_bit = get_bit hash ((log2_entries * depth) + i) in
          index := (!index lsl 1) lor selected_bit
        done;
        !index
      in

      for _ = 1 to 100 do
        let step = random_string 8 in
        (* We compute the valid index for this step at every depth up to
           [max_depth]: *)
        for depth = 0 to max_depth do
          let expected = reference ~depth step in
          check_child_index __POS__ (module Order) ~expected ~step ~depth
        done;
        (* Beyond [max_depth], the index computation should fail: *)
        check_max_depth_exception __POS__
          (module Order)
          ~step ~depth:(max_depth + 1)
      done
    done

  let test_custom () =
    let entries = 16 in
    let square_index ~depth step =
      let a = depth and b = int_of_string (Bytes.unsafe_to_string step) in
      a * b
    in
    let (module Order) = make ~entries (`Custom square_index) in
    check_child_index __POS__ (module Order) ~depth:1 ~step:"1" ~expected:1;
    check_child_index __POS__ (module Order) ~depth:2 ~step:"2" ~expected:4;
    check_child_index __POS__ (module Order) ~depth:3 ~step:"3" ~expected:9;
    ()
end

let tests =
  let tc_sync name f = Alcotest.test_case name `Quick f in
  let tc name f = tc_sync name f in
  (* Test disabled because it relies on being able to serialise concrete inodes,
     which is not possible following the introduction of structured keys. *)
  let _ = tc "test truncated inodes" test_truncated_inodes in
  let _ = tc "test encode bin of trees" Inode_tezos.test_encode_bin_tree in
  [
    tc "add values" test_add_values;
    tc "add values to inodes" test_add_inodes;
    tc "remove values" test_remove_values;
    tc "remove inodes" test_remove_inodes;
    tc "test concrete inodes" test_concrete_inodes;
    tc "test representation uniqueness"
      test_representation_uniqueness_maxdepth_3;
    tc "test encode bin of values" Inode_tezos.test_encode_bin_values;
    tc "test intermediate inode as root" test_intermediate_inode_as_root;
    tc_sync "Child_ordering.seeded_hash" Child_ordering.test_seeded_hash;
    tc_sync "Child_ordering.hash_bits" Child_ordering.test_hash_bits;
    tc_sync "Child_ordering.custom" Child_ordering.test_custom;
  ]
