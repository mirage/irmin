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
module Inode = Irmin_pack.Inode.Make (Conf) (H) (P) (Node)
module Private = Inode.Val.Private

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

module H_contents =
  Irmin.Hash.Typed
    (H)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

let normal x = `Contents (x, Metadata.default)
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
      [Private.index ~depth:i] maps to the ith index in the [index_list]. *)
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
            |> List.for_all (fun (depth, i) -> Private.index ~depth s = i)
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
  let h = Private.hash v in
  let+ h' = Inode.batch t.Context.store (fun i -> Inode.add i v) in
  check_hash msg h h'

let check_hardcoded_hash msg h v =
  h |> Irmin.Type.of_string Inode.Val.hash_t |> function
  | Error (`Msg str) -> Alcotest.failf "hash of string failed: %s" str
  | Ok hash -> check_hash msg hash (Private.hash v)

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
  Alcotest.(check bool) "v1 stable" (Private.stable v1) true;
  Alcotest.(check bool) "v2 stable" (Private.stable v2) true;
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
  Alcotest.(check bool) "v4 not stable" (Private.stable v4) false;
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
  Alcotest.(check bool) "v1 stable" (Private.stable v1) true;
  Alcotest.(check bool) "v5 stable" (Private.stable v5) true;
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
    Alcotest.test_case "test representation uniqueness" `Quick (fun () ->
        Lwt_main.run (test_representation_uniqueness_maxdepth_3 ()));
  ]
