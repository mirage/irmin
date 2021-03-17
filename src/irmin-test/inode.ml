open! Import

let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let check = Common.check

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  val batch : read t -> ([ read | write ] t -> 'a Lwt.t) -> 'a Lwt.t

  module Key : Irmin.Hash.S with type t = key
  module Val : Irmin.Private.Inode.VAL with type t = value and type hash = key

  val close : 'a t -> unit Lwt.t
end

module type INODE_STORE = sig
  module H : Irmin.Hash.S
  module Metadata : Irmin.Metadata.S with type t = unit
  module Path : Irmin.Path.S with type step = string
  module Conf : Irmin.INODE_CONF

  module Inter :
    Irmin.Private.Inode.S
      with type hash = H.t
       and type step = string
       and type metadata = unit
       and type value = [ `Node of H.t | `Contents of H.t * unit ]

  module Inode :
    S
      with type key = H.t
       and type Val.step = string
       and type Val.metadata = unit
       and type Val.value = [ `Node of H.t | `Contents of H.t * unit ]
       and type value = Inter.t

  type t

  val v : unit -> t Lwt.t
  val close : t -> unit Lwt.t
  val store : t -> read Inode.t
  val clean : unit -> unit
end

module Test (Context : INODE_STORE) = struct
  module Inode = Context.Inode
  module Inter = Context.Inter
  module Path = Context.Path
  module Metadata = Context.Metadata
  module H = Context.H
  module Conf = Context.Conf

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
  let check_hash = check Inode.Val.hash_t
  let check_values = check Inode.Val.t

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
              |> List.for_all (fun (depth, i) -> Inter.index ~depth s = i)
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

    (** [steps t] is a list of length [entries ^ maxdepth_of_test] (8)
        containing the necessary steps to fill a tree of depth equal to
        [maxdepth_of_test] (3). *)
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
    let h = Inter.hash v in
    let+ h' = Inode.batch (Context.store t) (fun i -> Inode.add i v) in
    check_hash msg h h'

  let check_hardcoded_hash msg h v =
    h |> Irmin.Type.of_string Inode.Val.hash_t |> function
    | Error (`Msg str) -> Alcotest.failf "hash of string failed: %s" str
    | Ok hash -> check_hash msg hash (Inter.hash v)

  (** Test add values from an empty node. *)
  let test_add_values () =
    Context.clean ();
    let* t = Context.v () in
    check_node "hash empty node" Inode.Val.empty t >>= fun () ->
    let* v1 = Inode.Val.add Inode.Val.empty "x" (normal foo) in
    let* v2 = Inode.Val.add v1 "y" (normal bar) in
    check_node "node x+y" v2 t >>= fun () ->
    check_hardcoded_hash "hash v2" "d4b55db5d2d806283766354f0d7597d332156f74" v2;
    let v3 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
    check_values "add x+y vs v x+y" v2 v3;
    Context.close t

  let integrity_check ?(stable = true) v =
    Alcotest.(check bool) "check stable" (Inter.stable v) stable;
    let+ check = Inter.integrity_check v in
    if not check then
      Alcotest.failf "node does not satisfy stability invariants %a"
        (Irmin.Type.pp Inode.Val.t)
        v

  (** Test add to inodes. *)
  let test_add_inodes () =
    Context.clean ();
    let* t = Context.v () in
    let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
    let* v2 = Inode.Val.add v1 "z" (normal foo) in
    let v3 =
      Inode.Val.v [ ("x", normal foo); ("z", normal foo); ("y", normal bar) ]
    in
    check_values "add x+y+z vs v x+z+y" v2 v3;
    check_hardcoded_hash "hash v3" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v3;
    let* () = integrity_check v1 in
    let* () = integrity_check v2 in
    let* v4 = Inode.Val.add v2 "a" (normal foo) in
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
    let* () = integrity_check v4 ~stable:false in
    Context.close t

  (** Test remove values on an empty node. *)
  let test_remove_values () =
    Context.clean ();
    let* t = Context.v () in
    let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
    let* v2 = Inode.Val.remove v1 "y" in
    let v3 = Inode.Val.v [ ("x", normal foo) ] in
    check_values "node x obtained two ways" v2 v3;
    check_hardcoded_hash "hash v2" "a1996f4309ea31cc7ba2d4c81012885aa0e08789" v2;
    let* v4 = Inode.Val.remove v2 "x" in
    check_node "remove results in an empty node" Inode.Val.empty t >>= fun () ->
    let* v5 = Inode.Val.remove v4 "x" in
    check_values "remove on an already empty node" v4 v5;
    check_hardcoded_hash "hash v4" "5ba93c9db0cff93f52b521d7420e43f6eda2784f" v4;
    Alcotest.(check bool) "v5 is empty" (Inode.Val.is_empty v5) true;
    Context.close t

  (** Test remove and add values to go from stable to unstable inodes. *)
  let test_remove_inodes () =
    Context.clean ();
    let* t = Context.v () in
    let v1 =
      Inode.Val.v [ ("x", normal foo); ("y", normal bar); ("z", normal foo) ]
    in
    check_hardcoded_hash "hash v1" "46fe6c68a11a6ecd14cbe2d15519b6e5f3ba2864" v1;
    let* v2 = Inode.Val.remove v1 "x" in
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
    let* v5 = Inode.Val.remove v4 "a" in
    check_values "node x+y+z obtained two ways" v1 v5;
    let* () = integrity_check v1 in
    let* () = integrity_check v5 in
    Context.close t

  (** For each of the 256 possible inode trees with [depth <= 3] and
      [width = Conf.entries = 2] built by [Inode.Val.v], assert that
      independently, all the possible [Inode.Val.add]/[Inode.Val.remove]
      operations yield a tree computable by [Inode.Val.v].

      In other words. Let [T] be the set of all possible trees (256). Let [O] be
      the set of unitary [tree -> tree] operations (8). If all the combinations
      of [T] and [O] yield trees in [T] then, by induction, the representation
      is unique.

      Caveats

      If something breaks at [depth > 3 || entries <> 2], this won't be caught
      here.

      If a corrupted tree is constructed using [Elt.decode_bin] and
      [Val.of_bin], this won't be caught here.

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
        let+ tree'_new = Inode.Val.remove tree s in
        check_values
          "The representation of the received tree obtained through [remove] \
           differs from the expected one obtained through [v]."
          tree'_ref tree'_new
      else
        let steps' = P.StepSet.add s steps in
        let c = P.content_of_step p s in
        let tree'_ref = P.tree_of_steps p steps' in
        let+ tree'_new = Inode.Val.add tree s c in
        check_values
          "The representation of the received tree obtained through [remove] \
           differs from the expected one obtained through [v]."
          tree'_ref tree'_new
    in
    Lwt_list.iter_s
      (fun (ss, t) -> Lwt_list.iter_s (fun s -> f ss t s) (P.steps p))
      (P.trees p)

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
        "Iteration on that Truncated inode with broken pointers was expected \
         to fail."
        (Failure
           "Impossible to load the subtree on an inode deserialized using Repr")
        f
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
    let* v2 = Inode.Val.add v1 s01 (normal foo) in
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
    Context.clean ();
    let* t = Context.v () in
    let s000, s001, s010 =
      Inode_permutations_generator.
        (gen_step [ 0; 0; 0 ], gen_step [ 0; 0; 1 ], gen_step [ 0; 1; 0 ])
    in
    Log.debug (fun l -> l "XXX val.v");
    let v0 =
      Inode.Val.v [ (s000, normal foo); (s001, normal bar); (s010, normal foo) ]
    in
    let* h_depth0 =
      Inode.batch (Context.store t) @@ fun store -> Inode.add store v0
    in
    Log.debug (fun l -> l "XXX inode.batch");
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
      Inode.find (Context.store t) h_depth0 >|= function
      | None -> Alcotest.fail "Could not fetch inode from backend"
      | Some v -> v
    in
    let* ls = Inode.Val.list v in
    if ls |> List.length <> 3 then
      Alcotest.fail "Failed to list entries of loaded inode";
    let _ = Inode.Val.remove v s000 in
    let _ = Inode.Val.add v s000 (normal foo) in
    let* _ = Inode.batch (Context.store t) @@ fun store -> Inode.add store v in

    (* On inode with depth=1 *)
    let* v =
      Inode.find (Context.store t) h_depth1 >|= function
      | None -> Alcotest.fail "Could not fetch inode from backend"
      | Some v -> v
    in
    let* ls = Inode.Val.list v in
    if ls |> List.length <> 3 then
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
      Inode.batch (Context.store t) (fun store ->
          with_exn (fun () -> Inode.add store v);
          Lwt.return_unit)
    in
    Lwt.return_unit

  let test_concrete_inodes () =
    Context.clean ();
    let* t = Context.v () in
    let pp_concrete = Irmin.Type.pp_json ~minify:false Inter.Concrete.t in
    let result_t = Irmin.Type.result Inode.Val.t Inter.Concrete.error_t in
    let testable =
      Alcotest.testable
        (Irmin.Type.pp_json ~minify:false result_t)
        Irmin.Type.(unstage (equal result_t))
    in
    let check v =
      let len = Inter.length v in
      let* () = integrity_check ~stable:(len <= Conf.stable_hash) v in
      let* c = Inter.to_concrete v in
      let+ r = Inter.of_concrete c in
      let msg = Fmt.str "%a" pp_concrete c in
      Alcotest.check testable msg (Ok v) r
    in
    let v = Inode.Val.v [ ("a", normal foo) ] in
    let* () = check v in
    let v = Inode.Val.v [ ("a", normal foo); ("y", node bar) ] in
    let* () = check v in
    let v =
      Inode.Val.v [ ("x", node foo); ("a", normal foo); ("y", node bar) ]
    in
    let* () = check v in
    let v =
      Inode.Val.v
        [
          ("x", normal foo);
          ("z", normal foo);
          ("a", normal foo);
          ("y", node bar);
        ]
    in
    let* () = check v in
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
end
