open Lwt.Infix
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

module Context = struct
  type t = {
    index : Index.t;
    store : [ `Read ] Inode.t;
    clone : readonly:bool -> [ `Read ] Inode.t Lwt.t;
  }

  let get_store ?(lru_size = 0) () =
    rm_dir root;
    let index = Index.v ~log_size ~fresh:true root in
    Inode.v ~fresh:true ~lru_size ~index root >|= fun store ->
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

let check_node msg v t =
  let h = Inode.Val.hash v in
  Inode.batch t.Context.store (fun i -> Inode.add i v) >|= fun h' ->
  check_hash msg h h'

(** Test add values from an empty node. *)
let test_add_values () =
  rm_dir root;
  Context.get_store () >>= fun t ->
  check_node "hash empty node" Inode.Val.empty t >>= fun () ->
  let v1 = Inode.Val.add Inode.Val.empty "x" (normal foo) in
  let v2 = Inode.Val.add v1 "y" (normal bar) in
  check_node "node x y" v1 t >>= fun () ->
  let v3 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  check_values "add x+y vs v x+y" v2 v3;
  Context.close t

(** Test add to inodes. *)
let test_add_inodes () =
  rm_dir root;
  Context.get_store () >>= fun t ->
  let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.add v1 "z" (normal foo) in
  let v3 =
    Inode.Val.v [ ("x", normal foo); ("z", normal foo); ("y", normal bar) ]
  in
  check_values "add x+y+z vs v x+z+y" v2 v3;
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
  Context.close t

(** Test remove values on an empty node. *)
let test_remove_values () =
  rm_dir root;
  Context.get_store () >>= fun t ->
  let v1 = Inode.Val.v [ ("x", normal foo); ("y", normal bar) ] in
  let v2 = Inode.Val.remove v1 "y" in
  let v3 = Inode.Val.v [ ("x", normal foo) ] in
  check_values "node x obtained two ways" v2 v3;
  let v4 = Inode.Val.remove v2 "x" in
  check_node "remove results in an empty node" Inode.Val.empty t >>= fun () ->
  let v5 = Inode.Val.remove v4 "x" in
  check_values "remove on an already empty node" v4 v5;
  Alcotest.(check bool) "v5 is empty" (Inode.Val.is_empty v5) true;
  Context.close t

(** Test remove and add values to go from stable to unstable inodes. *)
let test_remove_inodes () =
  rm_dir root;
  Context.get_store () >>= fun t ->
  let v1 =
    Inode.Val.v [ ("x", normal foo); ("y", normal bar); ("z", normal foo) ]
  in
  let v2 = Inode.Val.remove v1 "x" in
  let v3 = Inode.Val.v [ ("y", normal bar); ("z", normal foo) ] in
  check_values "node y+z obtained two ways" v2 v3;
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
  ]
