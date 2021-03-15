open! Import
open Common

let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Inode_pack : Irmin_test.INODE_STORE = struct
  module H = H
  module Path = Irmin.Path.String_list
  module Metadata = Irmin.Metadata.None

  module Conf = struct
    let entries = 2
    let stable_hash = 3
  end

  module Node = Irmin.Private.Node.Make (H) (Path) (Metadata)
  module Index = Irmin_pack.Index.Make (H)
  module Inter = Irmin_pack.Inode.Make_intermediate (Conf) (H) (Node)
  module Inode = Irmin_pack.Inode.Make_ext (H) (Inter) (P)

  type t = { index : Index.t; store : read Inode.t }

  let log_size = 1000
  let root = Filename.concat "_build" "test-inode"

  let v () =
    rm_dir root;
    let index = Index.v ~log_size ~fresh:true root in
    let+ store = Inode.v ~fresh:true ~index root in
    { index; store }

  let close t =
    Index.close t.index;
    Inode.close t.store

  let store t = t.store
  let clean () = rm_dir root
end
