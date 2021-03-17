open! Import

let src = Logs.Src.create "tests.instances" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Inode_mem : Irmin_test.INODE_STORE = struct
  module H = Irmin.Hash.SHA1
  module Path = Irmin.Path.String_list
  module Metadata = Irmin.Metadata.None

  module Conf = struct
    let entries = 2
    let stable_hash = 3
  end

  module Node = Irmin.Private.Node.Make (H) (Path) (Metadata)
  module Inter = Irmin.Private.Inode.Make (Conf) (H) (Node)
  module CA = Irmin.Content_addressable (Irmin_mem.Append_only)
  module Inode = Irmin.Private.Inode.Raw_store (CA) (H) (Inter)

  type t = { store : read Inode.t }

  let v () =
    let config = Irmin_mem.config () in
    let+ store = Inode.v config in
    { store }

  let close t = Inode.close t.store
  let store t = t.store
  let clean () = ()
end
