open Import
include Irmin_pack.Inode
include Inode_intf

module Make_persistent
    (H : Irmin.Hash.S)
    (Node : Irmin.Node.Generic_key.S
              with type hash = H.t
               and type contents_key = H.t Pack_key.t
               and type node_key = H.t Pack_key.t)
    (Inter : Internal
               with type hash = H.t
                and type key = H.t Pack_key.t
                and type Snapshot.metadata = Node.metadata
                and type Val.step = Node.step)
    (Pack : Pack_store.S
              with type hash = H.t
               and type key = H.t Pack_key.t
               and type value = Inter.Raw.t) =
struct
  module Raw = Inter.Raw
  module Pack = Pack

  type file_manager = Pack.file_manager
  type dict = Pack.dict

  let to_snapshot = Inter.to_snapshot

  module XKey = Pack_key.Make (H)
  include Make (H) (XKey) (Node) (Inter) (Pack)
  module Snapshot = Inter.Snapshot

  let of_snapshot t ~index v =
    let find ~expected_depth:_ k =
      let v = Pack.unsafe_find ~check_integrity:true t k in
      v
    in
    Inter.Val.of_snapshot ~index v find

  let v = Pack.v
  let integrity_check = Pack.integrity_check
  let purge_lru = Pack.purge_lru
end
