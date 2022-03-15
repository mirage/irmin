open Import
include Irmin_pack.Inode

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
    (CA : Irmin_pack.Pack_store_intf.Maker
            with type hash = H.t
             and type index := Pack_index.Make(H).t) =
struct
  module Raw = Inter.Raw

  let to_snapshot = Inter.to_snapshot

  module Persistent_pack = CA.Make (Inter.Raw)
  module Pack = Persistent_pack
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
  let sync = Pack.sync
  let integrity_check = Pack.integrity_check
  let clear_caches = Pack.clear_caches
end
