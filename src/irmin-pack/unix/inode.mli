open Import
open Irmin_pack.Inode

module type S = S

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
             and type index := Pack_index.Make(H).t) :
  Persistent
    with type key = H.t Pack_key.t
     and type hash = H.t
     and type Val.metadata = Node.metadata
     and type Val.step = Node.step
     and type index := Pack_index.Make(H).t
     and type value = Inter.Val.t
