include Commit_intf

module Make (St : Irmin.Generic_key.S) (T : Tree.S) = struct
  type tree = T.t
  type key = St.commit_key

  let tree_t = T.t

  type hash = St.Hash.t

  let hash_t = St.Hash.t
  let key_t = St.commit_key_t

  module Info = St.Info

  type t = { info : Info.t; parents : key list; key : key; tree : T.t }
  [@@deriving irmin]

  let info { info; _ } = info
  let key { key; _ } = key
  let parents { parents; _ } = parents
  let tree { tree; _ } = tree
  let v ~info ~parents ~key ~tree = { info; parents; key; tree }
end
