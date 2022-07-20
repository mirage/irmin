module type S = sig
  type hash
  type tree
  type key

  val tree_t : tree Irmin.Type.t
  val hash_t : hash Irmin.Type.t
  val key_t : key Irmin.Type.t

  module Info : Irmin.Info.S

  type t = { info : Info.t; parents : key list; key : key; tree : tree }
  [@@deriving irmin]

  val info : t -> Info.t
  val key : t -> key
  val parents : t -> key list
  val tree : t -> tree
  val v : info:Info.t -> parents:key list -> key:key -> tree:tree -> t
end

module type Commit = sig
  module type S = S

  module Make (S : Irmin.Generic_key.S) (T : Tree.S) :
    S
      with type hash = S.Hash.t
       and type tree = T.t
       and type key = S.commit_key
       and module Info = S.Info
end
