open! Import
include module type of Irmin_pack.Pack_store

module Maker
    (Index : Pack_index.S)
    (Hash : Irmin.Hash.S with type t = Index.key) :
  Maker with type hash = Hash.t and type index := Index.t
