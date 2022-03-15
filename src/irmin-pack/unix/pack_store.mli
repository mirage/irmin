open! Import
(* include module type of Irmin_pack.Pack_store *)

val selected_version : Version.t

module Maker
    (Index : Pack_index.S)
    (Hash : Irmin.Hash.S with type t = Index.key) :
  Irmin_pack.Pack_store_intf.Maker with type hash = Hash.t and type index := Index.t
