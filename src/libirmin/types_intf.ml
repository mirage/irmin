open Ctypes

type config = Irmin_unix.Resolver.Store.t * Irmin.config
type 'a repo = (module Irmin.Generic_key.S with type repo = 'a) * 'a
type 'a store = (module Irmin.Generic_key.S with type t = 'a) * 'a

module type P = sig
  type config
  type repo
  type store
  type ty
  type value
  type metadata
  type contents
  type path
  type tree
  type commit
  type hash
  type info
  type irmin_string
  type path_list
  type commit_list
  type branch_list
  type commit_key
  type kinded_key
end

module type Sigs = sig
  module Struct : P

  type nonrec config = config
  type nonrec 'a repo = 'a repo
  type nonrec 'a store = 'a store

  val config : Struct.config ptr typ
  val repo : Struct.repo ptr typ
  val store : Struct.store ptr typ
  val ty : Struct.ty ptr typ
  val value : Struct.value ptr typ
  val metadata : Struct.metadata ptr typ
  val contents : Struct.contents ptr typ
  val path : Struct.path ptr typ
  val tree : Struct.tree ptr typ
  val commit : Struct.commit ptr typ
  val hash : Struct.hash ptr typ
  val info : Struct.info ptr typ
  val irmin_string : Struct.irmin_string ptr typ
  val path_list : Struct.path_list ptr typ
  val commit_list : Struct.commit_list ptr typ
  val branch_list : Struct.branch_list ptr typ
  val commit_key : Struct.commit_key ptr typ
  val kinded_key : Struct.kinded_key ptr typ
end
