open Ctypes

type config = Irmin_unix.Resolver.Store.t * Irmin.config

type 'a repo = {
  mutable error : string option;
  repo_mod : (module Irmin.Generic_key.S with type repo = 'a);
  repo : 'a;
}

type 'a store = {
  repo : unit ptr;
  store_mod : (module Irmin.Generic_key.S with type t = 'a);
  store : 'a;
}

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
  type path_array
  type commit_array
  type branch_array
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
  val path_array : Struct.path_array ptr typ
  val commit_array : Struct.commit_array ptr typ
  val branch_array : Struct.branch_array ptr typ
  val commit_key : Struct.commit_key ptr typ
  val kinded_key : Struct.kinded_key ptr typ
end
