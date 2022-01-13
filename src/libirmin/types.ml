open Ctypes
include Types_intf

module Struct = struct
  type config = unit
  type repo = unit
  type store = unit
  type ty = unit
  type value = unit
  type metadata = unit
  type contents = unit
  type path = unit
  type tree = unit
  type commit = unit
  type hash = unit
  type info = unit
  type irmin_string = unit
  type path_list = unit
  type commit_list = unit
  type branch_list = unit
  type commit_key = unit
  type kinded_key = unit
end

let config : Struct.config ptr typ = ptr (typedef void "IrminConfig")
let repo : Struct.repo ptr typ = ptr (typedef void "IrminRepo")
let store : Struct.store ptr typ = ptr (typedef void "Irmin")
let ty : Struct.ty ptr typ = ptr (typedef void "IrminType")
let value : Struct.value ptr typ = ptr (typedef void "IrminValue")
let metadata : Struct.metadata ptr typ = ptr (typedef void "IrminMetadata")
let contents : Struct.metadata ptr typ = ptr (typedef void "IrminContents")
let path : Struct.path ptr typ = ptr (typedef void "IrminPath")
let tree : Struct.tree ptr typ = ptr (typedef void "IrminTree")
let commit : Struct.commit ptr typ = ptr (typedef void "IrminCommit")
let hash : Struct.hash ptr typ = ptr (typedef void "IrminHash")
let info : Struct.info ptr typ = ptr (typedef void "IrminInfo")

let irmin_string : Struct.irmin_string ptr typ =
  ptr (typedef void "IrminString")

let path_list : Struct.path_list ptr typ = ptr (typedef void "IrminPathList")

let commit_list : Struct.commit_list ptr typ =
  ptr (typedef void "IrminCommitList")

let branch_list : Struct.branch_list ptr typ =
  ptr (typedef void "IrminBranchList")

let commit_key : Struct.commit_key ptr typ = ptr (typedef void "IrminCommitKey")
let kinded_key : Struct.kinded_key ptr typ = ptr (typedef void "IrminKindedKey")
