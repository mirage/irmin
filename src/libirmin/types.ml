open Ctypes

type config = Irmin_unix.Resolver.Store.t * Irmin.config

let config = ptr (typedef void "IrminConfig")

type 'a repo = (module Irmin.Generic_key.S with type repo = 'a) * 'a

let repo = ptr (typedef void "IrminRepo")

type 'a store = (module Irmin.Generic_key.S with type t = 'a) * 'a

let store = ptr (typedef void "Irmin")
let ty = ptr (typedef void "IrminType")
let value = ptr (typedef void "IrminValue")
let metadata = ptr (typedef void "IrminMetadata")
let path = ptr (typedef void "IrminPath")
let tree = ptr (typedef void "IrminTree")
let commit = ptr (typedef void "IrminCommit")
let hash = ptr (typedef void "IrminHash")
let info = ptr (typedef void "IrminInfo")
let irmin_string = ptr (typedef void "IrminString")
let path_list = ptr (typedef void "IrminPathList")
let commit_list = ptr (typedef void "IrminCommitList")
let branch_list = ptr (typedef void "IrminBranchList")
let commit_key = ptr (typedef void "IrminCommitKey")
let kinded_key = ptr (typedef void "IrminKindedKey")
