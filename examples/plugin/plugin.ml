open Irmin_cli

(* Adding a new content type *)

module Int = struct
  type t = int

  let t = Irmin.Type.int
  let merge = Irmin.Merge.(option (idempotent t))
end

let () = Resolver.Contents.add ~default:true "int" (module Int)

module Schema = struct
  module Contents = Int
  module Hash = Irmin.Hash.BLAKE2B
  module Branch = Irmin.Branch.String
  module Path = Irmin.Path.String_list
  module Info = Irmin.Info.Default
  module Metadata = Irmin.Metadata.None
end

(* Adding a new store type *)

module Store = Irmin_mem.Make (Schema)

let store = Resolver.Store.v Irmin_mem.Conf.spec (module Store)
let () = Resolver.Store.add ~default:true "mem-int" (Fixed store)
