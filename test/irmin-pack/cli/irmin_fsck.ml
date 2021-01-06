module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Commit = Irmin.Private.Commit.Make (Hash)

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module Store = Irmin_pack.Checks.Make (struct
  module Hash = Hash
  module Store =
    Irmin_pack.Make_ext (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Path)
      (Irmin.Branch.String)
      (Hash)
      (Node)
      (Commit)
end)

let () = match Store.cli () with _ -> .
