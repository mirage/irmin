module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Commit = Irmin.Private.Commit.Make (Hash)

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Simple_Maker (V : Irmin_pack.VERSION) =
  Irmin_pack.Make_ext (V) (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Path)
    (Irmin.Branch.String)
    (Hash)
    (Node)
    (Commit)

module Store = Irmin_pack.Checks.Make (Simple_Maker)

module Store_layered =
  Irmin_pack_layered.Checks.Make
    (Simple_Maker)
    (Irmin_pack_layered.Make_ext (Conf) (Irmin.Metadata.None)
       (Irmin.Contents.String)
       (Path)
       (Irmin.Branch.String)
       (Hash)
       (Node)
       (Commit))

let () =
  match Sys.getenv_opt "PACK_LAYERED" with
  | Some "true" -> ( match Store_layered.cli () with _ -> .)
  | _ -> ( match Store.cli () with _ -> .)
