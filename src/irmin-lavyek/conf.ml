module Default = struct
  let fresh = false
  let sync = false
end

open Irmin.Backend.Conf

let sw_typ : Eio.Switch.t Typ.t = Typ.create ()
let fs_typ : Eio.Fs.dir_ty Eio.Path.t Typ.t = Typ.create ()
let spec = Spec.v "lavyek"

module Key = struct
  let fresh =
    key ~spec ~doc:"Start with a fresh disk." "fresh" Irmin.Type.bool
      Default.fresh

  let root = root spec

  let sync =
    key ~spec ~doc:"Whether lavyek should sync additions on disk" "use-sync"
      Irmin.Type.bool Default.sync
end

let fresh config = get config Key.fresh

let root config =
  match find_root config with
  | None ->
      failwith
        "unintialised root, call [Irmin_pack.Conf.init root] before opening \
         the store"
  | Some root -> root

let sync config = get config Key.sync
let switch config = find_key config "sw" sw_typ
let fs config = find_key config "fs" fs_typ

let spec ~sw ~fs =
  let spec = Spec.copy spec in
  let _sw_key =
    let to_string _ = "Eio.Switch.t" in
    let of_string _ = Ok sw in
    let of_json_string _ = Ok sw in
    serialized_key ~typ:sw_typ ~spec ~typename:"Eio.Switch.t" ~to_string
      ~of_string ~of_json_string "sw" sw
  in
  let fs = (fs :> Eio.Fs.dir_ty Eio.Path.t) in
  let _fs_key =
    let to_string fs = Eio.Path.native_exn fs in
    let of_string str = Ok Eio.Path.(fs / str) in
    let of_json_string str =
      match Irmin.Type.(of_json_string string) str with
      | Ok str -> Ok Eio.Path.(fs / str)
      | Error e -> Error e
    in
    serialized_key ~typ:fs_typ ~spec ~typename:"_ Eio.Path.t" ~to_string
      ~of_string ~of_json_string "fs" fs
  in
  spec

let init ~sw ~fs ?(fresh = Default.fresh) ?(sync = Default.sync) root =
  let root = Eio.Path.native_exn root in
  let config = empty (spec ~sw ~fs) in
  let config = add config Key.root root in
  let config = add config Key.fresh fresh in
  let config = add config Key.sync sync in
  verify config
