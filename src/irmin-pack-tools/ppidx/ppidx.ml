module Hash = Irmin.Hash.SHA256
module Index = Irmin_pack_unix.Index.Make (Hash)

type t = { hash : string; off : Optint.Int63.t; len : int; kind : string }
[@@deriving irmin]

let dump_json k v =
  let hash = Base64.encode_exn (Index.Key.encode k) in
  let off, len, kind = v in
  let kind = Fmt.str "%a" Irmin_pack.Pack_value.Kind.pp kind in
  let a = { hash; off; len; kind } in
  Fmt.pr "%a@." (Irmin.Type.pp_json t) a

let main root_folder =
  let v = Index.v_exn ~readonly:true ~log_size:500_000 root_folder in
  Index.iter dump_json v

(** Cmdliner **)

open Cmdliner

let root_folder =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"root folder" ~doc:"the path to the store")

let main_cmd =
  let doc = "a json printer for the informations stored in the index folder" in
  let info = Cmd.info "irmin-ppidx" ~doc in
  Cmd.v info Term.(const main $ root_folder)

let () = exit (Cmd.eval ~catch:false main_cmd)
