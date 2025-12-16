module Sha256 = Irmin.Hash.SHA256
module Blake2B = Irmin_tezos.Schema.Hash

module type Type = Irmin_pack_unix.Index.S

type store = Tezos | Irmin

type t = { hash : string; off : Optint.Int63.t; len : int; kind : string }
[@@deriving irmin]

let main store_type root_folder =
  let module Index =
    (val match store_type with
         | Irmin -> (module Irmin_pack_unix.Index.Make (Sha256) : Type)
         | Tezos -> (module Irmin_pack_unix.Index.Make (Blake2B) : Type))
  in
  let v = Index.v_exn ~readonly:true ~log_size:500_000 root_folder in
  let dump_json k v =
    let hash = Irmin.Type.to_string Index.Key.t k in
    let off, len, kind = v in
    let kind = Fmt.str "%a" Irmin_pack.Pack_value.Kind.pp kind in
    let a = { hash; off; len; kind } in
    Fmt.pr "%a@." (Irmin.Type.pp_json t) a
  in
  Index.iter dump_json v

(** Cmdliner **)

open Cmdliner

let store_type =
  Arg.(
    required
    & pos 0 (some (enum [ ("Tezos", Tezos); ("Irmin", Irmin) ])) None
    & info [] ~docv:"store type" ~doc:"the type of store, either Irmin or Tezos")

let root_folder =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"root_folder" ~doc:"the path to the store")

let main_cmd =
  let doc = "a json printer for the informations stored in the index folder" in
  let info = Cmd.info "irmin-ppidx" ~doc in
  Cmd.v info Term.(const main $ store_type $ root_folder)

let () = exit (Cmd.eval ~catch:false main_cmd)
