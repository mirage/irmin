module Hash = Irmin.Hash.SHA256
module Index = Irmin_pack_unix.Index.Make (Hash)

type hash = { base64 : string; base58 : string }
[@@deriving irmin]

type t = { hash : hash; off : Optint.Int63.t; len : int; kind : string }
[@@deriving irmin]

type alphabet = Bitcoin | Ripple | Flickr

let make_alphabet p c =
  match p, c with
  | _, Some s -> B58.make_alphabet s
  | Bitcoin, _ -> B58.make_alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  | Ripple, _ -> B58.make_alphabet "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
  | Flickr, _ -> B58.make_alphabet "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

let dump_json alphabet k v =
  let key = Index.Key.encode k in
  let base64 = Base64.encode_exn key in
  let base58 = Bytes.to_string @@ B58.encode alphabet (Bytes.of_string key) in
  let hash = { base64; base58 } in
  let off, len, kind = v in
  let kind = Fmt.str "%a" Irmin_pack.Pack_value.Kind.pp kind in
  let a = { hash; off; len; kind } in
  Fmt.pr "%a@." (Irmin.Type.pp_json t) a

let main root_folder alphabet =
  let v = Index.v_exn ~readonly:true ~log_size:500_000 root_folder in
  Index.iter (dump_json alphabet) v

(** Cmdliner **)

open Cmdliner

let root_folder =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"root_folder" ~doc:"the path to the store")

let predefined_alphabet =
  Arg.(
    value
    & opt (enum [ ("bitcoin", Bitcoin); ("ripple", Ripple); ("flickr", Flickr) ]) Bitcoin
    & info [ "p" ] ~docv:"pre-defined_base58_alphabet"
        ~doc:"the pre-defined alphabet used for the base58 encoding, default to Bitcoin, ignored if '-c' is set")

let custom_alphabet =
  Arg.(
    value
    & opt (some string) None
    & info [ "c" ]
        ~doc:
          "the path to the info file generated for next entries data, default \
           to `store.info.next`")

let main_cmd =
  let doc = "a json printer for the informations stored in the index folder" in
  let info = Cmd.info "irmin-ppidx" ~doc in
  Cmd.v info Term.(const main $ root_folder $ (const make_alphabet $ predefined_alphabet $ custom_alphabet))

let () = exit (Cmd.eval ~catch:false main_cmd)
