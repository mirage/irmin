module Hash = Irmin.Hash.SHA256
module Index = Irmin_pack_unix.Index.Make (Hash)
module Int63 = Optint.Int63
module Io = Irmin_pack_unix.Io.Unix
module Io_errors = Irmin_pack_unix.Io_errors.Make (Io)
module Upper_control = Irmin_pack_unix.Control_file.Upper (Io)

let src =
  Logs.Src.create "irmin-pack-tools" ~doc:"irmin-pack-tools last accessible"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { hash : string; off : Int63.t; len : int; kind : string }
[@@deriving irmin]

let get_payload root =
  let control_file = Irmin_pack.Layout.V5.control ~root in
  let r = Upper_control.read_payload ~path:control_file in
  match r with Error err -> Io_errors.raise_error err | Ok payload -> payload

let get_suffixes_sizes root
    (payload : Irmin_pack_unix.Control_file.Payload.Upper.V5.t) =
  let r = ref 0 in
  let last = ref 0 in
  for i = payload.chunk_start_idx to payload.chunk_num do
    let suffix = Irmin_pack.Layout.V5.suffix_chunk ~chunk_idx:i ~root in
    let stats = Unix.stat suffix in
    [%log.debug "found chunk at '%s' with size %d" suffix stats.st_size];
    r := !r + stats.st_size;
    last := stats.st_size
  done;
  [%log.debug "sum of found suffixes sizes: %d" !r];
  !r, !last

let get_status_infos (payload : Irmin_pack_unix.Control_file.Payload.Upper.V5.t)
    =
  match payload.status with
  | Gced gced ->
      [%log.debug
        "store status: gced, found suffix start offset at %a and %a dead bytes"
          Int63.pp gced.suffix_start_offset Int63.pp gced.suffix_dead_bytes];
      ( Int63.to_int gced.suffix_start_offset,
        Int63.to_int gced.suffix_dead_bytes )
  | _ -> (0, 0)

let get_last_accessible root off_max =
  let r = ref { hash = "n/a"; off = Int63.zero; len = 0; kind = "n/a" } in
  let off_max = Int63.of_int off_max in
  let f k v =
    let hash = Base64.encode_exn (Index.Key.encode k) in
    let off, len, kind = v in
    match (kind : Irmin_pack.Pack_value.Kind.t) with
    | (Commit_v1 | Commit_v2) when off > off_max ->
        [%log.warn
          "found commit with offset %a in index (higher than maximum \
           accessible offset)"
          Int63.pp off]
    | (Commit_v1 | Commit_v2) when Int63.add off (Int63.of_int len) > off_max ->
        [%log.warn
          "found commit with offset %a and length %d in index (too long to fit \
           in the maximum accessible offset)"
          Int63.pp off len]
    | (Commit_v1 | Commit_v2) when off >= !r.off ->
        let kind = Fmt.str "%a" Irmin_pack.Pack_value.Kind.pp kind in
        r := { hash; off; len; kind }
    | _ -> ()
  in
  let v = Index.v_exn ~readonly:true ~log_size:500_000 root in
  Index.iter f v;
  !r

let forge_new_payload (payload: Irmin_pack_unix.Control_file.Payload.Upper.V5.t) last_size off_max last_accessible =
  let open Int63.Infix in
  let appendable_chunk_poff =
    Int63.(of_int last_size - last_accessible.off - of_int last_accessible.len + of_int off_max)
  in
  (* Check if negativ ? *)
  Fmt.epr "new: %a@." Int63.pp appendable_chunk_poff;
  { payload with appendable_chunk_poff}

let main root_folder () =
  let payload = get_payload root_folder in
  let sizes, last_size = get_suffixes_sizes root_folder payload in
  let start_offset, dead_bytes = get_status_infos payload in
  let off_max = sizes + start_offset - dead_bytes in
  [%log.debug "last accessible offset: %d" off_max];
  let last_accessible = get_last_accessible root_folder off_max in
  Fmt.pr "%a@." (Irmin.Type.pp_json t) last_accessible;
  let pl = forge_new_payload payload last_size off_max last_accessible in
  Fmt.pr "%a@." (Irmin.Type.pp_json Irmin_pack_unix.Control_file.Payload.Upper.V5.t) pl;
  let _ = Result.get_ok (Upper_control.create_rw ~path:"foo.bar" ~overwrite:true pl) in
  ()

(** Cmdliner **)

open Cmdliner

let setup_log level =
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let root_folder =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"root folder" ~doc:"the path to the store")

let main_cmd =
  let doc =
    "gives the last accessible commit informations stored in the index of an \
     upper store"
  in
  let info = Cmd.info "irmin-last_accessible" ~doc in
  Cmd.v info
    Term.(const main $ root_folder $ (const setup_log $ Logs_cli.level ()))

let () = exit (Cmd.eval ~catch:false main_cmd)
