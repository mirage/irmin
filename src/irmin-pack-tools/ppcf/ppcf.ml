module Io = Irmin_pack_unix.Io.Unix
module Io_errors = Irmin_pack_unix.Io_errors.Make (Io)
module Upper_control = Irmin_pack_unix.Control_file.Upper (Io)
module Volume_control = Irmin_pack_unix.Control_file.Volume (Io)

type store_type = Upper | Volume

let print_cf ~sw ~fs read print control_file =
  let r = read ~sw ~path:Eio.Path.(fs / control_file) in
  match r with
  | Error err -> Io_errors.raise_error err
  | Ok payload -> Fmt.pr "%a\n" (Irmin.Type.pp_json print) payload

let main = function
  | Upper ->
      print_cf Upper_control.read_raw_payload
        Irmin_pack_unix.Control_file.Payload.Upper.raw_payload_t
  | Volume ->
      print_cf Volume_control.read_raw_payload
        Irmin_pack_unix.Control_file.Payload.Volume.raw_payload_t

(** Cmdliner **)

open Cmdliner

let store_type =
  Arg.(
    required
    & pos 0 (some (enum [ ("Upper", Upper); ("Volume", Volume) ])) None
    & info [] ~docv:"store type"
        ~doc:"the type of control file, either Upper or Volume")

let control_file =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"control file" ~doc:"the path to the control file")

let main_cmd ~sw ~fs =
  let doc = "a json printer for irmin pack control files" in
  let info = Cmd.info "irmin-ppcf" ~doc in
  Cmd.v info Term.(const (main ~sw ~fs) $ store_type $ control_file)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  exit (Cmd.eval ~catch:false (main_cmd ~sw ~fs))
