open Cmdliner

(* Common arguments *)
let eio_path root =
  let parse s = Ok Eio.Path.(root / s) in
  let print = Eio.Path.pp in
  Arg.conv ~docv:"PATH" (parse, print)

let store_path fs =
  Arg.(
    value
    & opt (eio_path fs) fs
    & info [ "store_path" ]
        ~doc:"the path to the irmin store files, default to `.`")

let info_last_path =
  Arg.(
    value
    & opt string "store.info.last"
    & info [ "store_info_last" ]
        ~doc:
          "the path to the info file generated for last entries data, default \
           to `store.info.last`")

let info_next_path =
  Arg.(
    value
    & opt string "store.info.next"
    & info [ "store_info_next" ]
        ~doc:
          "the path to the info file generated for next entries data, default \
           to `store.info.next`")

let index_path =
  Arg.(
    value
    & opt string "store.idx"
    & info [ "store_idx" ]
        ~doc:"the path to the index file generated, default to `store.index`")

(* Command parse *)
let parse_cmd env fs =
  let doc =
    "parses a pack file and generates the associated .info & .idx files"
  in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info
    Term.(
      const (Parse.main env)
      $ store_path fs
      $ info_last_path
      $ info_next_path
      $ index_path)

(* Command show *)
let show_cmd env fs =
  let doc = "graphical user interface for pack files inspection" in
  let info = Cmd.info "show" ~doc in
  Cmd.v info
    Term.(
      const (Show.main env)
      $ store_path fs
      $ info_last_path
      $ info_next_path
      $ index_path)

(* Main command *)
let main_cmd =
  let doc = "a visual tool for irmin pack files inspection" in
  let info = Cmd.info "irmin-pack-inspect" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Cmd.group info ~default [ parse_cmd env fs; show_cmd env fs ]

let () = exit (Cmd.eval ~catch:false main_cmd)
