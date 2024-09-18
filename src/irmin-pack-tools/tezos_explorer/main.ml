open Cmdliner

(* Common arguments *)
let store_path =
  Arg.(
    value
    & opt string "."
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
let parse_cmd ~sw ~fs =
  let doc =
    "parses a pack file and generates the associated .info & .idx files"
  in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info
    Term.(
      const (Parse.main ~sw ~fs)
      $ store_path
      $ info_last_path
      $ info_next_path
      $ index_path)

(* Command show *)
let show_cmd ~sw ~fs =
  let doc = "graphical user interface for pack files inspection" in
  let info = Cmd.info "show" ~doc in
  Cmd.v info
    Term.(
      const (Show.main ~sw ~fs)
      $ store_path
      $ info_last_path
      $ info_next_path
      $ index_path)

(* Main command *)
let main_cmd ~sw ~fs =
  let doc = "a visual tool for irmin pack files inspection" in
  let info = Cmd.info "irmin-pack-inspect" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ parse_cmd ~sw ~fs; show_cmd ~sw ~fs ]

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  exit (Cmd.eval ~catch:false (main_cmd ~sw ~fs))
