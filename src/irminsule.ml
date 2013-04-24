(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cmdliner
open Lib

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
]

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt conv default & doc)

let mk_required ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(required & opt conv default & doc)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(non_empty & pos_all conv [] & doc)

let port_flag =
  mk_opt ["p";"port"] "PORT"
    "Select the port on which the irminsule webserver is listening. Default is 8081."
    Arg.int 8081

(* START *)
let start_doc = "Start an irminsule daemon."
let start =
  let doc = start_doc in
  let man = [
    `S "DESCRIPTION";
    `P start_doc;
  ] in
  let path =
    arg_list "PATH" "Local path this instance of irminsule is monitoring." Arg.string in
  let exclude =
    mk_opt ["x";"exclude"] "DIRS"
      "Files to ignore"
      Arg.(list string) [] in
  Term.(pure Lwt_server.start $ path $ port_flag $ exclude),
  term_info "start" ~doc ~man

(* COMMIT *)
let commit_doc = "Commit the current state of the tree."
let commit =
  let doc = commit_doc in
  let man = [
    `S "DESCRIPTION";
    `P commit_doc;
  ] in
  Term.(pure Lwt_client.commit $ port_flag),
  term_info "commit" ~doc ~man

(* HELP *)
let help =
  let doc = "Display help about Mirari and Mirari commands." in
  let man = [
    `S "DESCRIPTION";
     `P "Prints help about Mirari commands.";
     `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e                -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
      | `Ok t                   -> `Help (man_format, Some t) in

  Term.(ret (pure help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default =
  let doc = "Mirage application builder" in
  let man = [
    `S "DESCRIPTION";
    `P "TODO";
    `P "Use either $(b,irminsule <command> --help) or $(b,irminsule help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: irminsule [--version]\n\
      \                 [--help]\n\
      \                 <command> [<args>]\n\
      \n\
      The most commonly used irminsule commands are:\n\
      \    start       %s\n\
      \    commit      %s\n\
      \n\
      See 'irminsule help <command>' for more information on a specific command.\n%!"
      start_doc commit_doc in
  Term.(pure usage $ (pure ())),
  Term.info "irminsule"
    ~version:(Path_generated.project_version)
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  start;
  commit;
  help;
]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
