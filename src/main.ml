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

let src_flag =
  mk_opt ["i";"input";"s";"src"] "SRC"
    "Select source irminsule instance."
    Arg.int 8081

let dst_flag =
  mk_opt ["o";"output";"d";"dst"] "DST"
    "Select the destination irminsule instance."
    Arg.int 8081

(* INIT *)
let init_doc = "Initialize a queue."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P init_doc;
  ] in
  Term.(pure Irminsule_server.init $ port_flag),
  term_info "init" ~doc ~man

(* ADD *)
let add_doc = "Add an element at the end of the queue."
let add =
  let doc = add_doc in
  let man = [
    `S "DESCRIPTION";
    `P add_doc;
  ] in
  let values =
    arg_list "VALUES" "Values to add the the distributed queue." Arg.string in
  Term.(pure Irminsule_client.add $ port_flag $values),
  term_info "add" ~doc ~man

(* WATCH *)
let watch_doc = "Watch a queue."
let watch =
  let doc = watch_doc in
  let man = [
    `S "DESCRIPTION";
    `P watch_doc;
  ] in
  Term.(pure Irminsule_client.watch $ port_flag),
  term_info "watch" ~doc ~man

(* TAKE *)
let take_doc = "Removes and returns the first element in the queue."
let take =
  let doc = take_doc in
  let man = [
    `S "DESCRIPTION";
    `P take_doc;
  ] in
  Term.(pure Irminsule_client.take $ port_flag),
  term_info "take" ~doc ~man

(* PEEK *)
let peek_doc = "Returns the first element in the queue, without removing it from \
                the queue."
let peek =
  let doc = peek_doc in
  let man = [
    `S "DESCRIPTION";
    `P peek_doc;
  ] in
  Term.(pure Irminsule_client.peek $ port_flag),
  term_info "peek" ~doc ~man

(* DUMP *)
let dump_doc = "Dump the queue contents."
let dump =
  let doc = dump_doc in
  let man = [
    `S "DESCRIPTION";
    `P dump_doc;
  ] in
  Term.(pure Irminsule_client.dump $ port_flag),
  term_info "dump" ~doc ~man

(* PULL *)
let pull_doc = "Pull changes between queues."
let pull =
  let doc = pull_doc in
  let man = [
    `S "DESCRIPTION";
    `P pull_doc;
  ] in
  Term.(pure Irminsule_client.pull $ src_flag $ dst_flag),
  term_info "pull" ~doc ~man

(* PUSH *)
let push_doc = "Push changes between queues."
let push =
  let doc = push_doc in
  let man = [
    `S "DESCRIPTION";
    `P push_doc;
  ] in
  Term.(pure Irminsule_client.push $ src_flag $ dst_flag),
  term_info "push" ~doc ~man

(* CLONE *)
let clone_doc = "Clone an existing queue."
let clone =
  let doc = clone_doc in
  let man = [
    `S "DESCRIPTION";
    `P clone_doc;
  ] in
  Term.(pure Irminsule_client.clone $ src_flag $ dst_flag),
  term_info "clone" ~doc ~man

(* HELP *)
let help =
  let doc = "Display help about Irminsule and Irminsule commands." in
  let man = [
    `S "DESCRIPTION";
     `P "Prints help about Irminsule commands.";
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
  let doc = "Irminsule, the database that never forgets." in
  let man = [
    `S "DESCRIPTION";
    `P "TODO";
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: irmin [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\
      \n\
      The most commonly used irminsule commands are:\n\
      \    init    %s\n\
      \    add     %s\n\
      \    take    %s\n\
      \    peek    %s\n\
      \    watch   %s\n\
      \    dump    %s\n\
      \    clone   %s\n\
      \    push    %s\n\
      \    pull    %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      init_doc add_doc take_doc peek_doc watch_doc dump_doc clone_doc push_doc
      pull_doc in
  Term.(pure usage $ (pure ())),
  Term.info "irmin"
    ~version:Version.current
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  init;
  add;
  take;
  peek;
  watch;
  dump;
  clone;
  push;
  pull;
]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
