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

open Lwt
open Cmdliner

let global_option_section = "COMMON OPTIONS"

let pr_str = Format.pp_print_string

type source =
  [ `In_memory
  | `FS of string ]

let source_conv : source Arg.converter =
  let parse = function
    | ":" -> `Ok `In_memory
    | dir -> `Ok (`FS dir) in
  let print ppf s =
    let name = match s with
      | `In_memory -> ":"
      | `FS dir    -> dir in
    pr_str ppf name in
  parse, print

let value_conv (type v) (module S: Irmin.S with type value = v) =
  let parse str = `Ok (S.Value.of_bytes str) in
  let print ppf v = pr_str ppf (S.Value.dump v) in
  parse, print

let tag_conv (type t) (module S: Irmin.S with type tag = t) =
  let parse str = `Ok (S.Tag.of_string str) in
  let print ppf tag = pr_str ppf (S.Tag.to_string tag) in
  parse, print

let value s =
  let doc =
    Arg.info ~docv:"VALUE" ~doc:"Value to add." [] in
  Arg.(required & pos 0 (some & value_conv s) None & doc)

let store =
  let in_memory =
    let doc =
      Arg.info ~doc:"In-memory source. Equivalent to `--source :`" ["m";"in-memory"] in
    Arg.(value & flag & doc) in
  let source =
    let doc =
      Arg.info ~docv:"SOURCE" ~doc:"Store source." ["s";"source"] in
    Arg.(value & opt (some source_conv) None & doc) in
  let create in_memory source = match in_memory, source with
    | true, _
    | _   , Some `In_memory -> (module IrminMemory.Simple: Irmin.S)
    | _   , Some `FS dir    -> failwith "TODO"
    | _ -> failwith "No store source specified" in
  Term.(pure create $ in_memory $ source)

let run t =
  Lwt_unix.run (
    catch
      (fun () -> t)
      (function e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

(* INIT *)
let init_doc = "Initialize a store."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P init_doc;
  ] in
  let daemon =
    let doc =
      Arg.info ~docv:"PORT" ~doc:"Start an Irminsule server on the specified port."
        ["d";"daemon"] in
    Arg.(value & opt (some int) (Some 8080) & doc) in
  let init (module S: Irmin.S) daemon =
    run begin
      S.create () >>= fun t ->
      match daemon with
      | None      -> return_unit
      | Some port -> IrminHTTP.server (module S) t port
    end
  in
  Term.(pure init $ store $ daemon),
  Term.info "init" ~doc ~man

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
  ] in
  let usage _ =
    Printf.printf
      "usage: irmin [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\
      \n\
      The most commonly used irminsule commands are:\n\
      \    init    %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      init_doc in
  Term.(pure usage $ (pure ())),
  Term.info "irmin"
    ~version:IrminVersion.current
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  init;
]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
