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

(* Global options *)
type global = {
  verbose: bool;
}

let app_global g =
  IrminLog.set_debug_mode g.verbose

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/irminsule/issues.";
]

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be more verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  Term.(pure (fun verbose -> { verbose }) $ verbose)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

(* Converters *)

let pr_str = Format.pp_print_string

let uri_conv =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf v = pr_str ppf (Uri.to_string v) in
  parse, print

let value_conv =
  let parse str = `Ok (IrminValue.Simple.of_bytes str) in
  let print ppf v = pr_str ppf (IrminValue.Simple.to_string v) in
  parse, print

let tag_conv =
  let parse str = `Ok (IrminTag.Simple.of_string str) in
  let print ppf tag = pr_str ppf (IrminTag.Simple.to_string tag) in
  parse, print

let path_conv =
  let parse str = `Ok (IrminTree.Path.of_pretty str) in
  let print ppf path = pr_str ppf (IrminTree.Path.pretty path) in
  parse, print

let value =
  let doc =
    Arg.info ~docv:"VALUE" ~doc:"Value to add." [] in
  Arg.(required & pos ~rev:true 0 (some & value_conv) None & doc)

let path =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Path." [] in
  Arg.(value & pos 0 path_conv [] & doc)

let default_dir = ".irmin"

(* XXX: ugly hack *)
let init_hook =
  ref (fun () -> ())

let local_store dir =
  IrminLog.msg "source: dir=%s" dir;
  init_hook := (fun () -> if not (Sys.file_exists dir) then Unix.mkdir dir 0o755);
  IrminFS.simple dir

let remote_store uri =
  IrminLog.msg "source: uri=%s" (Uri.to_string uri);
  IrminCRUD.simple uri

let store =
  let in_memory =
    let doc =
      Arg.info ~doc:"In-memory persistence."
        ["m";"in-memory"] in
    Arg.(value & flag & doc) in
  let fs =
    let doc =
      Arg.info ~doc:"File-system persistence." ["f";"file"] in
    Arg.(value & flag & doc) in
  let crud =
    let doc =
      Arg.info ~doc:"CRUD interface."  ["c";"crud"] in
    Arg.(value & flag & doc) in
  let uri =
    let doc = Arg.info ~doc:"Irminsule store location." [] in
    Arg.(value & pos 0 (some string) None & doc) in
  let create in_memory fs crud uri = match in_memory, fs, crud with
    | true , false, false ->
      if uri <> None then IrminLog.error "Non-empty URI, skipping it";
      IrminLog.msg "source: in-memory";
      (module IrminMemory.Simple: Irmin.SIMPLE)
    | false, false, true ->
      let uri = match uri with
        | None   -> Uri.of_string "http://127.0.0.1:8080"
        | Some u -> Uri.of_string u in
      remote_store uri
    | false, true , false ->
      let dir = match uri with
        | None   -> Filename.concat (Sys.getcwd ()) default_dir
        | Some d -> Filename.concat d default_dir in
      local_store dir
    | false, false, false ->
      (* try to guess the correct type *)
      begin match uri with
        | None   -> local_store default_dir
        | Some s ->
          if Sys.file_exists s then local_store s
          else remote_store (Uri.of_string s)
      end
    | _ -> failwith
             (Printf.sprintf "Invalid store source [%b %b %b %s]"
                in_memory fs crud (match uri with None -> "<none>" | Some s -> s))
  in
  Term.(pure create $ in_memory $ fs $ crud $ uri)

let run t =
  Lwt_unix.run (
    catch
      (fun () -> t)
      (function e -> Printf.eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun global -> app_global global; fn) $ global)

(* INIT *)
let init = {
  name = "init";
  doc  = "Initialize a store.";
  man  = [];
  term =
    let daemon =
      let doc =
        Arg.info ~docv:"PORT" ~doc:"Start an Irminsule server on the specified port."
          ["d";"daemon"] in
      Arg.(value & opt (some uri_conv) (Some (Uri.of_string "http://127.0.0.1:8080")) & doc) in
    let init (module S: Irmin.SIMPLE) daemon =
      run begin
        S.create () >>= fun t ->
        !init_hook ();
        match daemon with
        | None     -> return_unit
        | Some uri ->
          IrminLog.msg "daemon: %s" (Uri.to_string uri);
          IrminHTTP.start_server (module S) t uri
      end
    in
    Term.(mk init $ store $ daemon)
}


(* READ *)
let read = {
  name = "read";
  doc  = "Read the contents of a node.";
  man  = [];
  term =
    let read path =
      let (module S) = local_store default_dir in
      run begin
        S.create ()   >>= fun t ->
        S.read t path >>= function
        | None   -> IrminLog.msg "<none>"; exit 1
        | Some v -> IrminLog.msg "%s" (S.Value.pretty v); return_unit
      end
    in
    Term.(mk read $ path);
}

(* LS *)
let ls = {
  name = "ls";
  doc  = "List subdirectories.";
  man  = [];
  term =
    let ls path =
      let (module S) = local_store default_dir in
      run begin
        S.create ()   >>= fun t ->
        S.list t path >>= fun paths ->
        List.iter (fun p -> IrminLog.msg "%s" (IrminTree.Path.pretty p)) paths;
        return_unit
      end
    in
    Term.(mk ls $ path);
}

(* TREE *)
let tree = {
  name = "tree";
  doc  = "List the store contents.";
  man  = [];
  term =
  let tree () =
    let (module S) = local_store default_dir in
    run begin
      S.create () >>= fun t ->
      S.contents t >>= fun all ->
      let all = List.map (fun (k,v) -> IrminTree.Path.to_string k, S.Value.pretty v) all in
      let max_lenght l =
        List.fold_left (fun len s -> max len (String.length s)) 0 l in
      let k_max = max_lenght (List.map fst all) in
      let v_max = max_lenght (List.map snd all) in
      let pad = 80 + k_max + v_max in
      List.iter (fun (k,v) ->
          let dots = String.make (pad - String.length k - String.length v) '.' in
          IrminLog.msg "%s%s%s" k dots v
        ) all;
      return_unit
    end
  in
  Term.(mk tree $ pure ());
}

(* WRITE *)
let write = {
  name = "write";
  doc  = "Write/modify a node.";
  man  = [];
  term =
    let write path value =
      let (module S) = local_store default_dir in
      run begin
        S.create () >>= fun t ->
        S.update t path value
      end
    in
    Term.(mk write $ path $ value);
}

(* RM *)
let rm = {
  name = "rm";
  doc  = "Remove a node.";
  man  = [];
  term =
    let rm path =
      let (module S) = local_store default_dir in
      run begin
        S.create () >>= fun t ->
        S.remove t path
      end
    in
    Term.(mk rm $ path);
}


(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Clone a remote irminsule store.";
  man  = [];
  term =
    let clone (module R: Irmin.SIMPLE) =
      let (module L) = local_store default_dir in
      !init_hook ();
      run begin
        L.create ()         >>= fun local  ->
        R.create ()         >>= fun remote ->
        R.snapshot remote   >>= fun tag    ->
        R.export remote []  >>= fun dump   ->
        IrminLog.msg "Cloning %d bytes" (R.Dump.sizeof dump);
        L.import local dump >>= fun ()     ->
        L.revert local tag
      end
    in
    Term.(mk clone $ store);
}

let todo = {
  name = "TODO";
  doc  = "TODO";
  man  = [];
  term = Term.(pure (fun _ -> failwith "TODO") $ pure ());
}

(* PULL *)
let pull = {
  name = "pull";
  doc  = "Pull the contents of a remote irminsule store.";
  man  = [];
  term =
    let pull (module R: Irmin.SIMPLE) =
      let (module L) = local_store default_dir in
      run begin
        L.create ()         >>= fun local  ->
        R.create ()         >>= fun remote ->
        L.snapshot local    >>= fun l      ->
        R.snapshot remote   >>= fun r      ->
        R.export remote [l] >>= fun dump   ->
        IrminLog.msg "Cloning %d bytes" (R.Dump.sizeof dump);
        L.import local dump >>= fun ()     ->
        (* XXX: deal with merge conflicts properly. *)
        if R.Dump.is_empty dump  then return_unit
        else L.revert local r
      end
    in
    Term.(mk pull $ store);
}


(* PUSH *)
let push = {
  name = "push";
  doc  = "Pull the contents of the local store to a remote irminsule store.";
  man  = [];
  term =
    let push (module R: Irmin.SIMPLE) =
      let (module L) = local_store default_dir in
      run begin
        L.create ()          >>= fun local  ->
        R.create ()          >>= fun remote ->
        L.snapshot local     >>= fun l      ->
        R.snapshot remote    >>= fun r      ->
        L.export local [r]   >>= fun dump   ->
        IrminLog.msg "Pushing %d bytes" (R.Dump.sizeof dump);
        R.import remote dump >>= fun ()     ->
        (* XXX: deal with merge conflicts properly. *)
        if L.Dump.is_empty dump  then return_unit
        else L.revert local r
      end
    in
    Term.(mk push $ store);
}

(* SNAPSHOT *)
let snapshot = {
  todo with
  name = "snaspshot";
  doc  = "Snapshot the contents of the store."
}

(* REVERT *)
let revert = {
  todo with
  name = "revert";
  doc = "Revert the contents of the store to a previous state.";
}

(* WATCH *)
let watch = {
  todo with
  name = "watch";
  doc  = "Watch the contents of a store and be notified on updates.";
}

(* DUMP *)
let dump = {
  name = "dump";
  doc  = "Dump the contents of the store as a Graphviz file.";
  man  = [];
  term =
    let basename =
      let doc =
        Arg.info ~docv:"BASENAME" ~doc:"Basename for the .dot and .png files." [] in
      Arg.(required & pos 0 (some & string) None & doc) in
    let dump (module S: Irmin.SIMPLE) basename =
      run begin
        S.create () >>= fun t ->
        S.output t basename
      end
    in
    Term.(mk dump $ store $ basename);
}

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about Irminsule and Irminsule commands.";
  man = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ];
  term =
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
    Term.(ret (mk help $Term.man_format $Term.choice_names $topic))
}

let default =
  let doc = "Irminsule, the database that never forgets." in
  let man = [
    `S "DESCRIPTION";
    `P "Irminsule is a distributed database with built-in snapshot, branch \
        and revert mechanisms. It is designed to use a large variety of backends, \
        although it is optimized for append-only ones.";
    `P "Irminsule is written in pure OCaml, and can thus be compiled to a variety of \
        backends including Javascript -- to run inside Browsers, and Mirage microkernels \
        -- to run directly on top of Xen.";
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] in
  let usage global =
    app_global global;
    Printf.printf
      "usage: irmin [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\
      \n\
      The most commonly used irminsule commands are:\n\
      \    init        %s\n\
      \    read        %s\n\
      \    write       %s\n\
      \    rm          %s\n\
      \    ls          %s\n\
      \    tree        %s\n\
      \    clone       %s\n\
      \    pull        %s\n\
      \    push        %s\n\
      \    snaphsot    %s\n\
      \    revert      %s\n\
      \    watch       %s\n\
      \    dump        %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      init.doc read.doc write.doc rm.doc ls.doc tree.doc
      clone.doc pull.doc push.doc snapshot.doc revert.doc
      watch.doc dump.doc in
  Term.(pure usage $ global),
  Term.info "irmin"
    ~version:IrminVersion.current
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map command [
  init;
  read;
  write;
  rm;
  ls;
  tree;
  clone;
  pull;
  push;
  snapshot;
  revert;
  watch;
  dump;
]
