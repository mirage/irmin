(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Core_kernel.Std
open Cmdliner

let () =
  let origin =
    sprintf "Irminsule (%s[%d])" (Unix.gethostname()) (Unix.getpid()) in
  IrminOrigin.set_date (fun () -> Int64.of_float (Unix.time ()));
  IrminOrigin.set_id (fun () -> origin);
  IrminOrigin.set_string_of_date (fun d ->
      let tm = Unix.localtime (Int64.to_float d) in
      sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    );
  IrminFS.install_dir_polling_listener 0.5

(* Global options *)
type global = {
  level: Log.log_level option;
}

let app_global g =
  Log.color_on ();
  match g.level with
  | None   -> ()
  | Some d -> Log.set_log_level d

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
  let debug =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be very verbose." ["debug"] in
    Arg.(value & flag & doc) in
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let level debug verbose =
    match debug, verbose with
    | true, _    -> { level = Some Log.DEBUG }
    | _   , true -> { level = Some Log.INFO }
    | _          -> { level = None } in
  Term.(pure level $ debug $ verbose)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = unit Term.t * Term.info

type sub = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let create_command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

(* Converters *)

let pr_str = Format.pp_print_string

let path_conv =
  let parse str = `Ok (IrminPath.of_string str) in
  let print ppf path = pr_str ppf (IrminPath.to_string path) in
  parse, print

let path =
  let doc = Arg.info ~docv:"PATH" ~doc:"Path." [] in
  Arg.(value & pos 0 path_conv [] & doc)

let depth =
  let doc =
    Arg.info ~docv:"DEPTH" ~doc:"Limit the dump depth." ["d";"depth"] in
  Arg.(value & opt (some int) None & doc)

let run t =
  Lwt_unix.run (
    catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Exn.to_string e); exit 1)
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
      let doc = Arg.info ~doc:"Start an Irminsule server." ["d";"daemon"] in
      Arg.(value & flag & doc) in
    let uri =
      let doc =
        Arg.info ~docv:"URI" ["a";"address"]
          ~doc:"Start the Irminsule server on the given socket address \
                (to use with --daemon)." in
      Arg.(value & opt string "http://localhost:8080" & doc) in
    let init (module S: Irmin.S) daemon uri =
      run begin
        S.create () >>= fun t ->
        IrminResolver.init_hook ();
        if daemon then
          let uri = Uri.of_string uri in
          Log.infof "daemon: %s" (Uri.to_string uri);
          IrminHTTP.start_server (module S) t uri
        else return_unit
      end
    in
    Term.(mk init $ IrminResolver.store $ daemon $ uri)
}

let print fmt =
  ksprintf print_endline fmt

(* READ *)
let read = {
  name = "read";
  doc  = "Read the contents of a node.";
  man  = [];
  term =
    let read (module S: Irmin.S) path =
      run begin
        S.create ()   >>= fun t ->
        S.read t path >>= function
        | None   -> print "<none>"; exit 1
        | Some v -> print "%s" (S.Value.to_string v); return_unit
      end
    in
    Term.(mk read $ IrminResolver.store $ path);
}

(* LS *)
let ls = {
  name = "ls";
  doc  = "List subdirectories.";
  man  = [];
  term =
    let ls (module S: Irmin.S) path =
      run begin
        S.create ()     >>= fun t ->
        S.list t [path] >>= fun paths ->
        List.iter ~f:(fun p -> print "%s" (IrminPath.to_string p)) paths;
        return_unit
      end
    in
    Term.(mk ls $ IrminResolver.store $ path);
}

(* TREE *)
let tree = {
  name = "tree";
  doc  = "List the store contents.";
  man  = [];
  term =
  let tree (module S: Irmin.S) =
    run begin
      S.create () >>= fun t ->
      S.dump t    >>= fun all ->
      let all =
        List.map ~f:(fun (k,v) ->
            IrminPath.to_string k, sprintf "%S" (S.Value.to_string v)
          ) all in
      let max_lenght l =
        List.fold_left ~f:(fun len s -> max len (String.length s)) ~init:0 l in
      let k_max = max_lenght (List.map ~f:fst all) in
      let v_max = max_lenght (List.map ~f:snd all) in
      let pad = 79 + k_max + v_max in
      List.iter ~f:(fun (k,v) ->
          let dots = String.make (pad - String.length k - String.length v) '.' in
          print "/%s%s%s" k dots v
        ) all;
      return_unit
    end
  in
  Term.(mk tree $ IrminResolver.store);
}

(* WRITE *)
let write = {
  name = "write";
  doc  = "Write/modify a node.";
  man  = [];
  term =
    let args =
      let doc = Arg.info ~docv:"VALUE" ~doc:"Value to add." [] in
      Arg.(value & pos_all string [] & doc) in
    let write (module S: Irmin.S) args =
      let mk value =
        try S.Value.of_string value
        with _ -> failwith "invalid value" in
      let path, value = match args with
        | []            -> failwith "Not enough arguments"
        | [path; value] -> IrminPath.of_string path, mk value
        | [value]       -> []                      , mk value
        | _             -> failwith "Too many arguments" in
      run begin
        S.create () >>= fun t ->
        S.update t path value
      end
    in
    Term.(mk write $ IrminResolver.store $ args);
}

(* RM *)
let rm = {
  name = "rm";
  doc  = "Remove a node.";
  man  = [];
  term =
    let rm (module S: Irmin.S) path =
      run begin
        S.create () >>= fun t ->
        S.remove t path
      end
    in
    Term.(mk rm $ IrminResolver.store $ path);
}

(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Clone a repository into a new store.";
  man  = [];
  term =
    let clone (module L: Irmin.S) remote depth =
      IrminResolver.init_hook ();
      run begin
        L.create ()                           >>= fun local  ->
        L.Dump.create_exn local ?depth remote >>= fun d ->
        L.Dump.update local d
      end
    in
    Term.(mk clone $ IrminResolver.store $ IrminResolver.remote $ depth);
}

(* FETCH *)
let fetch = {
  name = "fetch";
  doc  = "Download objects and refs from another repository.";
  man  = [];
  term =
    let fetch (module L: Irmin.S) remote =
      run begin
        let branch = L.Branch.of_string "import" in
        L.create ~branch ()            >>= fun local ->
        L.Dump.create_exn local remote >>= fun d ->
        L.Dump.update local d
      end
    in
    Term.(mk fetch $ IrminResolver.store $ IrminResolver.remote);
}

(* PULL *)
let pull = {
  name = "pull";
  doc  = "Fetch and merge with another repository.";
  man  = [];
  term =
    let pull (module L: Irmin.S) remote =
      run begin
        L.create ()                    >>= fun local ->
        L.Dump.create_exn local remote >>= fun d ->
        L.Dump.merge_exn local d
      end
    in
    Term.(mk pull $ IrminResolver.store $ IrminResolver.remote);
}

(* PUSH *)
let push = {
  name = "push";
  doc  = "Update remote references along with associated objects.";
  man  = [];
  term =
    let push (module L: Irmin.S) remote =
      run begin
        L.create ()                  >>= fun local ->
        L.Dump.push_exn local remote >>= fun _ ->
        return_unit
      end
    in
    Term.(mk push $ IrminResolver.store $ IrminResolver.remote);
}

(* SNAPSHOT *)
let snapshot = {
  name = "snapshot";
  doc  = "Snapshot the contents of the store.";
  man  = [];
  term =
    let snapshot (module S: Irmin.S) =
      run begin
        S.create ()         >>= fun t ->
        S.Snapshot.create t >>= fun k ->
        print "%s" S.Snapshot.(to_string @@ to_state k);
        return_unit
      end
    in
    Term.(mk snapshot $ IrminResolver.store)
}

(* REVERT *)
let revert = {
  name = "revert";
  doc  = "Revert the contents of the store to a previous state.";
  man  = [];
  term =
    let snapshot =
      let doc = Arg.info ~docv:"SNAPSHOT" ~doc:"The snapshot to revert to." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let revert (module S: Irmin.S) snapshot =
      run begin
        S.create () >>= fun t ->
        let s = S.Snapshot.(of_state t @@ S.Snapshot.of_string snapshot) in
        S.Snapshot.revert t s
      end
    in
    Term.(mk revert $ IrminResolver.store $ snapshot)
}
(* WATCH *)
let watch = {
  name = "watch";
  doc  = "Watch the contents of a store and be notified on updates.";
  man  = [];
  term =
    let path =
      let doc =
        Arg.info ~docv:"PATH" ~doc:"The path to watch." [] in
      Arg.(value & pos 0 path_conv [] & doc) in
    let watch (module S: Irmin.S) path =
      run begin
        S.create () >>= fun t ->
        let stream = S.Snapshot.watch t path in
        Lwt_stream.iter_s (fun (path, s) ->
            print "%s %s" (IrminPath.to_string path) S.Snapshot.(to_string @@ to_state s);
            return_unit
          ) stream
      end
    in
    Term.(mk watch $ IrminResolver.store $ path)
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
    let dump (module S: Irmin.S) basename depth =
      run begin
        S.create () >>= fun t ->
        S.Dump.output_file t ?depth basename
      end
    in
    Term.(mk dump $ IrminResolver.store $ basename $ depth);
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
        let conv, _ = Arg.enum (List.rev_map ~f:(fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter ~f:print_endline cmds; `Ok ()
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
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] in
  let usage global =
    app_global global;
    printf
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
      \    fetch       %s\n\
      \    pull        %s\n\
      \    push        %s\n\
      \    snaphsot    %s\n\
      \    revert      %s\n\
      \    watch       %s\n\
      \    dump        %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      init.doc read.doc write.doc rm.doc ls.doc tree.doc
      clone.doc fetch.doc pull.doc push.doc snapshot.doc
      revert.doc watch.doc dump.doc in
  Term.(pure usage $ global),
  Term.info "irmin"
    ~version:IrminVersion.current
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map ~f:create_command [
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

let run ~default:x y =
  match Cmdliner.Term.eval_choice x y with
  | `Error _ -> exit 1
  | _        -> ()
