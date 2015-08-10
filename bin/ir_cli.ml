(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Irmin_unix
open Printf
open Ir_resolver

let fmt t = Printf.ksprintf (fun s -> t s)

let () =
  install_dir_polling_listener 0.5

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
  `P "Check bug reports at https://github.com/mirage/irmin/issues.";
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

let path =
  let path_conv =
    let parse str = `Ok str in
    let print ppf path = pr_str ppf path in
    parse, print
  in
  let doc = Arg.info ~docv:"PATH" ~doc:"Local path." [] in
  Arg.(required & pos 0 (some path_conv) None & doc)

let depth =
  let doc =
    Arg.info ~docv:"DEPTH" ~doc:"Limit the history depth." ["d";"depth"] in
  Arg.(value & opt (some int) None & doc)

let run t =
  Lwt_unix.run (
    catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
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
      let doc = Arg.info ~doc:"Start an Irmin server." ["d";"daemon"] in
      Arg.(value & flag & doc)
    in
    let uri =
      let doc =
        Arg.info ~docv:"URI" ["a";"address"]
          ~doc:"Start the Irmin server on the given socket address \
                (to use with --daemon)." in
      Arg.(value & opt string "http://localhost:8080" & doc)
    in
    let init (S ((module S), store)) daemon uri =
      run begin
        store >>= fun t ->
        let module HTTP = Irmin_http_server.Make(S) in
        if daemon then
          let uri = Uri.of_string uri in
          Log.info "daemon: %s" (Uri.to_string uri);
          HTTP.listen ~timeout:3600 (t "Initialising the HTTP server.") uri
        else return_unit
      end
    in
    Term.(mk init $ store $ daemon $ uri)
}

let print fmt =
  ksprintf print_endline fmt

(* READ *)
let read = {
  name = "read";
  doc  = "Read the contents of a node.";
  man  = [];
  term =
    let read (S ((module S), store)) path =
      run begin
        store >>= fun t ->
        S.read (fmt t "Reading %s" path) (S.Key.of_hum path) >>= function
        | None   -> print "<none>"; exit 1
        | Some v -> print "%s" (Tc.write_string (module S.Val) v); return_unit
      end
    in
    Term.(mk read $ store $ path);
}

(* LS *)
let ls = {
  name = "ls";
  doc  = "List subdirectories.";
  man  = [];
  term =
    let ls (S ((module S), store)) path =
      run begin
        store >>= fun t ->
        S.list (fmt t "ls %s." path) (S.Key.of_hum path) >>= fun paths ->
        List.iter (fun p -> print "%s" (S.Key.to_hum p)) paths;
        return_unit
      end
    in
    Term.(mk ls $ store $ path);
}

(* TREE *)
let tree = {
  name = "tree";
  doc  = "List the store contents.";
  man  = [];
  term =
  let tree (S ((module S), store)) =
    run begin
      store >>= fun t ->
      let all = ref [] in
      S.iter (t "tree") (fun k v ->
          v >>= fun v -> all := (k, v) :: !all; return_unit
        )>>= fun () ->
      let all = !all in
      let all =
        List.map (fun (k,v) ->
            S.Key.to_hum k, sprintf "%S" (Tc.write_string (module S.Val) v)
          ) all in
      let max_lenght l =
        List.fold_left (fun len s -> max len (String.length s)) 0 l in
      let k_max = max_lenght (List.map fst all) in
      let v_max = max_lenght (List.map snd all) in
      let pad = 79 + k_max + v_max in
      List.iter (fun (k,v) ->
          let dots =
            String.make (pad - String.length k - String.length v) '.'
          in
          print "%s%s%s" k dots v
        ) all;
      return_unit
    end
  in
  Term.(mk tree $ store);
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
    let write (S ((module S), store)) args =
      run begin
        store >>= fun t ->
        let mk value =
          try Tc.read_string (module S.Val) value
          with _ -> failwith "invalid value" in
        let path, value = match args with
          | [] | [_]      -> failwith "Not enough arguments"
          | [path; value] -> S.Key.of_hum path, mk value
          | _             -> failwith "Too many arguments"
        in
        S.update (t "write") path value
      end
    in
    Term.(mk write $ store $ args);
}

(* RM *)
let rm = {
  name = "rm";
  doc  = "Remove a node.";
  man  = [];
  term =
    let rm (S ((module S), store)) path =
      run begin
        store >>= fun t ->
        S.remove (fmt t "rm %s." path) (S.Key.of_hum path)
      end
    in
    Term.(mk rm $ store $ path);
}

(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Clone a repository into a new store.";
  man  = [];
  term =
    let clone (S ((module S), store)) remote depth =
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun remote ->
        Sync.fetch (t "Cloning.") ?depth remote >>= function
        | `Head d  -> S.update_head (t "update head after clone") d
        | `No_head -> return_unit
        | `Error   -> Printf.eprintf "Error!\n"; exit 1
      end
    in
    Term.(mk clone $ store $ remote $ depth);
}

(* FETCH *)
let fetch = {
  name = "fetch";
  doc  = "Download objects and refs from another repository.";
  man  = [];
  term =
    let fetch (S ((module S), store)) remote =
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun r ->
        let tag = S.Tag.of_hum "import" in
        S.of_tag (S.Private.config (t "config")) task tag >>= fun t ->
        Sync.pull_exn (t "Fetching.")  r `Update
      end
    in
    Term.(mk fetch $ store $ remote);
}

(* PULL *)
let pull = {
  name = "pull";
  doc  = "Fetch and merge with another repository.";
  man  = [];
  term =
    let pull (S ((module S), store)) remote =
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun r ->
        Sync.pull_exn (t "Pulling.") r `Merge
      end
    in
    Term.(mk pull $ store $ remote);
}

(* PUSH *)
let push = {
  name = "push";
  doc  = "Update remote references along with associated objects.";
  man  = [];
  term =
    let push (S ((module S), store)) remote =
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun r ->
        Sync.push_exn (fmt t "Pushing.") r
      end
    in
    Term.(mk push $ store $ remote);
}

(* SNAPSHOT *)
let snapshot = {
  name = "snapshot";
  doc  = "Snapshot the contents of the store.";
  man  = [];
  term =
    let snapshot (S ((module S), store)) =
      run begin
        store >>= fun t ->
        S.head_exn (t "Snapshot") >>= fun k ->
        print "%s" (S.Head.to_hum k);
        return_unit
      end
    in
    Term.(mk snapshot $ store)
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
    let revert (S ((module S), store)) snapshot =
      run begin
        store >>= fun t ->
        let s = S.Head.of_hum snapshot in
        S.update_head (t "Revert") s
      end
    in
    Term.(mk revert $ store $ snapshot)
}
(* WATCH *)
let watch = {
  name = "watch";
  doc  = "Watch the contents of a store and be notified on updates.";
  man  = [];
  term =
    let watch (S ((module S), store)) path =
      let path = S.Key.of_hum path in
      let module View = Irmin.View(S) in
      run begin
        store >>= fun t ->
        View.watch_path (t "watch") path (fun d ->
            let pr (k, v) =
              let k = S.Key.to_hum k in
              let v = match v with
                | `Updated _ -> "*"
                | `Added _   -> "+"
                | `Removed _ -> "-"
              in
              printf "%s%s\n%!" v k
            in
            let x, y = match d with
              | `Updated (x, y) -> Lwt.return (snd x), Lwt.return (snd y)
              | `Added x        -> View.empty (), Lwt.return (snd x)
              | `Removed x      -> Lwt.return (snd x), View.empty ()
            in
            x >>= fun x -> y >>= fun y ->
            View.diff x y >>= fun diff ->
            List.iter pr diff;
            return_unit
          ) >>= fun _ ->
        let t, _ = Lwt.task () in
        t
      end
    in
    Term.(mk watch $ store $ path)
}

(* DOT *)
let dot = {
  name = "dot";
  doc  = "Dump the contents of the store as a Graphviz file.";
  man  = [];
  term =
    let basename =
      let doc =
        Arg.info ~docv:"BASENAME" ~doc:"Basename for the .dot and .png files." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let no_dot_call =
      let doc =
        Arg.info ~doc:"Do not call the `dot' utility on the generated `.dot` file."
          ["no-dot-call"] in
      Arg.(value & flag & doc) in
    let full =
      let doc =
        Arg.info ~doc:"Show the full graph of objects, including the filesystem \
                       nodes and the content blobs."
          ["full"] in
      Arg.(value & flag & doc) in
    let dot (S ((module S), store)) basename depth no_dot_call full =
      let module Dot = Irmin.Dot(S) in
      let date d =
        let tm = Unix.localtime (Int64.to_float d) in
        Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      in
      run begin
        store >>= fun t ->
        let call_dot = not no_dot_call in
        let buf = Buffer.create 1024 in
        Dot.output_buffer ~html:false (t "output dot file") ?depth ~full ~date
          buf >>= fun () ->
        let oc = open_out_bin (basename ^ ".dot") in
        Lwt.finalize
          (fun () -> output_string oc (Buffer.contents buf); return_unit)
          (fun () -> close_out oc; return_unit)
        >>= fun () ->
        if call_dot then (
          let i = Sys.command "/bin/sh -c 'command -v dot'" in
          if i <> 0 then Log.error
              "Cannot find the `dot' utility. Please install it on your system \
               and be sure it is available in your $PATH.";
          let i = Sys.command
              (Printf.sprintf "dot -Tpng %s.dot -o%s.png" basename basename) in
          if i <> 0 then Log.error "The %s.dot is corrupted" basename;
        );
        return_unit
      end
    in
    Term.(mk dot $ store $ basename $ depth $ no_dot_call $ full);
}

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about Irmin and Irmin commands.";
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
  let doc = "Irmin, the database that never forgets." in
  let man = [
    `S "DESCRIPTION";
    `P "Irmin is a distributed database with built-in snapshot, branch \
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
      The most commonly used subcommands are:\n\
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
      \    snapshot    %s\n\
      \    revert      %s\n\
      \    watch       %s\n\
      \    dot         %s\n\
      \n\
      See `irmin help <command>` for more information on a specific command.\n%!"
      init.doc read.doc write.doc rm.doc ls.doc tree.doc
      clone.doc fetch.doc pull.doc push.doc snapshot.doc
      revert.doc watch.doc dot.doc in
  Term.(pure usage $ global),
  Term.info "irmin"
    ~version:Irmin.version
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map create_command [
    help;
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
    dot;
  ]

let run ~default:x y =
  match Cmdliner.Term.eval_choice x y with
  | `Error _ -> exit 1
  | _        -> ()
