(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Cmdliner
open Ir_resolver

let () = Irmin_unix.set_listen_dir_hook ()

let info ?author:(author="irmin") fmt = Irmin_unix.info ~author fmt

(* Help sections common to all commands *)
let help_sections = [
  `S global_option_section;
  `P "These options can be passed to any command";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/mirage/irmin/issues.";
]

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~docs:global_option_section ~doc ~man title

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
  let doc = Arg.info ~docv:"PATH" ~doc:"Key to lookup or modify." [] in
  Arg.(required & pos 0 (some path_conv) None & doc)

let depth =
  let doc =
    Arg.info ~docv:"DEPTH" ~doc:"Limit the history depth." ["d";"depth"] in
  Arg.(value & opt (some int) None & doc)

let print_exc exc =
  begin
    match exc with
    | Failure f -> Fmt.epr "ERROR: %s\n%!" f
    | e -> Fmt.epr "ERROR: %a\n%!" Fmt.exn e
  end;
  exit 1

let run t =
  Lwt_main.run (
    Lwt.catch
      (fun () -> t)
      print_exc
  )

let mk (fn:'a): 'a Term.t =
  Term.(const (fun () -> fn) $ setup_log)

(* INIT *)
let init = {
  name = "init";
  doc  = "Initialize a store.";
  man  = [];
  term =
    let daemon =
      let doc = Arg.info ~doc:"Start an Irmin HTTP server." ["d";"daemon"] in
      Arg.(value & flag & doc)
    in
    let uri =
      let doc =
        Arg.info ~docv:"URI" ["a";"address"]
          ~doc:"Start the Irmin server on the given socket address \
                (to use with --daemon). Examples include \
                http://localhost:8080 and \
                launchd://Listener." in
      Arg.(value & opt string "http://localhost:8080" & doc)
    in
    let init (S ((module S), store)) daemon uri =
      run begin
        store >>= fun t ->
        let module HTTP = Irmin_unix.Http.Server(S) in
        if daemon then
          let uri = Uri.of_string uri in
          let spec = HTTP.v (S.repo t) in
          match Uri.scheme uri with
          | Some "launchd" ->
            let uri, name = match Uri.host uri with
              | None   -> Uri.with_host uri (Some "Listener"), "Listener"
              | Some name -> uri, name in
            Logs.info (fun f -> f "daemon: %s" (Uri.to_string uri));
            Cohttp_lwt_unix.Server.create ~timeout:3600 ~mode:(`Launchd name) spec
          | _ ->
            let uri = match Uri.host uri with
              | None   -> Uri.with_host uri (Some "localhost")
              | Some _ -> uri in
            let port, uri = match Uri.port uri with
              | None   -> 8080, Uri.with_port uri (Some 8080)
              | Some p -> p, uri in
            Logs.info (fun f -> f "daemon: %s" (Uri.to_string uri));
            Printf.printf "Server starting on port %d.\n%!" port;
            Cohttp_lwt_unix.Server.create ~timeout:3600 ~mode:(`TCP (`Port port)) spec

        else Lwt.return_unit
      end
    in
    Term.(mk init $ store $ daemon $ uri)
}

let print fmt = Fmt.kstrf print_endline fmt

let get name f x = match f x with
  | Ok x           -> x
  | Error (`Msg e) -> Fmt.kstrf invalid_arg "invalid %s: %s" name e

let key f x = get "key" f x
let value f x = get "value" f x
let branch f x = get "branch" f x
let commit f x = get "commit" f x

(* GET *)
let get = {
  name = "get";
  doc  = "Read the value associated with a key. \
          If the key has not been set then the program \
          will terminate with a non-zero exit code";
  man  = [];
  term =
    let get (S ((module S), store)) path =
      run begin
        store >>= fun t ->
        S.find t (key S.Key.of_string path) >>= function
        | None   -> print "<none>"; exit 1
        | Some v ->
          print "%a" S.Contents.pp v;
          Lwt.return_unit
      end
    in
    Term.(mk get $ store $ path);
}

(* LIST *)
let list = {
  name = "list";
  doc  = "List subdirectories.";
  man  = [];
  term =
    let list (S ((module S), store)) path =
      run begin
        store >>= fun t ->
        S.list t (key S.Key.of_string path) >>= fun paths ->
        let pp ppf (s, k) = match k with
          | `Contents -> Fmt.pf ppf "FILE %a" S.Key.pp_step s
          | `Node     -> Fmt.pf ppf "DIR  %a" S.Key.pp_step s
        in
        List.iter (print "%a" pp) paths;
        Lwt.return_unit
      end
    in
    Term.(mk list $ store $ path);
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
        let todo = ref [S.Key.empty] in
        let rec walk () = match !todo with
          | []      -> Lwt.return_unit
          | k::rest ->
            todo := rest;
            S.list t k >>= fun childs ->
            Lwt_list.iter_p (fun (s, c) ->
                let k = S.Key.rcons k s in
                match c with
                | `Node     -> todo := k :: !todo; Lwt.return_unit
                | `Contents ->
                  S.get t k >|= fun v ->
                  all := (k, v) :: !all
              ) childs >>=
            walk
        in
        walk () >>= fun () ->
        let all = !all in
        let all =
          List.map (fun (k,v) ->
              Fmt.to_to_string S.Key.pp k,
              Fmt.strf "%a" S.Contents.pp v
            ) all in
        let max_length l =
          List.fold_left (fun len s -> max len (String.length s)) 0 l in
        let k_max = max_length (List.map fst all) in
        let v_max = max_length (List.map snd all) in
        let pad = 79 + k_max + v_max in
        List.iter (fun (k,v) ->
            let dots =
              String.make (pad - String.length k - String.length v) '.'
            in
            print "%s%s%s" k dots v
          ) all;
        Lwt.return_unit
      end
    in
    Term.(mk tree $ store);
}

let author =
  let doc = Arg.info ~docv:"NAME" ~doc:"Commit author name." ["author"] in
  Arg.(value & opt (some string) None & doc)

let message =
  let doc = Arg.info ~docv:"MESSAGE" ~doc:"Commit message." ["message"] in
  Arg.(value & opt (some string) None & doc)

(* SET *)
let set = {
  name = "set";
  doc  = "Update the value associated with a key.";
  man  = [];
  term =
    let v =
      let doc = Arg.info ~docv:"VALUE" ~doc:"Value to add." [] in
      Arg.(required & pos 1 (some string) None & doc) in
    let set (S ((module S), store)) author message path v =
      run begin
        let message = match message with Some s -> s | None -> "set" in
        store >>= fun t ->
        let path = key S.Key.of_string path in
        let value = value S.Contents.of_string v in
        S.set t ~info:(info ?author "%s" message) path value
      end
    in
    Term.(mk set $ store $ author $ message $ path $ v);
}

(* REMOVE *)
let remove = {
  name = "remove";
  doc  = "Delete a key.";
  man  = [];
  term =
    let remove (S ((module S), store)) author message path =
      run begin
        let message = match message with Some s -> s | None -> "remove " ^ path in
        store >>= fun t ->
        S.remove t ~info:(info ?author "%s" message) (key S.Key.of_string path)
      end
    in
    Term.(mk remove $ store $ author $ message $ path);
}

(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Copy a remote respository to a local store";
  man  = [];
  term =
    let clone (S ((module S), store)) remote depth =
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun remote ->
        Sync.fetch t ?depth remote >>= function
        | Ok d    -> S.Head.set t d
        | Error e -> Format.eprintf "ERROR: %a!\n" Sync.pp_fetch_error e; exit 1
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
        let branch = match S.Branch.of_string "import" with
          | Ok branch -> branch
          | Error (`Msg e) -> failwith e in
        S.of_branch (S.repo t) branch >>= fun t ->
        Sync.pull_exn t r `Set
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
    let pull (S ((module S), store)) message remote =
      let message = match message with Some s -> s | None -> "pull" in
      let module Sync = Irmin.Sync (S) in
      run begin
        store >>= fun t ->
        remote >>= fun r ->
        Sync.pull_exn t r (`Merge (Irmin_unix.info "%s" message))
      end
    in
    Term.(mk pull $ store $ message $ remote);
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
        Sync.push_exn t r
      end
    in
    Term.(mk push $ store $ remote);
}

(* SNAPSHOT *)
let snapshot = {
  name = "snapshot";
  doc  = "Return a snapshot for the current state of the database.";
  man  = [];
  term =
    let snapshot (S ((module S), store)) =
      run begin
        store >>= fun t ->
        S.Head.get t >>= fun k ->
        print "%a" S.Commit.pp k;
        Lwt.return_unit
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
        let hash = match S.Commit.Hash.of_string snapshot with
          | Ok hash -> hash
          | Error (`Msg msg) -> failwith msg in
        S.Commit.of_hash (S.repo t) hash >>= fun s ->
        match s with
        | Some s -> S.Head.set t s
        | None -> failwith "invalid commit"
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
      let path = key S.Key.of_string path in
      run begin
        store >>= fun t ->
        S.watch_key t path (fun d ->
            let pr (k, v) =
              let v = match v with
                | `Updated _ -> "*"
                | `Added _   -> "+"
                | `Removed _ -> "-"
              in
              print "%s%a" v S.Key.pp k
            in
            let view (c, _) =
              S.of_commit c >>= fun t ->
              S.find_tree t path >|= function
              | None   -> S.Tree.empty
              | Some v -> v
            in
            let empty = Lwt.return S.Tree.empty in
            let x, y = match d with
              | `Updated (x, y) -> view x, view y
              | `Added x        -> empty , view x
              | `Removed x      -> view x, empty
            in
            x >>= fun x -> y >>= fun y ->
            S.Tree.diff x y >>= fun diff ->
            List.iter pr diff;
            Lwt.return_unit
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
        Arg.info ~docv:"BASENAME"
                 ~doc:"Basename for the .dot and .png files, for example: \
                      ./graph will create ./graph.dot and ./graph.png" [] in
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
        Dot.output_buffer ~html:false t ?depth ~full ~date buf >>= fun () ->
        let oc = open_out_bin (basename ^ ".dot") in
        Lwt.finalize
          (fun () -> output_string oc (Buffer.contents buf); Lwt.return_unit)
          (fun () -> close_out oc; Lwt.return_unit)
        >>= fun () ->
        if call_dot then (
          let i = Sys.command "/bin/sh -c 'command -v dot'" in
          if i <> 0 then Logs.err (fun f -> f
                                      "Cannot find the `dot' utility. Please install it on your system \
                                       and be sure it is available in your $PATH.");
          let i = Sys.command
              (Printf.sprintf "dot -Tpng %s.dot -o%s.png" basename basename) in
          if i <> 0 then Logs.err (fun f -> f "The %s.dot is corrupted" basename);
        );
        Lwt.return_unit
      end
    in
    Term.(mk dot $ store $ basename $ depth $ no_dot_call $ full);
}

let irminconfig_man =
  let version_string = Printf.sprintf "Irmin %s" Irmin.version in
  ("irminconfig", 5, "", version_string, "Irmin Manual"), [
    `S Manpage.s_name;
    `P "irminconfig - Specify certain command-line options to save on typing";

    `S Manpage.s_synopsis;
    `P ".irminconfig";

    `S Manpage.s_description;
    `P "An $(b,irminconfig) file lets the user specify repetitve command-line options \
        in a text file. The $(b,irminconfig) file is only read if it is found in \
        the current working directory. Every line is of the form $(i,key)=$(i,value), \
        where $(i,key) is one of the following: $(b,contents), $(b,store), $(b,branch), \
        $(b,root), $(b,bare), $(b,head), or $(b,uri). These correspond to the irmin \
        options of the same names.";

    `S "NOTES";
    `P "When specifying a value for the $(b,contents) or $(b,store) options, the \
        shortest unique substring starting from index 0 is sufficient. For example, \
        \"store=g\" is equivalent to \"store=git\".";

    `S Manpage.s_examples;
    `P "Here is an example $(b,irminconfig) for accessing a local http irmin store. This \
       $(b,irminconfig) prevents the user from having to specify the $(b,store) and $(b,uri) \
       options for every command.";
    `Pre "    \\$ cat .irminconfig\n    store=http\n    uri=http://127.0.0.1:8080";
  ] @ help_sections

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
        let topics = "irminconfig" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) ("topics" :: topics)) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
        | `Ok t when t = "irminconfig" ->
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter irminconfig_man)
        | `Ok t                   -> `Help (man_format, Some t) in
    Term.(ret (mk help $Term.man_format $Term.choice_names $topic))
}

let default =
  let doc = "Irmin, the database that never forgets." in
  let man = [
    `S "DESCRIPTION";
    `P "Irmin is a distributed database used primarily for application data. \
        It is designed to work with a large variety of backends and has built-in \
        snapshotting, reverting and branching mechanisms.";
    `P "Use either $(mname) <command> --help or $(mname) help <command> \
        for more information on a specific command.";
  ] in
  let usage () =
    Fmt.pr
      "usage: irmin [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\
       \n\
       The most commonly used subcommands are:\n\
      \    init        %s\n\
      \    get         %s\n\
      \    set         %s\n\
      \    remove      %s\n\
      \    list        %s\n\
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
       See `irmin help <command>` for more information on a specific command.\n\
       %!"
      init.doc get.doc set.doc remove.doc list.doc tree.doc
      clone.doc fetch.doc pull.doc push.doc snapshot.doc
      revert.doc watch.doc dot.doc
  in
  Term.(mk usage $ const ()),
  Term.info "irmin"
    ~version:Irmin.version
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map create_command [
    help;
    init;
    get;
    set;
    remove;
    list;
    tree;
    clone;
    fetch;
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
