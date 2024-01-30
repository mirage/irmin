(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open Cmdliner
open Resolver
module Graphql = Irmin_graphql_unix

let deprecated_info = (Term.info [@alert "-deprecated"])
let deprecated_man_format = (Term.man_format [@alert "-deprecated"])
let deprecated_eval_choice = (Term.eval_choice [@alert "-deprecated"])

let info (type a) (module S : Irmin.Generic_key.S with type Schema.Info.t = a)
    ?(author = "irmin") fmt =
  let module Info = Info.Make (S.Info) in
  Info.v ~author fmt

(* Help sections common to all commands *)
let help_sections =
  [
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
  deprecated_info ~sdocs:global_option_section ~docs:global_option_section ~doc
    ~man title

type command = (unit Term.t * Term.info[@alert "-deprecated"])

type sub = {
  name : string;
  doc : string;
  man : Manpage.block list;
  term : unit Term.t;
}

let create_command c =
  let man = [ `S "DESCRIPTION"; `P c.doc ] @ c.man in
  (c.term, term_info c.name ~doc:c.doc ~man)

(* Converters *)

let pr_str = Format.pp_print_string

let path =
  let path_conv =
    let parse str = `Ok str in
    let print ppf path = pr_str ppf path in
    (parse, print)
  in
  let doc = Arg.info ~docv:"PATH" ~doc:"Key to lookup or modify." [] in
  Arg.(required & pos 0 (some path_conv) None & doc)

type path_or_empty = Empty | Path of string

let path_or_empty =
  let path_conv =
    let parse str = `Ok (Path str) in
    let print = Fmt.of_to_string (function Path str -> str | Empty -> "/") in
    (parse, print)
  in
  let doc =
    Arg.info [] ~docv:"PATH"
      ~doc:
        "Path to lookup or modify. Defaults to the empty path (which queries \
         the root tree of a store)."
  in
  Arg.(value & pos 0 path_conv Empty & doc)

let depth =
  let doc =
    Arg.info ~docv:"DEPTH" ~doc:"Limit the history depth." [ "d"; "depth" ]
  in
  Arg.(value & opt (some int) None & doc)

let print_exc exc =
  (match exc with
  | Failure f -> Fmt.epr "ERROR: %s\n%!" f
  | e -> Fmt.epr "ERROR: %a\n%!" Fmt.exn e);
  exit 1

open Lwt.Syntax

let run t = try t () with err -> print_exc err
let mk (fn : 'a) : 'a Term.t = Term.(const (fun () -> fn) $ setup_log)

(* INIT *)
let init =
  {
    name = "init";
    doc = "Initialize a store.";
    man = [];
    term =
      (let init store =
         Eio.Switch.run @@ fun sw ->
         let (S (_, _store, _)) = store ~sw in
         ()
       in
       Term.(mk init $ store ()));
  }

let print fmt = Fmt.kstr print_endline fmt

let get name f x =
  match Irmin.Type.of_string f x with
  | Ok x -> x
  | Error (`Msg e) -> Fmt.kstr invalid_arg "invalid %s: %s" name e

let key f x = get "key" f x
let value f x = get "value" f x
let branch f x = get "branch" f x
let commit f x = get "commit" f x

(* GET *)
let get =
  {
    name = "get";
    doc = "Read the value associated with a key.";
    man = [];
    term =
      (let get store path =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         match S.find t (key S.Path.t path) with
         | None ->
             print "<none>";
             exit 1
         | Some v -> print "%a" (Irmin.Type.pp S.Contents.t) v
       in
       Term.(mk get $ store () $ path));
  }

(* LIST *)
let list =
  {
    name = "list";
    doc = "List subdirectories.";
    man = [];
    term =
      (let list store path_or_empty =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let path =
           match path_or_empty with
           | Empty -> S.Path.empty
           | Path str -> key S.Path.t str
         in
         run @@ fun () ->
         let t = store () in
         let paths = S.list t path in
         let pp_step = Irmin.Type.pp S.Path.step_t in
         let pp ppf (s, k) =
           match S.Tree.destruct k with
           | `Contents _ -> Fmt.pf ppf "FILE %a" pp_step s
           | `Node _ -> Fmt.pf ppf "DIR %a" pp_step s
         in
         List.iter (print "%a" pp) paths
       in
       Term.(mk list $ store () $ path_or_empty));
  }

(* TREE *)
let tree =
  {
    name = "tree";
    doc = "List the store contents.";
    man = [];
    term =
      (let tree store =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         let all = ref [] in
         let todo = ref [ S.Path.empty ] in
         let rec walk () =
           match !todo with
           | [] -> ()
           | k :: rest ->
               todo := rest;
               let childs = S.list t k in
               List.iter
                 (fun (s, c) ->
                   let k = S.Path.rcons k s in
                   match S.Tree.destruct c with
                   | `Node _ -> todo := k :: !todo
                   | `Contents _ ->
                       let v = S.get t k in
                       all := (k, v) :: !all)
                 childs;
               walk ()
         in
         walk ();
         let all = !all in
         let all =
           List.map
             (fun (k, v) ->
               ( Irmin.Type.to_string S.Path.t k,
                 Irmin.Type.to_string S.Contents.t v ))
             all
         in
         let max_length l =
           List.fold_left (fun len s -> max len (String.length s)) 0 l
         in
         let k_max = max_length (List.map fst all) in
         let v_max = max_length (List.map snd all) in
         let pad = 79 + k_max + v_max in
         List.iter
           (fun (k, v) ->
             let dots =
               String.make (pad - String.length k - String.length v) '.'
             in
             print "%s%s%s" k dots v)
           all
       in
       Term.(mk tree $ store ()));
  }

let author =
  let doc = Arg.info ~docv:"NAME" ~doc:"Commit author name." [ "author" ] in
  Arg.(value & opt (some string) None & doc)

let message =
  let doc = Arg.info ~docv:"MESSAGE" ~doc:"Commit message." [ "message" ] in
  Arg.(value & opt (some string) None & doc)

(* SET *)
let set =
  {
    name = "set";
    doc = "Update the value associated with a key.";
    man = [];
    term =
      (let v =
         let doc = Arg.info ~docv:"VALUE" ~doc:"Value to add." [] in
         Arg.(required & pos 1 (some string) None & doc)
       in
       let set store author message path v =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let message = match message with Some s -> s | None -> "set" in
         let t = store () in
         let path = key S.Path.t path in
         let value = value S.Contents.t v in
         S.set_exn t ~info:(info (module S) ?author "%s" message) path value
       in
       Term.(mk set $ store () $ author $ message $ path $ v));
  }

(* REMOVE *)
let remove =
  {
    name = "remove";
    doc = "Delete a key.";
    man = [];
    term =
      (let remove store author message path =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let message =
           match message with Some s -> s | None -> "remove " ^ path
         in
         let t = store () in
         S.remove_exn t
           ~info:(info (module S) ?author "%s" message)
           (key S.Path.t path)
       in
       Term.(mk remove $ store () $ author $ message $ path));
  }

let apply e f =
  match (e, f) with
  | R (h, e), Some f -> f ?ctx:None ?headers:h e ()
  | R _, None -> Fmt.failwith "invalid remote for that kind of store"
  | r, _ -> r

(* CLONE *)
let clone =
  {
    name = "clone";
    doc = "Copy a remote respository to a local store";
    man = [];
    term =
      (let clone sr depth =
         Eio.Switch.run @@ fun sw ->
         let S (impl, store, f), remote = sr ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run @@ fun () ->
         let t = store () in
         let r = remote () in
         let x = apply r f in
         match Sync.fetch t ?depth x with
         | Ok (`Head d) -> S.Head.set t d
         | Ok `Empty -> ()
         | Error (`Msg e) -> failwith e
       in
       Term.(mk clone $ Resolver.remote () $ depth));
  }

(* FETCH *)
let fetch =
  {
    name = "fetch";
    doc = "Download objects and refs from another repository.";
    man = [];
    term =
      (let fetch sr =
         Eio.Switch.run @@ fun sw ->
         let S (impl, store, f), remote = sr ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run @@ fun () ->
         let t = store () in
         let r = remote () in
         let branch = branch S.Branch.t "import" in
         let t = S.of_branch (S.repo t) branch in
         let x = apply r f in
         let _ = Sync.pull_exn t x `Set in
         ()
       in
       Term.(mk fetch $ Resolver.remote ()));
  }

(* MERGE *)
let merge =
  {
    name = "merge";
    doc = "Merge branches.";
    man = [];
    term =
      (let merge store author message branch =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let message = match message with Some s -> s | None -> "merge" in
         let branch =
           match Irmin.Type.of_string S.Branch.t branch with
           | Ok b -> b
           | Error (`Msg msg) -> failwith msg
         in
         let t = store () in
         match
           S.merge_with_branch t branch
             ~info:(info (module S) ?author "%s" message)
         with
         | Ok () -> ()
         | Error conflict ->
             let fmt = Irmin.Type.pp_json Irmin.Merge.conflict_t in
             Fmt.epr "CONFLICT: %a\n%!" fmt conflict
       in
       let branch_name =
         let doc = Arg.info ~docv:"BRANCH" ~doc:"Branch to merge from." [] in
         Arg.(required & pos 0 (some string) None & doc)
       in
       Term.(mk merge $ store () $ author $ message $ branch_name));
  }

(* PULL *)
let pull =
  {
    name = "pull";
    doc = "Fetch and merge with another repository.";
    man = [];
    term =
      (let pull sr author message =
         Eio.Switch.run @@ fun sw ->
         let S (impl, store, f), remote = sr ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let message = match message with Some s -> s | None -> "pull" in
         let module Sync = Irmin.Sync.Make (S) in
         run @@ fun () ->
         let t = store () in
         let r = remote () in
         let x = apply r f in
         let _ =
           Sync.pull_exn t x (`Merge (info (module S) ?author "%s" message))
         in
         ()
       in
       Term.(mk pull $ remote () $ author $ message));
  }

(* PUSH *)
let push =
  {
    name = "push";
    doc = "Update remote references along with associated objects.";
    man = [];
    term =
      (let push sr =
         Eio.Switch.run @@ fun sw ->
         let S (impl, store, f), remote = sr ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run @@ fun () ->
         let t = store () in
         let r = remote () in
         let x = apply r f in
         let _ = Sync.push_exn t x in
         ()
       in
       Term.(mk push $ remote ()));
  }

(* SNAPSHOT *)
let snapshot =
  {
    name = "snapshot";
    doc = "Return a snapshot for the current state of the database.";
    man = [];
    term =
      (let snapshot store =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         let k = S.Head.get t in
         print "%a" S.Commit.pp_hash k;
         ()
       in
       Term.(mk snapshot $ store ()));
  }

(* REVERT *)
let revert =
  {
    name = "revert";
    doc = "Revert the contents of the store to a previous state.";
    man = [];
    term =
      (let snapshot =
         let doc =
           Arg.info ~docv:"SNAPSHOT" ~doc:"The snapshot to revert to." []
         in
         Arg.(required & pos 0 (some string) None & doc)
       in
       let revert store snapshot =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         let hash = commit S.Hash.t snapshot in
         match S.Commit.of_hash (S.repo t) hash with
         | Some s -> S.Head.set t s
         | None -> failwith "invalid commit"
       in
       Term.(mk revert $ store () $ snapshot));
  }

(* WATCH *)

let run_command (type a b c)
    (module S : Irmin.Generic_key.S
      with type Schema.Path.t = a
       and type Schema.Contents.t = b
       and type Schema.Metadata.t = c) diff command proc =
  let simple_output (k, v) =
    let x =
      match v with `Updated _ -> "*" | `Added _ -> "+" | `Removed _ -> "-"
    in
    print "%s %a" x (Irmin.Type.pp S.Path.t) k;
    Lwt.return_unit
  in
  (* Check if there was a command passed, if not print a simple message to stdout, if there is
     a command pass the whole diff *)
  match command with
  | h :: t ->
      let ty = [%typ: (S.path * (S.contents * S.metadata) Irmin.Diff.t) list] in
      let s = Fmt.str "%a" (Irmin.Type.pp_json ty) diff in
      let make_proc () =
        (* Start new process *)
        let p = Lwt_process.open_process_out (h, Array.of_list (h :: t)) in
        proc := Some p;
        p
      in
      let proc =
        (* Check if process is already running, if not run it *)
        match !proc with
        | None -> make_proc ()
        | Some p -> (
            (* Determine if the subprocess completed succesfully or exited with an error,
               if it was successful then we can restart it, otherwise report the exit code
               the user *)
            let status = p#state in
            match status with
            | Lwt_process.Running -> p
            | Exited (Unix.WEXITED 0) -> make_proc ()
            | Exited (Unix.WEXITED code) ->
                Printf.printf "Subprocess exited with code %d\n" code;
                exit code
            | Exited (Unix.WSIGNALED code) | Exited (Unix.WSTOPPED code) ->
                Printf.printf "Subprocess stopped with code %d\n" code;
                exit code)
      in
      (* Write the diff to the subprocess *)
      let* () = Lwt_io.write_line proc#stdin s in
      Lwt_io.flush proc#stdin
  | [] -> Lwt_list.iter_s simple_output diff

let handle_diff (type a b)
    (module S : Irmin.Generic_key.S
      with type Schema.Path.t = a
       and type commit = b) (path : a) command proc d =
  Lwt_eio.run_lwt @@ fun () ->
  let view (c, _) =
    Lwt_eio.run_eio @@ fun () ->
    let t = S.of_commit c in
    match S.find_tree t path with None -> S.Tree.empty () | Some v -> v
  in
  let* x, y =
    match d with
    | `Updated (x, y) ->
        let* x = view x in
        let+ y = view y in
        (x, y)
    | `Added x ->
        let+ x = view x in
        (S.Tree.empty (), x)
    | `Removed x ->
        let+ x = view x in
        (x, S.Tree.empty ())
  in
  let* (diff : (S.path * (S.contents * S.metadata) Irmin.Diff.t) list) =
    Lwt_eio.run_eio @@ fun () -> S.Tree.diff x y
  in
  run_command
    (module S : Irmin.Generic_key.S
      with type Schema.Path.t = S.path
       and type Schema.Contents.t = S.contents
       and type Schema.Metadata.t = S.metadata)
    diff command proc

let watch =
  {
    name = "watch";
    doc = "Get notifications when values change.";
    man = [];
    term =
      (let watch store path command =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let path = key S.Path.t path in
         let proc = ref None in
         let () =
           at_exit (fun () ->
               match !proc with None -> () | Some p -> p#terminate)
         in
         run @@ fun () ->
         let t = store () in
         let _ =
           S.watch_key t path
             (handle_diff
                (module S : Irmin.Generic_key.S
                  with type Schema.Path.t = S.path
                   and type commit = S.commit)
                path command proc)
         in
         ()
       in
       let command =
         let doc = Arg.info ~docv:"COMMAND" ~doc:"Command to execute" [] in
         Arg.(value & pos_right 0 string [] & doc)
       in
       Term.(mk watch $ store () $ path $ command));
  }

(* DOT *)
let dot =
  {
    name = "dot";
    doc = "Dump the contents of the store as a Graphviz file.";
    man = [];
    term =
      (let basename =
         let doc =
           Arg.info ~docv:"BASENAME"
             ~doc:"Basename for the .dot and .png files." []
         in
         Arg.(required & pos 0 (some string) None & doc)
       in
       let no_dot_call =
         let doc =
           Arg.info
             ~doc:"Do not call the `dot' utility on the generated `.dot` file."
             [ "no-dot-call" ]
         in
         Arg.(value & flag & doc)
       in
       let full =
         let doc =
           Arg.info
             ~doc:
               "Show the full graph of objects, including the filesystem nodes \
                and the content blobs."
             [ "full" ]
         in
         Arg.(value & flag & doc)
       in
       let dot store basename depth no_dot_call full =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         let module Dot = Irmin.Dot (S) in
         let date d =
           let tm = Unix.localtime (Int64.to_float d) in
           Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min
             tm.Unix.tm_sec
         in
         run @@ fun () ->
         let t = store () in
         let call_dot = not no_dot_call in
         let buf = Buffer.create 1024 in
         let () = Dot.output_buffer ~html:false t ?depth ~full ~date buf in
         let oc = open_out_bin (basename ^ ".dot") in
         let () =
           Fun.protect
             (fun () -> output_string oc (Buffer.contents buf))
             ~finally:(fun () -> close_out oc)
         in
         if call_dot then (
           let i = Sys.command "/bin/sh -c 'command -v dot'" in
           if i <> 0 then
             [%logs.err
               "Cannot find the `dot' utility. Please install it on your \
                system and be sure it is available in your $PATH."];
           let i =
             Sys.command
               (Printf.sprintf "dot -Tpng %s.dot -o%s.png" basename basename)
           in
           if i <> 0 then [%logs.err "The %s.dot is corrupted" basename])
       in
       Term.(mk dot $ store () $ basename $ depth $ no_dot_call $ full));
  }

let config_man =
  let version_string = Printf.sprintf "Irmin %s" Irmin.version in
  ( ("irmin.yml", 5, "", version_string, "Irmin Manual"),
    [
      `S Manpage.s_name;
      `P "irmin.yml";
      `S Manpage.s_synopsis;
      `P
        "Configure certain command-line options to cut down on mistakes and \
         save on typing";
      `S Manpage.s_description;
      `P
        "An $(b,irmin.yml) file lets the user specify repetitve command-line \
         options in a YAML file. The $(b,irmin.yml) is read by default if it \
         is found in the current working directory or defined globally as \
         \\$HOME/.config/irmin/config.yml. The configuration file path can \
         also be set using the $(b,--config) command-line flag or by setting \
         \\$XDG_CONFIG_HOME. \n\
        \        The following keys are allowed: $(b,contents), $(b,store), \
         $(b,branch), $(b,root), $(b,bare) or $(b,head). These correspond to \
         the irmin options of the same names. Additionally, specific\n\
        \         backends may have other options available, these can be \
         lised using the $(b,options)\n\
        \         command and applied using the $(b,--opt) flag.";
      `S Manpage.s_examples;
      `P
        "Here is an example $(b,irmin.yml) for accessing a local irmin store. \
         This $(b,irmin.yml) prevents the user from having to specify the \
         $(b,store) and $(b,root) options for every command.";
      `Pre "    \\$ cat irmin.yml\n    store: pack\n    root: /path/to/my/store";
    ]
    @ help_sections )

(* HELP *)
let help =
  {
    name = "help";
    doc = "Display help about Irmin and Irmin commands.";
    man =
      [ `P "Use `$(mname) help topics' to get the full list of help topics." ];
    term =
      (let topic =
         let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
         Arg.(value & pos 0 (some string) None & doc)
       in
       let help man_format cmds topic =
         match topic with
         | None -> `Help (`Pager, None)
         | Some topic -> (
             let topics = "irmin.yml" :: cmds in
             let conv, _ =
               Arg.enum (List.rev_map (fun s -> (s, s)) ("topics" :: topics))
             in
             match conv topic with
             | `Error e -> `Error (false, e)
             | `Ok t when t = "topics" ->
                 List.iter print_endline topics;
                 `Ok ()
             | `Ok t when t = "irmin.yml" ->
                 `Ok
                   (Cmdliner.Manpage.print man_format Format.std_formatter
                      config_man)
             | `Ok t -> `Help (man_format, Some t))
       in
       Term.(ret (mk help $ deprecated_man_format $ Term.choice_names $ topic)));
  }

(* GRAPHQL *)
let graphql =
  {
    name = "graphql";
    doc = "Run a graphql server.";
    man = [];
    term =
      (let port =
         let doc = Arg.info ~doc:"Port for graphql server." [ "p"; "port" ] in
         Arg.(value & opt int 8080 & doc)
       in
       let addr =
         let doc =
           Arg.info ~doc:"Address for graphql server." [ "a"; "address" ]
         in
         Arg.(value & opt string "localhost" & doc)
       in
       let graphql store port addr =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, remote_fn)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let module Server =
           Graphql.Server.Make
             (S)
             (struct
               let remote = remote_fn
             end)
         in
         let t = store () in
         let server = Server.v (S.repo t) in
         let ctx =
           Lwt_eio.run_lwt @@ fun () -> Conduit_lwt_unix.init ~src:addr ()
         in
         let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
         let on_exn exn = [%logs.debug "on_exn: %s" (Printexc.to_string exn)] in
         Lwt_eio.run_lwt @@ fun () ->
         Cohttp_lwt_unix.Server.create ~on_exn ~ctx
           ~mode:(`TCP (`Port port))
           server
       in
       Term.(mk graphql $ store () $ port $ addr));
  }

(* SERVER *)
let server =
  {
    name = "server";
    doc = "Run irmin-server.";
    man = [];
    term =
      (let server main = Eio.Switch.run @@ fun sw -> main ~sw in
       Term.(mk server $ Server.main_term));
  }

let options =
  {
    name = "options";
    doc = "Get information about backend specific configuration options.";
    man = [];
    term =
      (let options (store, hash, contents) =
         let module Conf = Irmin.Backend.Conf in
         let store, _ = Resolver.load_config ?store ?hash ?contents () in
         let spec = Store.spec store in
         Seq.iter
           (fun (Conf.K k) ->
             let name = Conf.name k in
             if name = "root" || name = "uri" then ()
             else
               let ty = Conf.ty k in
               let doc = Conf.doc k |> Option.value ~default:"" in
               let ty =
                 Fmt.str "%a" Irmin.Type.pp_ty ty
                 |> Astring.String.filter (fun c -> c <> '\n')
               in
               Fmt.pr "%s: %s\n\t%s\n" name ty doc)
           (Conf.Spec.keys spec)
       in
       Term.(mk options $ Store.term ()));
  }

let branches =
  {
    name = "branches";
    doc = "List branches";
    man = [];
    term =
      (let branches store =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         let branches = S.Branch.list (S.repo t) in
         List.iter (Fmt.pr "%a\n" (Irmin.Type.pp S.branch_t)) branches
       in
       Term.(mk branches $ store ()));
  }

let weekday Unix.{ tm_wday; _ } =
  match tm_wday with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> assert false

let month Unix.{ tm_mon; _ } =
  match tm_mon with
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> assert false

let log =
  {
    name = "log";
    doc = "List commits";
    man = [];
    term =
      (let plain =
         let doc = Arg.info ~doc:"Show plain text without pager" [ "plain" ] in
         Arg.(value & flag & doc)
       in
       let pager =
         let doc = Arg.info ~doc:"Specify pager program to use" [ "pager" ] in
         Arg.(value & opt string "pager" & doc)
       in
       let num =
         let doc =
           Arg.info ~doc:"Number of entries to show" [ "n"; "max-count" ]
         in
         Arg.(value & opt (some int) None & doc)
       in
       let skip =
         let doc = Arg.info ~doc:"Number of entries to skip" [ "skip" ] in
         Arg.(value & opt (some int) None & doc)
       in
       let reverse =
         let doc = Arg.info ~doc:"Print in reverse order" [ "reverse" ] in
         Arg.(value & flag & doc)
       in
       let exception Return in
       let commits store plain pager num skip reverse =
         Eio.Switch.run @@ fun sw ->
         let (S (impl, store, _)) = store ~sw in
         let (module S) = Store.Impl.generic_keyed impl in
         run @@ fun () ->
         let t = store () in
         let fmt f date =
           Fmt.pf f "%s %s %02d %02d:%02d:%02d %04d" (weekday date) (month date)
             date.tm_mday date.tm_hour date.tm_min date.tm_sec
             (date.tm_year + 1900)
         in
         let repo = S.repo t in
         let skip = ref (Option.value ~default:0 skip) in
         let num = Option.value ~default:0 num in
         let num_count = ref 0 in
         let commit formatter key =
           if num > 0 && !num_count >= num then raise Return
           else if !skip > 0 then decr skip
           else
             let commit = S.Commit.of_key repo key |> Option.get in
             let hash = S.Backend.Commit.Key.to_hash key in
             let info = S.Commit.info commit in
             let date = S.Info.date info in
             let author = S.Info.author info in
             let message = S.Info.message info in
             let date = Unix.localtime (Int64.to_float date) in
             let () =
               Fmt.pf formatter "commit %a\nAuthor: %s\nDate: %a\n\n%s\n\n%!"
                 (Irmin.Type.pp S.hash_t) hash author fmt date message
             in
             incr num_count
         in
         let max =
           let x = S.Head.get t in
           [ `Commit (S.Commit.key x) ]
         in
         let iter ~commit ~max repo =
           try
             if reverse then S.Repo.iter ~commit ~min:[] ~max repo
             else S.Repo.breadth_first_traversal ~commit ~max repo
           with Return -> ()
         in
         if plain then
           let commit = commit Format.std_formatter in
           iter ~commit ~max repo
         else
           try
             let out = Unix.open_process_out pager in
             let commit = commit (Format.formatter_of_out_channel out) in
             let () = iter ~commit ~max repo in
             let _ = Unix.close_process_out out in
             ()
           with Sys_error s when String.equal s "Broken pipe" -> ()
       in
       Term.(mk commits $ store () $ plain $ pager $ num $ skip $ reverse));
  }

let default =
  let doc = "Irmin, the database that never forgets." in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Irmin is a distributed database used primarily for application data. \
         It is designed to work with a large variety of backends and has \
         built-in snapshotting, reverting and branching mechanisms.";
      `P
        "Use either $(mname) <command> --help or $(mname) help <command> for \
         more information on a specific command.";
    ]
  in
  let usage () =
    Fmt.pr
      "usage: irmin [--version]\n\
      \             [--help]\n\
      \             <command> [<args>]\n\n\
       The most commonly used subcommands are:\n\
      \    init        %s\n\
      \    get         %s\n\
      \    set         %s\n\
      \    remove      %s\n\
      \    list        %s\n\
      \    tree        %s\n\
      \    clone       %s\n\
      \    fetch       %s\n\
      \    merge       %s\n\
      \    pull        %s\n\
      \    push        %s\n\
      \    snapshot    %s\n\
      \    revert      %s\n\
      \    watch       %s\n\
      \    dot         %s\n\
      \    graphql     %s\n\
      \    server      %s\n\
      \    options     %s\n\
      \    branches    %s\n\
      \    log         %s\n\n\
       See `irmin help <command>` for more information on a specific command.\n\
       %!"
      init.doc get.doc set.doc remove.doc list.doc tree.doc clone.doc fetch.doc
      merge.doc pull.doc push.doc snapshot.doc revert.doc watch.doc dot.doc
      graphql.doc server.doc options.doc branches.doc log.doc
  in
  ( Term.(mk usage $ const ()),
    deprecated_info "irmin" ~version:Irmin.version ~sdocs:global_option_section
      ~doc ~man )

let commands =
  List.map create_command
    [
      help;
      init;
      get;
      set;
      remove;
      list;
      tree;
      clone;
      fetch;
      merge;
      pull;
      push;
      snapshot;
      revert;
      watch;
      dot;
      graphql;
      server;
      options;
      branches;
      log;
    ]

let run ~default:x y =
  Eio_main.run @@ fun env ->
  Irmin_pack_unix.Io.set_env (Eio.Stdenv.fs env);
  Irmin_fs.run env#fs @@ fun () ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook;
  match deprecated_eval_choice x y with `Error _ -> exit 1 | _ -> ()
