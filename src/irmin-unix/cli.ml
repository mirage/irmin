(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let () = Hook.init ()

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
  Term.info ~sdocs:global_option_section ~docs:global_option_section ~doc ~man
    title

type command = unit Term.t * Term.info

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

let run t = Lwt_main.run (Lwt.catch (fun () -> t) print_exc)
let mk (fn : 'a) : 'a Term.t = Term.(const (fun () -> fn) $ setup_log)

(* INIT *)
let init =
  {
    name = "init";
    doc = "Initialize a store.";
    man = [];
    term =
      (let init (S (_, _store, _)) = run Lwt.return_unit in
       Term.(mk init $ store ()));
  }

(* HTTP *)
let http =
  {
    name = "http";
    doc = "Run http server";
    man = [];
    term =
      (let uri =
         let doc =
           Arg.info ~docv:"URI" [ "a"; "address" ]
             ~doc:
               "Start the Irmin server on the given socket address. Examples \
                include http://localhost:8080 and launchd://Listener."
         in
         Arg.(value & opt string "http://localhost:8080" & doc)
       in
       let init (S (impl, store, _)) uri =
         let (module S) =
           match Store.Impl.hash_keyed impl with
           | Some x -> x
           | None ->
               Fmt.failwith
                 "Unsupported backend: can't start an HTTP server with a store \
                  that is not keyed by hashes"
         in
         run
           (let* t = store in
            let module HTTP = Http.Server (S) in
            let uri = Uri.of_string uri in
            let spec = HTTP.v (S.repo t) in
            match Uri.scheme uri with
            | Some "launchd" ->
                let uri, name =
                  match Uri.host uri with
                  | None -> (Uri.with_host uri (Some "Listener"), "Listener")
                  | Some name -> (uri, name)
                in
                [%logs.info "daemon: %s" (Uri.to_string uri)];
                Cohttp_lwt_unix.Server.create ~timeout:3600
                  ~mode:(`Launchd name) spec
            | _ ->
                let uri =
                  match Uri.host uri with
                  | None -> Uri.with_host uri (Some "localhost")
                  | Some _ -> uri
                in
                let port, uri =
                  match Uri.port uri with
                  | None -> (8080, Uri.with_port uri (Some 8080))
                  | Some p -> (p, uri)
                in
                [%logs.info "daemon: %s" (Uri.to_string uri)];
                Printf.printf "Server starting on port %d.\n%!" port;
                Cohttp_lwt_unix.Server.create ~timeout:3600
                  ~mode:(`TCP (`Port port))
                  spec)
       in
       Term.(mk init $ store () $ uri));
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
      (let get (S (impl, store, _)) path =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            S.find t (key S.Path.t path) >>= function
            | None ->
                print "<none>";
                exit 1
            | Some v ->
                print "%a" (Irmin.Type.pp S.Contents.t) v;
                Lwt.return_unit)
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
      (let list (S (impl, store, _)) path_or_empty =
         let (module S) = Store.Impl.generic_keyed impl in
         let path =
           match path_or_empty with
           | Empty -> S.Path.empty
           | Path str -> key S.Path.t str
         in
         run
           (let* t = store in
            let* paths = S.list t path in
            let pp_step = Irmin.Type.pp S.Path.step_t in
            let pp ppf (s, k) =
              match S.Tree.destruct k with
              | `Contents _ -> Fmt.pf ppf "FILE %a" pp_step s
              | `Node _ -> Fmt.pf ppf "DIR %a" pp_step s
            in
            List.iter (print "%a" pp) paths;
            Lwt.return_unit)
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
      (let tree (S (impl, store, _)) =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            let all = ref [] in
            let todo = ref [ S.Path.empty ] in
            let rec walk () =
              match !todo with
              | [] -> Lwt.return_unit
              | k :: rest ->
                  todo := rest;
                  let* childs = S.list t k in
                  Lwt_list.iter_p
                    (fun (s, c) ->
                      let k = S.Path.rcons k s in
                      match S.Tree.destruct c with
                      | `Node _ ->
                          todo := k :: !todo;
                          Lwt.return_unit
                      | `Contents _ ->
                          let+ v = S.get t k in
                          all := (k, v) :: !all)
                    childs
                  >>= walk
            in
            walk () >>= fun () ->
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
              all;
            Lwt.return_unit)
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
       let set (S (impl, store, _)) author message path v =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let message = match message with Some s -> s | None -> "set" in
            let* t = store in
            let path = key S.Path.t path in
            let value = value S.Contents.t v in
            S.set_exn t ~info:(info (module S) ?author "%s" message) path value)
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
      (let remove (S (impl, store, _)) author message path =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let message =
              match message with Some s -> s | None -> "remove " ^ path
            in
            let* t = store in
            S.remove_exn t
              ~info:(info (module S) ?author "%s" message)
              (key S.Path.t path))
       in
       Term.(mk remove $ store () $ author $ message $ path));
  }

let apply e f =
  match (e, f) with
  | R (h, e), Some f -> f ?ctx:None ?headers:h e
  | R _, None -> Fmt.failwith "invalid remote for that kind of store"
  | r, _ -> r

(* CLONE *)
let clone =
  {
    name = "clone";
    doc = "Copy a remote respository to a local store";
    man = [];
    term =
      (let clone (S (impl, store, f)) remote depth =
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run
           (let* t = store in
            let* r = remote in
            Sync.fetch t ?depth (apply r f) >>= function
            | Ok (`Head d) -> S.Head.set t d
            | Ok `Empty -> Lwt.return_unit
            | Error (`Msg e) -> failwith e)
       in
       Term.(mk clone $ store () $ remote $ depth));
  }

(* FETCH *)
let fetch =
  {
    name = "fetch";
    doc = "Download objects and refs from another repository.";
    man = [];
    term =
      (let fetch (S (impl, store, f)) remote =
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run
           (let* t = store in
            let* r = remote in
            let branch = branch S.Branch.t "import" in
            let* t = S.of_branch (S.repo t) branch in
            let* _ = Sync.pull_exn t (apply r f) `Set in
            Lwt.return_unit)
       in
       Term.(mk fetch $ store () $ remote));
  }

(* MERGE *)
let merge =
  {
    name = "merge";
    doc = "Merge branches.";
    man = [];
    term =
      (let merge (S (impl, store, _)) author message branch =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let message = match message with Some s -> s | None -> "merge" in
            let branch =
              match Irmin.Type.of_string S.Branch.t branch with
              | Ok b -> b
              | Error (`Msg msg) -> failwith msg
            in
            let* t = store in
            S.merge_with_branch t branch
              ~info:(info (module S) ?author "%s" message)
            >|= function
            | Ok () -> ()
            | Error conflict ->
                let fmt = Irmin.Type.pp_json Irmin.Merge.conflict_t in
                Fmt.epr "CONFLICT: %a\n%!" fmt conflict)
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
      (let pull (S (impl, store, f)) author message remote =
         let (module S) = Store.Impl.generic_keyed impl in
         let message = match message with Some s -> s | None -> "pull" in
         let module Sync = Irmin.Sync.Make (S) in
         run
           (let* t = store in
            let* r = remote in
            let* _ =
              Sync.pull_exn t (apply r f)
                (`Merge (info (module S) ?author "%s" message))
            in
            Lwt.return_unit)
       in
       Term.(mk pull $ store () $ author $ message $ remote));
  }

(* PUSH *)
let push =
  {
    name = "push";
    doc = "Update remote references along with associated objects.";
    man = [];
    term =
      (let push (S (impl, store, f)) remote =
         let (module S) = Store.Impl.generic_keyed impl in
         let module Sync = Irmin.Sync.Make (S) in
         run
           (let* t = store in
            let* r = remote in
            let* _ = Sync.push_exn t (apply r f) in
            Lwt.return_unit)
       in
       Term.(mk push $ store () $ remote));
  }

(* SNAPSHOT *)
let snapshot =
  {
    name = "snapshot";
    doc = "Return a snapshot for the current state of the database.";
    man = [];
    term =
      (let snapshot (S (impl, store, _)) =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            let* k = S.Head.get t in
            print "%a" S.Commit.pp_hash k;
            Lwt.return_unit)
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
       let revert (S (impl, store, _)) snapshot =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            let hash = commit S.Hash.t snapshot in
            let* s = S.Commit.of_hash (S.repo t) hash in
            match s with
            | Some s -> S.Head.set t s
            | None -> failwith "invalid commit")
       in
       Term.(mk revert $ store () $ snapshot));
  }

(* WATCH *)
let watch =
  {
    name = "watch";
    doc = "Get notifications when values change.";
    man = [];
    term =
      (let watch (S (impl, store, _)) path =
         let (module S) = Store.Impl.generic_keyed impl in
         let path = key S.Path.t path in
         run
           (let* t = store in
            let* _ =
              S.watch_key t path (fun d ->
                  let pr (k, v) =
                    let v =
                      match v with
                      | `Updated _ -> "*"
                      | `Added _ -> "+"
                      | `Removed _ -> "-"
                    in
                    print "%s%a" v (Irmin.Type.pp S.Path.t) k
                  in
                  let view (c, _) =
                    let* t = S.of_commit c in
                    S.find_tree t path >|= function
                    | None -> S.Tree.empty ()
                    | Some v -> v
                  in
                  let empty = Lwt.return (S.Tree.empty ()) in
                  let x, y =
                    match d with
                    | `Updated (x, y) -> (view x, view y)
                    | `Added x -> (empty, view x)
                    | `Removed x -> (view x, empty)
                  in
                  let* x = x in
                  let* y = y in
                  let* diff = S.Tree.diff x y in
                  List.iter pr diff;
                  Lwt.return_unit)
            in
            let t, _ = Lwt.task () in
            t)
       in
       Term.(mk watch $ store () $ path));
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
       let dot (S (impl, store, _)) basename depth no_dot_call full =
         let (module S) = Store.Impl.generic_keyed impl in
         let module Dot = Irmin.Dot (S) in
         let date d =
           let tm = Unix.localtime (Int64.to_float d) in
           Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min
             tm.Unix.tm_sec
         in
         run
           (let* t = store in
            let call_dot = not no_dot_call in
            let buf = Buffer.create 1024 in
            Dot.output_buffer ~html:false t ?depth ~full ~date buf >>= fun () ->
            let oc = open_out_bin (basename ^ ".dot") in
            let* () =
              Lwt.finalize
                (fun () ->
                  output_string oc (Buffer.contents buf);
                  Lwt.return_unit)
                (fun () ->
                  close_out oc;
                  Lwt.return_unit)
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
              if i <> 0 then [%logs.err "The %s.dot is corrupted" basename]);
            Lwt.return_unit)
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
        "Here is an example $(b,irmin.yml) for accessing a local http irmin \
         store. This $(b,irmin.yml) prevents the user from having to specify \
         the $(b,store) and $(b,root) options for every command.";
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
       Term.(ret (mk help $ Term.man_format $ Term.choice_names $ topic)));
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
       let graphql (S (impl, store, remote_fn)) port addr =
         let (module S) =
           match Store.Impl.hash_keyed impl with
           | Some x -> x
           | None ->
               Fmt.failwith
                 "Unsupported backend: can't start a GraphQL server with a \
                  store that is not keyed by hashes"
         in
         run
           (let module Server =
              Graphql.Server.Make
                (S)
                (struct
                  let remote = remote_fn
                end)
            in
           let* t = store in
           let server = Server.v (S.repo t) in
           let* ctx = Conduit_lwt_unix.init ~src:addr () in
           let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
           let on_exn exn =
             [%logs.debug "on_exn: %s" (Printexc.to_string exn)]
           in
           Cohttp_lwt_unix.Server.create ~on_exn ~ctx
             ~mode:(`TCP (`Port port))
             server)
       in
       Term.(mk graphql $ store () $ port $ addr));
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
      (let branches (S (impl, store, _)) =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            let+ branches = S.Branch.list (S.repo t) in
            List.iter (Fmt.pr "%a\n" (Irmin.Type.pp S.branch_t)) branches)
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
       let commits (S (impl, store, _)) plain pager =
         let (module S) = Store.Impl.generic_keyed impl in
         run
           (let* t = store in
            let fmt f date =
              Fmt.pf f "%s %s %02d %02d:%02d:%02d %04d" (weekday date)
                (month date) date.tm_mday date.tm_hour date.tm_min date.tm_sec
                (date.tm_year + 1900)
            in
            let repo = S.repo t in
            let commit formatter key =
              let+ commit = S.Commit.of_key repo key >|= Option.get in
              let hash = S.Backend.Commit.Key.to_hash key in
              let info = S.Commit.info commit in
              let date = S.Info.date info in
              let author = S.Info.author info in
              let message = S.Info.message info in
              let date = Unix.localtime (Int64.to_float date) in
              Fmt.pf formatter "commit %a\nAuthor: %s\nDate: %a\n\n%s\n\n%!"
                (Irmin.Type.pp S.hash_t) hash author fmt date message
            in
            let* max = S.Head.get t >|= fun x -> [ `Commit (S.Commit.key x) ] in
            if plain then
              let commit = commit Format.std_formatter in
              S.Repo.breadth_first_traversal ~commit ~max repo
            else
              Lwt.catch
                (fun () ->
                  let out = Unix.open_process_out pager in
                  let commit = commit (Format.formatter_of_out_channel out) in
                  let+ () = S.Repo.breadth_first_traversal ~commit ~max repo in
                  let _ = Unix.close_process_out out in
                  ())
                (function
                  | Sys_error s when String.equal s "Broken pipe" ->
                      Lwt.return_unit
                  | exn -> raise exn))
       in
       Term.(mk commits $ store () $ plain $ pager));
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
      \    http        %s\n\
      \    options     %s\n\
      \    branches    %s\n\
      \    log         %s\n\n\
       See `irmin help <command>` for more information on a specific command.\n\
       %!"
      init.doc get.doc set.doc remove.doc list.doc tree.doc clone.doc fetch.doc
      merge.doc pull.doc push.doc snapshot.doc revert.doc watch.doc dot.doc
      graphql.doc http.doc options.doc branches.doc log.doc
  in
  ( Term.(mk usage $ const ()),
    Term.info "irmin" ~version:Irmin.version ~sdocs:global_option_section ~doc
      ~man )

let commands =
  List.map create_command
    [
      help;
      init;
      http;
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
      options;
      branches;
      log;
    ]

let run ~default:x y =
  match Cmdliner.Term.eval_choice x y with `Error _ -> exit 1 | _ -> ()
