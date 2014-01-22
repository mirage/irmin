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

let uri_conv =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf v = pr_str ppf (Uri.to_string v) in
  parse, print

(*
let blob_conv (type t) (module B: IrminBlob.S with type t = t) =
  let parse str = `Ok (B.of_bytes str) in
  let print ppf v = pr_str ppf (B.to_string v) in
  parse, print

let ref_conv (type t) (module R: IrminReference.S with type t = t) =
  let parse str = `Ok (R.of_pretty str) in
  let print ppf tag = pr_str ppf (R.pretty tag) in
  parse, print
*)

let key =
  let module K = IrminKey.SHA1 in
  let key_conv =
    let parse str =
      try `Ok (K.of_string str)
      with e -> `Error "Invalid key" in
    let print ppf key = pr_str ppf (K.to_string key) in
    parse, print in
  let doc = Arg.info ~docv:"KEY" ~doc:"SHA1 key." [] in
  Arg.(required & pos 0 (some key_conv) None & doc)

let path_conv =
  let parse str = `Ok (IrminPath.of_string str) in
  let print ppf path = pr_str ppf (IrminPath.to_string path) in
  parse, print

let path =
  let doc = Arg.info ~docv:"PATH" ~doc:"Path." [] in
  Arg.(value & pos 0 path_conv [] & doc)

let default_dir = ".irmin"

(* XXX: ugly hack *)
let init_hook =
  ref (fun () -> ())

let in_memory_store () =
  Log.info (lazy "source: in-memory");
  (module IrminMemory.Simple: Irmin.SIMPLE)

let local_store dir =
  Log.infof "source: dir=%s" dir;
  init_hook := (fun () -> if not (Sys.file_exists dir) then Unix.mkdir dir 0o755);
  IrminFS.simple dir

let remote_store uri =
  let module CRUD = IrminCRUD.Make(Cohttp_lwt_unix.Client) in
  Log.infof "source: uri=%s" (Uri.to_string uri);
  CRUD.simple uri

let git_store () =
  Log.infof "git";
  (module IrminGit.Simple(GitLocal): Irmin.SIMPLE)

let store_of_string str =
  let open Core_kernel.Std in
  if String.length str > 0 then
    match str.[0] with
    | 'm' -> Some (in_memory_store ())
    | 'g' -> Some (git_store ())
    | 'l' ->
      let dir = match String.chop_prefix ~prefix:"l:"str with
        | None   -> Sys.getcwd ()
        | Some d -> d in
      Some (local_store dir)
    | 'r' ->
      let uri = match String.chop_prefix ~prefix:"r:" str with
        | None   -> "http://127.0.0.1:8080"
        | Some u -> u in
      Some (uri |> Uri.of_string |> remote_store)
    | _   ->
      Printf.eprintf "%s is not a store specification\n%!" str;
      None
  else
    None

let store_of_env_var () =
  try store_of_string (Sys.getenv "IRMIN")
  with Not_found -> None

let store =
  let in_memory =
    let doc =
      Arg.info ~doc:"In-memory persistence."
        ["m";"in-memory"] in
    Arg.(value & flag & doc) in
  let local =
    let doc =
      Arg.info ~docv:"PATH" ~doc:"Local store." ["l";"local"] in
    Arg.(value & opt (some string) None & doc) in
  let remote =
    let doc =
      Arg.info ~docv:"URI" ~doc:"Remote store." ["r";"remote"] in
    Arg.(value & opt (some uri_conv) None & doc) in
  let git =
    let doc =
      Arg.info ~doc:"Local Git store." ["g";"git"] in
    Arg.(value & flag & doc) in
  let create git in_memory local remote =
    if git || in_memory || local <> None || remote <> None  then
      match git, in_memory, local, remote with
      | true , false, None   , None   -> git_store ()
      | false, true , None   , None   -> in_memory_store ()
      | false, false, None   , Some u -> remote_store u
      | false, false, Some d , None   -> local_store (Filename.concat d default_dir)
      | false, false, None   , None   -> local_store default_dir
      | _ ->
        let local = match local with None -> "<none>" | Some d -> d in
        let remote = match remote with None -> "<none>" | Some u -> Uri.to_string u in
        failwith (Printf.sprintf
                    "Invalid store source [git=%b in-memory=%b %s %s]"
                    git in_memory local remote)
    else match store_of_env_var () with
      | None   -> git_store ()
      | Some s -> s
  in
  Term.(pure create $ git $ in_memory $ local $ remote)

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
      let doc = Arg.info ~doc:"Start an Irminsule server." ["d";"daemon"] in
      Arg.(value & flag & doc) in
    let port =
      let doc =
        Arg.info ~docv:"PORT" ["a";"address"]
          ~doc:"Start the Irminsule server on the given port (to use with --daemon)." in
      Arg.(value & opt int 8080 & doc) in
    let init (module S: Irmin.SIMPLE) daemon port =
      run begin
        S.create () >>= fun t ->
        !init_hook ();
        match daemon, port with
        | false, _   -> return_unit
        | true , port ->
          let uri = Uri.of_string ("http://127.0.0.1:" ^ string_of_int port) in
          Log.infof "daemon: %s" (Uri.to_string uri);
          IrminHTTP.start_server (module S) t uri
      end
    in
    Term.(mk init $ store $ daemon $ port)
}

let print fmt =
  Printf.ksprintf print_endline fmt

(* READ *)
let read = {
  name = "read";
  doc  = "Read the contents of a node.";
  man  = [];
  term =
    let read (module S: Irmin.SIMPLE) path =
      run begin
        S.create ()   >>= fun t ->
        S.read t path >>= function
        | None   -> print "<none>"; exit 1
        | Some v -> print "%s" (S.Value.to_string v); return_unit
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
    let ls (module S: Irmin.SIMPLE) path =
      run begin
        S.create ()   >>= fun t ->
        S.list t path >>= fun paths ->
        List.iter (fun p -> print "%s" (IrminPath.to_string p)) paths;
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
  let tree (module S: Irmin.SIMPLE) =
    run begin
      S.create () >>= fun t ->
      S.contents t >>= fun all ->
      let all = List.map (fun (k,v) -> IrminPath.to_string k, Printf.sprintf "%S" (S.Value.to_string v)) all in
      let max_lenght l =
        List.fold_left (fun len s -> max len (String.length s)) 0 l in
      let k_max = max_lenght (List.map fst all) in
      let v_max = max_lenght (List.map snd all) in
      let pad = 79 + k_max + v_max in
      List.iter (fun (k,v) ->
          let dots = String.make (pad - String.length k - String.length v) '.' in
          print "/%s%s%s" k dots v
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
    let write (module S: Irmin.SIMPLE) args =
      let path, value = match args with
        | []            -> failwith "Not enough arguments"
        | [path; value] -> IrminPath.of_string path, S.Value.of_bytes_exn value
        | [value]       -> [], S.Value.of_bytes_exn value
        | _             -> failwith "Too many arguments" in
      run begin
        S.create () >>= fun t ->
        S.update t path value
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
    let rm (module S: Irmin.SIMPLE) path =
      run begin
        S.create () >>= fun t ->
        S.remove t path
      end
    in
    Term.(mk rm $ store $ path);
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
        print "Cloning %d bytes" (R.Dump.bin_size_t dump);
        L.import local dump >>= fun ()     ->
        L.revert local tag
      end
    in
    Term.(mk clone $ store);
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
        print "Pulling %d bytes" (R.Dump.bin_size_t dump);
        L.import local dump >>= fun ()     ->
        (* XXX: deal with merge conflicts properly. *)
        match dump with
        | [] -> return_unit
        | _  -> L.revert local r
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
        print "Pushing %d bytes" (R.Dump.bin_size_t dump);
        R.import remote dump >>= fun ()     ->
        (* XXX: deal with merge conflicts properly. *)
        match dump with
        | [] -> return_unit
        | _  -> L.revert local r
      end
    in
    Term.(mk push $ store);
}

(* SNAPSHOT *)
let snapshot = {
  name = "snapshot";
  doc  = "Snapshot the contents of the store.";
  man  = [];
  term =
    let snapshot (module S: Irmin.SIMPLE) =
      run begin
        S.create ()  >>= fun t ->
        S.snapshot t >>= fun k ->
        print "%s" (S.Snapshot.to_string k);
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
    let revert (module S: Irmin.SIMPLE) key =
      run begin
        S.create () >>= fun t ->
        S.revert t key
      end
    in
    Term.(mk revert $ store $ key)
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
    let watch (module S: Irmin.SIMPLE) path =
      run begin
        S.create () >>= fun t ->
        let stream = S.watch t path in
        Lwt_stream.iter_s (fun (path, rev) ->
            print "%s %s" (IrminPath.to_string path) (S.Snapshot.to_string rev);
            return_unit
          ) stream
      end
    in
    Term.(mk watch $ store $ path)
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

let commands = List.map create_command [
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
