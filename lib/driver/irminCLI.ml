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
  Irmin.set_date_hook Unix.time;
  Irmin.set_origin_hook (fun () -> origin);
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

let uri_conv =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf v = pr_str ppf (Uri.to_string v) in
  parse, print

let path_conv =
  let parse str = `Ok (IrminPath.of_string str) in
  let print ppf path = pr_str ppf (IrminPath.to_string path) in
  parse, print

let path =
  let doc = Arg.info ~docv:"PATH" ~doc:"Path." [] in
  Arg.(value & pos 0 path_conv [] & doc)

let repository =
  let doc = Arg.info ~docv:"REPOSITORY"
      ~doc:"The (possibly remote) repository to clone from." [] in
  Arg.(required & pos 0 (some string) None & doc)

let default_dir = ".irmin"

(* XXX: ugly hack *)
let init_hook =
  ref (fun () -> ())

let modules x: (module IrminKey.S) * (module IrminContents.S) * (module IrminReference.S) =
  match x with
  | `String -> (module IrminKey.SHA1), (module IrminContents.String), (module IrminReference.String)
  | `JSON   -> (module IrminKey.SHA1), (module IrminContents.JSON)  , (module IrminReference.String)

let in_memory_store (type key) k =
  Log.info (lazy "source: in-memory");
  let (module K), (module C), (module R) = modules k in
  let module M = IrminMemory.Make(K)(C)(R) in
  M.(cast (create ()))

let local_store k dir =
  Log.infof "source: dir=%s" dir;
  init_hook := (fun () -> if not (Sys.file_exists dir) then Unix.mkdir dir 0o755);
  let (module K), (module C), (module R) = modules k in
  let module M = IrminFS.Make(K)(C)(R) in
  M.(cast (create dir))

let remote_store k uri =
  let module CRUD_ = IrminCRUD.Make(Cohttp_lwt_unix.Client) in
  let (module K), (module C), (module R) = modules k in
  let module CRUD = CRUD_.Make(K)(C)(R) in
  Log.infof "source: uri=%s" (Uri.to_string uri);
  CRUD.(cast (create uri))

let git_store k g =
  Log.infof "git";
  let (module K), (module C), (module R) = modules k in
  let module M = IrminGit.Make(K)(C)(R) in
  M.(cast (create ~kind:g ~bare:false ()))

let store_of_string str =
  let open Core_kernel.Std in
  let prefix, suffix =
    match String.split ~on:':' str with
    | []   -> str, None
    | [h]  -> h  , None
    | h::t -> h  , Some (String.concat ~sep:":" t) in
  let json = if String.mem prefix 'j' then `JSON else `String in
  let mem = String.mem prefix 'm' in
  let git = String.mem prefix 'g' in
  let local = String.mem prefix 'l' in
  let remote = String.mem prefix 'r' in
  match mem, git, local, remote with
  | true , false, false, false -> Some (in_memory_store json)
  | _    , true , false, false -> Some (git_store json (if mem then `Memory else `Disk))
  | false, false, true , false ->
    let dir = match suffix with
      | None   -> default_dir
      | Some d -> Filename.concat d default_dir in
    Some (local_store json dir)
  | false, false, false, true  ->
    let uri = match suffix with
      | None   -> "http://localhost:8080"
      | Some u -> u in
    Some (uri |> Uri.of_string |> remote_store json)
  | _   ->
    eprintf "%s is not a valid store specification\n%!" str;
    None

let store_of_string_exn str =
  match store_of_string str with
  | None   -> failwith "store_of_string"
  | Some s -> s

let store_of_env_var () =
  try store_of_string (Sys.getenv "IRMIN")
  with Not_found -> None

let store =
  let json =
    let doc = Arg.info ~doc:"Use JSON values." ["j";"json"] in
    Arg.(value & flag & doc) in
  let in_memory =
    let doc =
      Arg.info ~doc:"In-memory persistence." ["m";"in-memory"] in
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
  let create json git in_memory local remote =
    let json = if json then `JSON else `String in
    if git || in_memory || local <> None || remote <> None  then
      match git, in_memory, local, remote with
      | true , _    , None   , None   -> git_store json (if in_memory then `Memory else `Disk)
      | false, true , None   , None   -> in_memory_store json
      | false, false, None   , Some u -> remote_store json u
      | false, false, Some d , None   -> local_store json (Filename.concat d default_dir)
      | false, false, None   , None   -> local_store json default_dir
      | _ ->
        let local = match local with None -> "<none>" | Some d -> d in
        let remote = match remote with None -> "<none>" | Some u -> Uri.to_string u in
        failwith (sprintf
                    "Invalid store source [git=%b in-memory=%b %s %s]"
                    git in_memory local remote)
    else match store_of_env_var () with
      | None   -> local_store json default_dir
      | Some s -> s
  in
  Term.(pure create $ json $ git $ in_memory $ local $ remote)

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
        !init_hook ();
        if daemon then
          let uri = Uri.of_string uri in
          Log.infof "daemon: %s" (Uri.to_string uri);
          IrminHTTP.start_server (module S) t uri
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
    let read (module S: Irmin.S) path =
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
    let ls (module S: Irmin.S) path =
      run begin
        S.create ()   >>= fun t ->
        S.list t path >>= fun paths ->
        List.iter ~f:(fun p -> print "%s" (IrminPath.to_string p)) paths;
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
    Term.(mk write $ store $ args);
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
    Term.(mk rm $ store $ path);
}

let convert_dump
    (type a) (type b) (module L: Irmin.S with type Internal.key = a and type value = b)
    (type c) (type d) (module R: Irmin.S with type Internal.key = c and type value = d)
    (dump: R.dump): L.dump =
  let key k = L.Internal.Key.of_string (R.Internal.Key.to_string k) in
  let value v = L.Internal.Value.of_string (R.Internal.Value.to_string v) in
  let head = match dump.IrminDump.head with
    | None   -> None
    | Some k -> Some (key k) in
  let store = List.map ~f:(fun (k,v) -> key k, value v) dump.IrminDump.store in
  { IrminDump.head; store }

(* CLONE *)
let clone = {
  name = "clone";
  doc  = "Clone a repository into a new store.";
  man  = [];
  term =
    let clone (module L: Irmin.S) repository =
      !init_hook ();
      let (module R) = store_of_string_exn repository in
      run begin
        L.create ()         >>= fun local  ->
        R.create ()         >>= fun remote ->
        R.snapshot remote   >>= fun tag    ->
        R.export remote []  >>= fun dump   ->
        print "Cloning %d bytes" (R.Dump.bin_size_t dump);
        let dump = convert_dump (module L) (module R) dump in
        L.import local L.Reference.Key.master dump
      end
    in
    Term.(mk clone $ store $ repository);
}

let op_repo op (module L: Irmin.S) (module R: Irmin.S) branch =
  L.create ()         >>= fun local  ->
  R.create ()         >>= fun remote ->
  L.snapshot local    >>= fun l      ->
  let l = R.Internal.Key.of_string (L.Internal.Key.to_string l) in
  R.snapshot remote   >>= fun r      ->
  let r = L.Internal.Key.of_string (R.Internal.Key.to_string l) in
  R.export remote [l] >>= fun dump   ->
  print "%sing %d bytes" op (R.Dump.bin_size_t dump);
  let dump = convert_dump (module L) (module R) dump in
  let branch = L.Reference.Key.of_string "import" in
  L.import local branch dump >>= fun () ->
  return (L.Internal.Key.to_string r)

let fetch_repo local remote branch =
  op_repo "Fetch" local remote branch

(* FETCH *)
let fetch = {
  name = "fetch";
  doc  = "Download objects and refs from another repository.";
  man  = [];
  term =
    let fetch (module L: Irmin.S) repository =
      let remote = store_of_string_exn repository in
      run begin
        let branch = L.Reference.Key.of_string "FETCH_HEAD" in
        fetch_repo (module L) remote branch >>= fun _ ->
        return_unit
      end
    in
    Term.(mk fetch $ store $ repository);
}

(* PULL *)
let pull = {
  name = "pull";
  doc  = "Fetch and merge with another repository.";
  man  = [];
  term =
    let pull (module L: Irmin.S) repository =
      let remote = store_of_string_exn repository in
      run begin
        let branch = L.Reference.Key.of_string "FETCH_HEAD" in
        fetch_repo (module L) remote branch >>= fun r ->
        let r = L.Internal.Key.of_string r in
        L.create () >>= fun t ->
        (* XXX: implement merge
           L.Reference.(read_exn (L.reference l) Key.master) >>= fun l ->
           match S.merge t r l with
          | None   -> failwith "Conflict!"
          | Some m -> *)
        L.Reference.(update (L.reference t) Key.master r)
      end
    in
    Term.(mk pull $ store $ repository);
}

let push_repo = op_repo "Push"

(* PUSH *)
let push = {
  name = "push";
  doc  = "Update remote references along with associated objects.";
  man  = [];
  term =
    let push local repository =
      let (module R) = store_of_string_exn repository in
      let name = R.Reference.Key.to_string R.Reference.Key.master in
      run begin
        push_repo (module R) local name >>= fun _ ->
        return_unit
      end
    in
    Term.(mk push $ store $ repository);
}

(* SNAPSHOT *)
let snapshot = {
  name = "snapshot";
  doc  = "Snapshot the contents of the store.";
  man  = [];
  term =
    let snapshot (module S: Irmin.S) =
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
    let revision =
      let doc = Arg.info ~docv:"REVISION" ~doc:"The revision to revert to." [] in
      Arg.(required & pos 0 (some string) None & doc) in
    let revert (module S: Irmin.S) revision =
      let revision = S.Internal.Key.of_string revision in
      run begin
        S.create () >>= fun t ->
        S.revert t revision
      end
    in
    Term.(mk revert $ store $ revision)
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
    let dump (module S: Irmin.S) basename =
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
