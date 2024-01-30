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
open Astring
module Xgit = Irmin_git_unix

let global_option_section = "COMMON OPTIONS"

module Conf = Irmin.Backend.Conf

let try_parse ty v =
  match Irmin.Type.of_string ty v with
  | Error e -> (
      let x = Format.sprintf "{\"some\": %s}" v in
      match Irmin.Type.of_string ty x with
      | Error _ ->
          let y = Format.sprintf "{\"some\": \"%s\"}" v in
          Irmin.Type.of_string ty y |> Result.map_error (fun _ -> e)
      | v -> v)
  | v -> v

let pconv t =
  let pp = Irmin.Type.pp t in
  let parse s =
    match try_parse t s with Ok x -> `Ok x | Error (`Msg e) -> `Error e
  in
  (parse, pp)

let config_path_term =
  let name = "config" in
  let doc = "Allows configuration file to be specified on the command-line." in
  let docv = "PATH" in
  let docs = "COMMON OPTIONS" in
  let mk = pconv Irmin.Type.(option string) in
  let i = Arg.info ~docv ~docs ~doc [ name ] in
  Arg.(value & opt mk None i)

let root_term =
  let name = "root" in
  let doc = "The location of the Irmin store on disk." in
  let docv = "PATH" in
  let docs = "COMMON OPTIONS" in
  let mk = pconv Irmin.Type.(option string) in
  let i = Arg.info ~docv ~docs ~doc [ name ] in
  Arg.(value & opt mk None i)

let ( / ) = Filename.concat
let global_config_path = "irmin" / "config.yml"

(* Contents *)

module Contents = struct
  type t = (module Irmin.Contents.S)

  let all =
    ref
      [
        ("string", (module Irmin.Contents.String : Irmin.Contents.S));
        ("json", (module Irmin.Contents.Json));
        ("json-value", (module Irmin.Contents.Json_value));
      ]

  let default = "string" |> fun n -> ref (n, List.assoc n !all)

  let add name ?default:(x = false) m =
    all := (name, m) :: !all;
    if x then default := (name, m)

  let find name =
    match List.assoc_opt (String.Ascii.lowercase name) !all with
    | Some c -> c
    | None ->
        let valid = String.concat ~sep:", " (List.split !all |> fst) in
        let msg =
          Printf.sprintf "Invalid content type: %s. Expected one of: %s." name
            valid
        in
        failwith msg

  let term () =
    let content_types = !all |> List.map (fun (name, _) -> (name, name)) in
    let kind =
      let doc =
        Fmt.str "The type of user-defined contents (%s). Default is `%s'."
          (Arg.doc_alts_enum content_types)
          (fst !default)
      in
      let arg_info =
        Arg.info ~doc ~docs:global_option_section [ "contents"; "c" ]
      in
      Arg.(value & opt (some string) None & arg_info)
    in
    let create kind = kind in
    Term.(const create $ kind)
end

type contents = Contents.t

module Hash = struct
  type t = (module Irmin.Hash.S)
  type hash_function = Fixed of t | Variable_size of (int option -> t)

  module type SIZEABLE = functor
    (S : sig
       val digest_size : int
     end)
    -> Irmin.Hash.S

  let variable_size (module Make : SIZEABLE) (module Default : Irmin.Hash.S) =
    Variable_size
      (function
      | Some s ->
          (module struct
            include Make (struct
              let digest_size = s
            end)
          end : Irmin.Hash.S)
      | None -> (module Default))

  let all =
    ref
      [
        ( "blake2b",
          variable_size
            (module Irmin.Hash.Make_BLAKE2B : SIZEABLE)
            (module Irmin.Hash.BLAKE2B : Irmin.Hash.S) );
        ( "blake2s",
          variable_size
            (module Irmin.Hash.Make_BLAKE2S : SIZEABLE)
            (module Irmin.Hash.BLAKE2S : Irmin.Hash.S) );
        ("rmd160", Fixed (module Irmin.Hash.RMD160 : Irmin.Hash.S));
        ("sha1", Fixed (module Irmin.Hash.SHA1 : Irmin.Hash.S));
        ("sha224", Fixed (module Irmin.Hash.SHA224 : Irmin.Hash.S));
        ("sha256", Fixed (module Irmin.Hash.SHA256 : Irmin.Hash.S));
        ("sha384", Fixed (module Irmin.Hash.SHA384 : Irmin.Hash.S));
        ("sha512", Fixed (module Irmin.Hash.SHA512 : Irmin.Hash.S));
        ("tezos", Fixed (module Irmin_tezos.Schema.Hash : Irmin.Hash.S));
      ]

  let default = ref ("blake2b", (module Irmin.Hash.BLAKE2B : Irmin.Hash.S))

  let add name ?default:(x = false) m =
    all := (name, Fixed m) :: !all;
    if x then default := (name, m)

  let find_hashfn name =
    match List.assoc_opt (String.Ascii.lowercase name) !all with
    | Some c -> c
    | None ->
        let valid = String.concat ~sep:", " (List.split !all |> fst) in
        let msg =
          Printf.sprintf "Invalid hash function: %s. Expected one of: %s." name
            valid
        in
        failwith msg

  let of_specifier hashname =
    let ( >>= ) x f = match x with Ok x -> f x | Error _ as e -> e in
    (match String.cut ~rev:true ~sep:"/" hashname with
    | Some (hashname, size) -> (
        match int_of_string_opt size with
        | Some size -> Ok (hashname, Some size)
        | None -> Error (`Msg (Fmt.str "Non-numeric hash size %s passed" size)))
    | None -> Ok (hashname, None))
    >>= fun (hashname, size_opt) ->
    match (find_hashfn hashname, size_opt) with
    | Variable_size hashfn, size_opt -> Ok (hashfn size_opt)
    | Fixed hashfn, None -> Ok hashfn
    | Fixed _, Some size ->
        Error
          (`Msg
            (Fmt.str "Cannot specify a size for hash function `%s' (%d passed)."
               hashname size))

  let find h =
    of_specifier h |> function Ok h -> h | Error (`Msg e) -> failwith e

  let hash_function_conv : t Cmdliner.Arg.conv = Arg.conv (of_specifier, Fmt.nop)

  let term () =
    let kind =
      let quote s = Fmt.str "`%s'" s in
      let hash_types = !all |> List.map (fun (name, _) -> (name, name)) in
      let variable_size_types =
        !all
        |> List.filter (function
             | _, Variable_size _ -> true
             | _, Fixed _ -> false)
        |> List.map fst
      in
      let pp_prose_list =
        Fmt.of_to_string (function
          | [] -> ""
          | [ h ] -> quote h
          | hs ->
              let rev_hs = List.rev hs in
              Fmt.str "%s and %s"
                (String.concat ~sep:", " (List.rev_map quote (List.tl rev_hs)))
                (quote (List.hd rev_hs)))
      in
      let pp_plural =
        Fmt.of_to_string (function _ :: _ :: _ -> "s" | _ -> "")
      in
      let pp_variable_size_doc ppf = function
        | [] -> ()
        | _ :: _ as hs ->
            Fmt.pf ppf
              "\n\
               The bit-length of the hash function%a %a may optionally be set \
               with a trailing slash (e.g. `%s/16')."
              pp_plural hs pp_prose_list hs (List.hd hs)
      in
      let doc =
        Fmt.str "The hash function (%s). Default is `%s'.%a"
          (Arg.doc_alts_enum hash_types)
          (fst !default) pp_variable_size_doc variable_size_types
      in
      let arg_info =
        Arg.info ~doc ~docs:global_option_section [ "hash"; "h" ]
      in
      Arg.(value & opt (some hash_function_conv) None & arg_info)
    in
    let create kind = kind in
    Term.(const create $ kind)
end

type hash = Hash.t

(* Store *)

module Store = struct
  module Impl = struct
    type 'a t =
      | Hash_keyed : (module Irmin.S with type t = 'a) -> 'a t
      | Generic_keyed : (module Irmin.Generic_key.S with type t = 'a) -> 'a t

    let generic_keyed (type a) (t : a t) :
        (module Irmin.Generic_key.S with type t = a) =
      match t with Hash_keyed (module S) -> (module S) | Generic_keyed x -> x

    let hash_keyed = function Hash_keyed x -> Some x | Generic_keyed _ -> None
  end

  type remote_fn =
    ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> unit -> Irmin.remote

  type t =
    | T : {
        impl : _ Impl.t;
        spec : Irmin.Backend.Conf.Spec.t;
        remote : remote_fn option;
      }
        -> t

  let spec (T { spec; _ }) = spec

  type store_functor =
    | Fixed_hash of (contents -> t)
    | Variable_hash of (hash -> contents -> t)
    | Fixed of t

  module type G = sig
    include Irmin.S

    val remote : remote_fn
  end

  let v ?remote spec s = T { impl = Impl.Hash_keyed s; spec; remote }
  let v_generic ?remote spec s = T { impl = Impl.Generic_keyed s; spec; remote }
  let v_git (module S : G) = v Irmin_git.Conf.spec (module S) ~remote:S.remote

  let create :
      Irmin.Backend.Conf.Spec.t -> (module Irmin.Maker) -> hash -> contents -> t
      =
   fun spec (module S) (module H) (module C) ->
    let module Schema = struct
      include Irmin.Schema.KV (C)
      module Hash = H
    end in
    let module S = S.Make (Schema) in
    v spec (module S)

  let mem = create Irmin_mem.Conf.spec (module Irmin_mem)
  let fs = create Irmin_fs.Conf.spec (module Irmin_fs_unix)
  let git (module C : Irmin.Contents.S) = v_git (module Xgit.FS.KV (C))
  let git_mem (module C : Irmin.Contents.S) = v_git (module Xgit.Mem.KV (C))

  module Irmin_pack_maker : Irmin.Generic_key.Maker = struct
    include Irmin_pack_unix.Maker (Irmin_tezos.Conf)

    module Make (Schema : Irmin.Schema.S) = Make (struct
      include Schema
      module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
      module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
      module Commit = Commit_maker.Make (Hash)
    end)
  end

  let pack : hash -> contents -> t =
   fun (module H) (module C) ->
    let module Schema = struct
      include Irmin.Schema.KV (C)
      module Hash = H
    end in
    v_generic Irmin_pack.Conf.spec (module Irmin_pack_maker.Make (Schema))

  let tezos = v_generic Irmin_pack.Conf.spec (module Irmin_tezos.Store)

  let all =
    ref
      [
        ("git", Fixed_hash git);
        ("git-mem", Fixed_hash git_mem);
        ("fs", Variable_hash fs);
        ("mem", Variable_hash mem);
        ("pack", Variable_hash pack);
        ("tezos", Fixed tezos);
      ]

  let default = "git" |> fun n -> ref (n, List.assoc n !all)

  let add name ?default:(x = false) m =
    all := (name, m) :: !all;
    if x then default := (name, m)

  let find name =
    match List.assoc_opt (String.Ascii.lowercase name) !all with
    | Some s -> s
    | None ->
        let valid = String.concat ~sep:", " (List.split !all |> fst) in
        let msg =
          Printf.sprintf "Invalid store type: %s. Expected one of: %s." name
            valid
        in
        failwith msg

  let generic_keyed = function
    | T { impl = Generic_keyed (module S); _ } ->
        (module S : Irmin.Generic_key.S)
    | T { impl = Hash_keyed (module S); _ } -> (module S : Irmin.Generic_key.S)

  let hash_keyed = function
    | T { impl = Generic_keyed (module S); _ } -> None
    | T { impl = Hash_keyed (module S); _ } -> Some (module S : Irmin.S)

  let remote (T { remote; _ }) = remote

  let term () =
    let store =
      let store_types = !all |> List.map (fun (name, _) -> (name, name)) in
      let doc =
        Fmt.str "The storage backend (%s). Default is `%s'."
          (Arg.doc_alts_enum store_types)
          (fst !default)
      in
      let arg_info =
        Arg.info ~doc ~docs:global_option_section [ "s"; "store" ]
      in
      Arg.(value & opt (some string) None & arg_info)
    in
    let create store hash contents = (store, hash, contents) in
    Term.(const create $ store $ Hash.term () $ Contents.term ())
end

(* Config *)

let home =
  try Sys.getenv "HOME"
  with Not_found -> (
    try (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
    with Unix.Unix_error _ | Not_found ->
      if Sys.win32 then try Sys.getenv "AppData" with Not_found -> "" else "")

let config_root () =
  try Sys.getenv "XDG_CONFIG_HOME"
  with Not_found ->
    if Sys.win32 then home / "Local Settings" else home / ".config"

let config_term =
  let create root config_path (opts : (string * string) list list) =
    (root, config_path, opts)
  in
  let doc =
    "Backend-specific options. See the output of `irmin options` for a list of \
     options supported by the selected backend"
  in
  let opts =
    Arg.info ~docv:"OPTIONS" ~docs:global_option_section ~doc
      [ "opt"; "options" ]
  in
  Term.(
    const create
    $ root_term
    $ config_path_term
    $ Arg.(value @@ opt_all (list (pair ~sep:'=' string string)) [] opts))

type store =
  | S : 'a Store.Impl.t * (unit -> 'a) * Store.remote_fn option -> store

let rec read_config_file path =
  let home = config_root () / global_config_path in
  let path =
    match path with
    | Some path ->
        if (not (Sys.file_exists path)) && not (String.equal path home) then
          Fmt.failwith "config file does not exist: %s" path
        else path
    | None -> "irmin.yml"
  in
  let global =
    if String.equal path home then `O [] else read_config_file (Some home)
  in
  if not (Sys.file_exists path) then global
  else
    let () = [%logs.debug "Loading config from file: %s" path] in
    let oc = open_in path in
    let len = in_channel_length oc in
    let buf = really_input_string oc len in
    close_in oc;
    if Astring.String.(is_empty (trim buf)) then `O []
    else
      match Yaml.of_string buf with
      | Ok (`O _ as y) -> Yaml.Util.combine_exn y global
      | Ok `Null -> global
      | Ok _ -> Fmt.failwith "invalid YAML file: %s" path
      | Error (`Msg msg) -> Fmt.failwith "unable to parse YAML: %s" msg

let rec json_of_yaml : Yaml.value -> Yojson.Basic.t = function
  | `O x -> `Assoc (List.map (fun (k, v) -> (k, json_of_yaml v)) x)
  | `A x -> `List (List.map json_of_yaml x)
  | (`Null | `Bool _ | `Float _ | `String _) as x -> x

let parse_config ?root y spec =
  let config = Conf.empty spec in
  (* Initialise root for the examples in README to pass. *)
  let config = Conf.add config (Conf.root spec) "." in
  let config =
    List.fold_left
      (fun config k ->
        match (Conf.Spec.find_key spec k, Yaml.Util.find_exn k y) with
        | Some (Irmin.Backend.Conf.K k), Some v ->
            let v = json_of_yaml v |> Yojson.Basic.to_string in
            let v =
              match Irmin.Type.of_json_string (Conf.ty k) v with
              | Error _ ->
                  let v = Format.sprintf "{\"some\": %s}" v in
                  Irmin.Type.of_json_string (Conf.ty k) v |> Result.get_ok
              | Ok v -> v
            in
            Conf.add config k v
        | None, _ -> (
            match k with
            | "contents" | "hash" | "store" | "plugin" -> config
            | _ ->
                Fmt.invalid_arg "unknown config key for %s: %s"
                  (Conf.Spec.name spec) k)
        | _ -> config)
      config (Yaml.Util.keys_exn y)
  in
  let config =
    match (root, Conf.Spec.find_key spec "root") with
    | Some root, Some (K r) ->
        let v = Irmin.Type.of_string (Conf.ty r) root |> Result.get_ok in
        Conf.add config r v
    | _ -> config
  in
  config

let load_plugin ?plugin config =
  match plugin with
  | Some p -> Dynlink.loadfile_private p
  | None -> (
      match Yaml.Util.find "plugin" config with
      | Ok (Some v) -> Dynlink.loadfile_private (Yaml.Util.to_string_exn v)
      | _ -> ())

let get_store ?plugin config (store, hash, contents) =
  let () = load_plugin ?plugin config in
  let store =
    match store with
    | Some s -> Store.find s
    | None -> (
        match Yaml.Util.find_exn "store" config with
        | Some (`String s) -> (
            match store with Some s -> Store.find s | None -> Store.find s)
        | _ -> snd !Store.default)
  in
  let contents =
    match contents with
    | Some s -> Contents.find s
    | None -> (
        match Yaml.Util.find_exn "contents" config with
        | Some (`String s) -> Contents.find s
        | _ -> snd !Contents.default)
  in
  let hash =
    match hash with
    | Some s -> Some s
    | None -> (
        match Yaml.Util.find_exn "hash" config with
        | Some (`String s) -> Some (Hash.find s)
        | _ -> None)
  in
  match store with
  | Variable_hash s ->
      let hash : Hash.t = Option.value ~default:(snd !Hash.default) hash in
      s hash contents
  | Fixed_hash s -> (
      (* error if a hash function has been passed *)
      match hash with
      | None -> s contents
      | _ ->
          Fmt.failwith "Cannot customize the hash function for the given store")
  | Fixed s -> (
      match hash with
      | None -> s
      | _ ->
          Fmt.failwith "Cannot customize the hash function for the given store")

let load_config ?plugin ?root ?config_path ?store ?hash ?contents () =
  let y = read_config_file config_path in
  let store = get_store ?plugin y (store, hash, contents) in
  let spec = Store.spec store in
  let config = parse_config ?root y spec in
  (store, config)

let string_value = function `String s -> s | _ -> raise Not_found

let find_key config name =
  Yaml.Util.find_exn name config |> Option.map (fun x -> string_value x)

let handle_decode_err err t x =
  match Irmin.Type.of_string t x with Ok h -> h | _ -> invalid_arg err

let get_branch (type a)
    (module S : Irmin.Generic_key.S with type Schema.Branch.t = a) config branch
    =
  let of_string = Option.map (handle_decode_err "invalid branch" S.Branch.t) in
  match branch with
  | None -> of_string (find_key config "branch")
  | Some t -> of_string (Some t)

let get_commit (type a b)
    (module S : Irmin.Generic_key.S
      with type commit = a
       and type Schema.Hash.t = b) config commit =
  let of_string = Option.map (handle_decode_err "invalid commit" S.Hash.t) in
  match commit with
  | None -> of_string (find_key config "commit")
  | Some t -> of_string (Some t)

let build_irmin_config ~sw config root opts (store, hash, contents) branch
    commit plugin : store =
  let (T { impl; spec; remote }) =
    get_store ?plugin config (store, hash, contents)
  in
  let (module S) = Store.Impl.generic_keyed impl in
  let branch = get_branch (module S) config branch in
  let commit = get_commit (module S) config commit in
  let config = parse_config ?root config spec in
  let config =
    List.fold_left
      (fun config (k, v) ->
        let (Irmin.Backend.Conf.K key) =
          if k = "root" then
            invalid_arg
              "use the --root flag to set the root directory instead of \
               passing it as a config"
          else
            match Conf.Spec.find_key spec k with
            | Some x -> x
            | None -> invalid_arg ("opt: " ^ k)
        in
        let ty = Conf.ty key in
        let v = try_parse ty v |> Result.get_ok in
        let config = Conf.add config key v in
        config)
      config (List.flatten opts)
  in
  let spec () =
    match (branch, commit) with
    | _, Some hash -> (
        let repo = S.Repo.v ~sw config in
        let commit = S.Commit.of_hash repo hash in
        match commit with
        | None -> invalid_arg "unknown commit"
        | Some c -> S.of_commit c)
    | None, None -> S.Repo.v ~sw config |> S.main
    | Some b, None ->
        let repo = S.Repo.v ~sw config in
        S.of_branch repo b
  in
  S (impl, spec, remote)

let branch =
  let doc =
    Arg.info ~doc:"The current branch name. Default is the store's main branch."
      ~docs:global_option_section ~docv:"BRANCH" [ "b"; "branch" ]
  in
  Arg.(value & opt (some string) None & doc)

let commit =
  let doc =
    Arg.info
      ~doc:"The store's head commit. This will take precedence over --branch."
      ~docs:global_option_section ~docv:"COMMIT" [ "commit" ]
  in
  Arg.(value & opt (some string) None & doc)

let plugin =
  let doc = "Register new contents, store or hash types" in
  Arg.(value & opt (some string) None & info ~doc [ "plugin" ])

let store () =
  let create plugin store (root, config_path, opts) branch commit =
    let y = read_config_file config_path in
    fun ~sw -> build_irmin_config ~sw y root opts store branch commit plugin
  in
  Term.(const create $ plugin $ Store.term () $ config_term $ branch $ commit)

let header_conv =
  let parse str =
    match String.cut ~sep:":" str with
    | Some (k, v) -> Ok (String.trim k, String.trim v)
    | None -> Error (`Msg "invalid header")
  in
  let print ppf (k, v) = Fmt.pf ppf "%s: %s" k v in
  Cmdliner.Arg.conv (parse, print)

let headers =
  let doc =
    Arg.info ~docv:"HEADER" ~doc:"Extra HTTP headers to use when sync." [ "H" ]
  in
  Arg.(value & opt_all header_conv [] & doc)

type Irmin.remote += R of Cohttp.Header.t option * string

(* FIXME: this is a very crude heuristic to choose the remote
   kind. Would be better to read the config file and look for remote
   alias. *)
let infer_remote ~sw hash contents branch headers str =
  let hash = match hash with None -> snd !Hash.default | Some c -> c in
  let contents =
    match contents with
    | None -> snd !Contents.default
    | Some c -> Contents.find c
  in
  if Sys.file_exists str then
    let r =
      if Sys.file_exists (str / ".git") then Store.git contents
      else if Sys.file_exists (str / "store.dict") then Store.pack hash contents
      else Store.fs hash contents
    in
    match r with
    | Store.T { impl; spec; _ } ->
        let (module R) = Store.Impl.generic_keyed impl in
        let config = Conf.empty spec in
        let config =
          match Conf.Spec.find_key spec "root" with
          | Some (K r) ->
              let v = Irmin.Type.of_string (Conf.ty r) str |> Result.get_ok in
              Conf.add config r v
          | _ -> config
        in
        let repo = R.Repo.v ~sw config in
        let branch =
          match branch with
          | Some b -> Irmin.Type.of_string R.branch_t b |> Result.get_ok
          | None -> R.Branch.main
        in
        let r = R.of_branch repo branch in
        Irmin.remote_store (module R) r
  else
    let headers =
      match headers with [] -> None | h -> Some (Cohttp.Header.of_list h)
    in
    R (headers, str)

let remote () =
  let repo =
    let doc =
      Arg.info ~docv:"REMOTE"
        ~doc:"The URI of the remote repository to clone from." []
    in
    Arg.(required & pos 0 (some string) None & doc)
  in
  let create (store, hash, contents) (root, config_path, opts) branch commit
      headers str =
    let y = read_config_file config_path in
    let store ~sw =
      build_irmin_config ~sw y root opts (store, hash, contents) branch commit
        None
    in
    let remote ~sw () = infer_remote ~sw hash contents branch headers str in
    fun ~sw -> (store ~sw, remote ~sw)
  in
  Term.(
    const create
    $ Store.term ()
    $ config_term
    $ branch
    $ commit
    $ headers
    $ repo)
