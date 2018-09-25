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
open Astring

let global_option_section = "COMMON OPTIONS"

let flag_key k =
  let doc = Irmin.Private.Conf.doc k in
  let docs = Irmin.Private.Conf.docs k in
  let docv = Irmin.Private.Conf.docv k in
  let default = Irmin.Private.Conf.default k in
  let name =
    let x = Irmin.Private.Conf.name k in
    if default then "no-" ^ x else x
  in
  let i = Arg.info ?docv ?doc ?docs [name] in
  if default then Arg.(value & vflag true [false, i])
  else Arg.(value & flag i)

let pconv (parse, pp) =
  let parse str = match parse str with
    | Ok x           -> `Ok x
    | Error (`Msg e) -> `Error e
  in
  parse, pp

let key k default =
  let doc = Irmin.Private.Conf.doc k in
  let docs = Irmin.Private.Conf.docs k in
  let docv = Irmin.Private.Conf.docv k in
  let mk = pconv (Irmin.Private.Conf.conv k) in
  let name = Irmin.Private.Conf.name k in
  let i = Arg.info ?docv ?doc ?docs [name] in
  Arg.(value & opt mk default i)

let opt_key k = key k (Irmin.Private.Conf.default k)

let config_path_key =
  Irmin.Private.Conf.key
    ~docs:global_option_section
    ~docv:"PATH"
    ~doc:"Allows configuration file to be specified on the command-line"
    "config" Irmin.Private.Conf.string "irmin.yml"

let (/) = Filename.concat

let global_config_path = ".irmin" / "config.yml"

let add_opt k v config = match v with
  | None -> config
  | Some _ -> Irmin.Private.Conf.add config k v


(* Contents *)

module Contents = struct

  type t = (module Irmin.Contents.S)

  let all = ref [
      "string", (module Irmin.Contents.String: Irmin.Contents.S);
      "bytes" , (module Irmin.Contents.Bytes);
      "json"  , (module Irmin.Contents.Json);
    ]
  let default = ref (module Irmin.Contents.String: Irmin.Contents.S)

  let add name ?default:(x=false) m =
    all := (name, m) :: !all;
    if x then default := m

  let find name =
    match List.assoc_opt (String.Ascii.lowercase name) !all with
    | Some c -> c
    | None ->
      let valid = String.concat ~sep:", " (List.split !all |> fst) in
      let msg =
        Printf.sprintf "Invalid content type: %s. Expected one of: %s."
          name valid
      in
      failwith msg

  let term =
    let kind =
      let doc =
        Arg.info ~doc:"The type of user-defined contents."
          ~docs:global_option_section ["contents";"c"]
      in
      Arg.(value & opt (some string) None & doc)
    in
    let create kind = kind in
    Term.(const create $  kind)

end

type contents = Contents.t

(* Store *)

module Store = struct

  type t =
    | T: (module Irmin.S) * (Git_unix.endpoint -> Irmin.remote) option -> t

  let v ?endpoint s = T (s, endpoint)

  let v_git (module S: Irmin.S with type endpoint = Git_unix.endpoint) =
    v (module S) ~endpoint:(fun e -> S.Private.Sync.remote e)

  let create: (module Irmin.S_MAKER) -> contents -> t =
    fun (module S) (module C) ->
      let module S =
        S(Irmin.Metadata.None)(C)
          (Irmin.Path.String_list)
          (Irmin.Branch.String)
          (Irmin.Hash.SHA1)
      in
      T ((module S), None)

  let mem = create (module Irmin_mem.Make)
  let irf = create (module Fs.Make)
  let http = create (module Http.Make)

  let git (module C: Irmin.Contents.S) =
    v_git (module Xgit.FS.KV(C))

  let git_mem (module C: Irmin.Contents.S) =
    v_git (module Xgit.Mem.KV(C))

  let all = ref [
      ("git"    , git);
      ("git-mem", git_mem);
      ("irf"    , irf);
      ("http"   , http);
      ("mem"    , mem);
    ]

  let default = ref git

  let add name ?default:(x=false) m =
    all := (name, m) :: !all;
    if x then default := m

  let find name =
    match List.assoc_opt (String.Ascii.lowercase name) !all with
    | Some s -> s
    | None ->
      let valid = String.concat ~sep:", " (List.split !all|> fst) in
      let msg =
        Printf.sprintf "Invalid store type: %s. Expected one of: %s." name valid
      in
      failwith msg

  let term =
    let store =
      let doc =
        Arg.info ~doc:"The storage backend." ~docs:global_option_section
          ["s";"store"] in
      Arg.(value & opt (some string) None & doc)
    in
    let create store contents = (store, contents) in
  Term.(const create $ store $ Contents.term)

end

(* Config *)

let rec read_config_file path =
  let home = Unix.getenv "HOME" / global_config_path in
  let global =
    if String.equal path home
      then []
      else read_config_file home
  in
  if not (Sys.file_exists path) then global
  else
    let oc = open_in path in
    let len = in_channel_length oc in
    let buf = really_input_string oc len in
    close_in oc;
    match Yaml.of_string buf with
      | Ok (`O y) -> y @ global
      | _ -> global

let config_term =
  let add k v config = Irmin.Private.Conf.add config k v in
  let create root bare head level uri config_path =
    Irmin.Private.Conf.empty
    |> add_opt Irmin.Private.Conf.root root
    |> add Irmin_git.bare bare
    |> add_opt Irmin_git.head head
    |> add_opt Irmin_git.level level
    |> add_opt Irmin_http.uri uri
    |> add config_path_key config_path
  in
  Term.(const create $
        opt_key Irmin.Private.Conf.root $
        flag_key Irmin_git.bare $
        opt_key Irmin_git.head $
        opt_key Irmin_git.level $
        opt_key Irmin_http.uri $
        opt_key config_path_key)

type store =
  | S: (module Irmin.S with type t = 'a)
       * 'a Lwt.t
       * (Git_unix.endpoint -> Irmin.remote) option
    -> store

let from_config_file_with_defaults path (store, contents) config branch: store =
  let y = read_config_file path in
  let string_value = function
    | `String s -> s
    | _ -> raise Not_found
  in
  let assoc name fn =
    try Some (fn (List.assoc name y |> string_value))
    with Not_found -> None
  in
  let store =
    let contents =
      match contents with
      | None ->
        (match assoc "contents" Contents.find with
        | None   -> !Contents.default
        | Some c -> c)
      | Some c -> Contents.find c
    in
    let store =
      match store with
      | None ->
        (match assoc "store" Store.find with
        | None   -> !Store.default
        | Some s -> s)
      | Some s -> Store.find s
    in
    store contents
  in
  let config =
    let root = assoc "root" (fun x -> x) in
    let bare = match assoc "bare" bool_of_string with
      | None   -> Irmin.Private.Conf.default Irmin_git.bare
      | Some b -> b
    in
    let head = assoc "head" (fun x -> Git.Reference.of_string x) in
    let uri = assoc "uri" Uri.of_string in
    let add k v config = Irmin.Private.Conf.add config k v in
    Irmin.Private.Conf.empty
    |> add_opt Irmin.Private.Conf.root root
    |> add Irmin_git.bare bare
    |> add_opt Irmin_git.head head
    |> add_opt Irmin_http.uri uri
    |> Irmin.Private.Conf.union config
  in
  match store with
  | Store.T ((module S), remote) ->
    let mk_master () = S.Repo.v config >>= fun repo -> S.master repo in
    let mk_branch b = S.Repo.v config >>= fun repo -> S.of_branch repo b in
    let branch =
      match branch with
      | None   -> assoc "branch" (fun x -> match S.Branch.of_string x with
          | Ok x -> x
          | Error (`Msg msg) -> failwith msg)
      | Some t -> (match S.Branch.of_string t with
          | Ok x           -> Some x
          | Error (`Msg e) -> failwith e)
    in
    match branch with
    | None   -> S ((module S), mk_master (), remote)
    | Some b -> S ((module S), mk_branch b , remote)

let branch =
  let doc =
    Arg.info
      ~doc:"The current branch name. Default is the store's master branch."
      ~docs:global_option_section
      ~docv:"BRANCH"
      ["b"; "branch"]
  in
  Arg.(value & opt (some string) None & doc)

let store =
  let create store config branch =
    let cfg = Irmin.Private.Conf.get config config_path_key in
    from_config_file_with_defaults cfg store config branch
  in
  Term.(const create $ Store.term $ config_term $ branch)


let header_conv =
  let parse str = match String.cut ~sep:":" str with
    | Some (k, v) -> Ok (String.trim k, String.trim v)
    | None        -> Error (`Msg "invalid header")
  in
  let print ppf (k, v) = Fmt.pf ppf "%s: %s" k v in
  Cmdliner.Arg.conv (parse, print)

let headers =
  let doc =
    Arg.info ~docv:"HEADER" ~doc:"Extra HTTP headers to use when sync." ["H"]
  in
  Arg.(value & opt_all header_conv [] & doc)

type Irmin.remote += E of Git_unix.endpoint

(* FIXME: this is a very crude heuristic to choose the remote
   kind. Would be better to read the config file and look for remote
   alias. *)
let infer_remote contents headers str =
  let contents = match contents with
    | None   -> !Contents.default
    | Some c -> Contents.find c
  in
  if Sys.file_exists str then (
    let r =
      if Sys.file_exists (str / ".git")
      then Store.git contents
      else Store.irf contents
    in
    match r with
    | Store.T ((module R), _) ->
      let config =
        Irmin.Private.Conf.empty
        |> add_opt Irmin_http.uri (Some (Uri.of_string str))
        |> add_opt Irmin.Private.Conf.root (Some str)
      in
      R.Repo.v config >>= fun repo ->
      R.master repo >|= fun r ->
      Irmin.remote_store (module R) r
  ) else
    let headers = Cohttp.Header.of_list headers in
    let endpoint = Git_unix.endpoint ~headers (Uri.of_string str) in
    Lwt.return (E endpoint)

let remote =
  let repo =
    let doc = Arg.info ~docv:"REMOTE"
        ~doc:"The URI of the remote repository to clone from." [] in
    Arg.(required & pos 0 (some string) None & doc) in
  Term.(const infer_remote $ Contents.term $ headers $ repo)
