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
open Astring

type contents = (module Irmin.Contents.S)

let create: (module Irmin.S_MAKER) -> contents -> (module Irmin.S) =
  fun (module B) (module C) ->
    let module S = B(C)(Irmin.Ref.String)(Irmin.Hash.SHA1) in (module S)

let mem_store = create (module Irmin_mem.Make)
let irf_store = create (module Irmin_fs.Make)
let http_store = create (module Irmin_http.Make(Irmin.Metadata.None))
let git_store = create (module Irmin_git.FS)

let mk_store = function
  | `Mem  -> mem_store
  | `Irf  -> irf_store
  | `Http -> http_store
  | `Git  -> git_store

let store_kinds = [
  ("git" , `Git);
  ("irf" , `Irf);
  ("http", `Http);
  ("mem" , `Mem);
]

let default_store = `Git

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

let key k default =
  let doc = Irmin.Private.Conf.doc k in
  let docs = Irmin.Private.Conf.docs k in
  let docv = Irmin.Private.Conf.docv k in
  let conv = Irmin.Private.Conf.conv k in
  let name = Irmin.Private.Conf.name k in
  let i = Arg.info ?docv ?doc ?docs [name] in
  Arg.(value & opt conv default i)

let opt_key k = key k (Irmin.Private.Conf.default k)

let config_term =
  let add k v config = Irmin.Private.Conf.add config k v in
  let create root bare head level uri content_type =
    Irmin.Private.Conf.empty
    |> add Irmin.Private.Conf.root root
    |> add Irmin_git.bare bare
    |> add Irmin_git.head head
    |> add Irmin_git.level level
    |> add Irmin_http.uri uri
    |> add Irmin_http.content_type content_type
  in
  Term.(pure create $
        opt_key Irmin.Private.Conf.root $
        flag_key Irmin_git.bare $
        opt_key Irmin_git.head $
        opt_key Irmin_git.level $
        opt_key Irmin_http.uri $
        opt_key Irmin_http.content_type)

let mk_contents k: contents = match k with
  | `String  -> (module Irmin.Contents.String)
  | `Json    -> (module Irmin.Contents.Json)
  | `Cstruct -> (module Irmin.Contents.Cstruct)

let contents_kinds = [
  "string" , `String;
  "json"   , `Json;
  "cstruct", `Cstruct;
]

let default_contents = `String

let contents =
  let kind =
    let doc = Arg.info ~doc:"The type of user-defined contents." ["contents";"c"] in
    Arg.(value & opt (enum contents_kinds) default_contents & doc)
  in
  Term.(pure mk_contents $ kind)

let store_term =
  let store =
    let doc = Arg.info ~doc:"The kind of backend stores." ["s";"store"] in
    Arg.(value & opt (some (enum store_kinds)) None & doc)
  in
  let create store contents = match store with
    | Some s -> Some (mk_store s contents)
    | None   -> None
  in
  Term.(pure create $ store $ contents)

let cfg = ".irminconfig"

type t = S: (module Irmin.S with type t = 'a) * (string -> 'a) Lwt.t -> t

(* FIXME: use a proper configuration format (toml?) and interface
   properly with cmdliner *)
let read_config_file (): t option =
  if not (Sys.file_exists cfg) then None
  else
    let oc = open_in cfg in
    let len = in_channel_length oc in
    let buf = Bytes.create len in
    really_input oc buf 0 len;
    let lines = String.cuts ~sep:"\n" buf in
    let lines = List.map (fun s -> String.trim s) lines in
    let lines = List.map (fun s -> String.cut ~sep:"=" s) lines in
    let lines =
      List.fold_left (fun l -> function None -> l | Some x -> x::l) [] lines
    in
    let assoc name fn = try Some (fn (List.assoc name lines)) with Not_found -> None in
    let contents =
      let kind =
        match assoc "contents" (fun x -> List.assoc x contents_kinds) with
        | None   -> default_contents
        | Some c -> c
      in
      mk_contents kind
    in
    let store =
      let kind =
        match assoc "store" (fun x -> List.assoc x store_kinds) with
        | None   -> default_store
        | Some s -> s
      in
      mk_store kind contents
    in
    let module S = (val store) in
    let branch = assoc "branch" (fun x -> S.Ref.of_hum x) in
    let config =
      let root = assoc "root" (fun x -> x) in
      let bare = match assoc "bare" bool_of_string with
        | None   -> Irmin.Private.Conf.default Irmin_git.bare
        | Some b -> b
      in
      let head = assoc "head" (fun x -> Git.Reference.of_raw x) in
      let uri = assoc "uri" Uri.of_string in
      let add k v config = Irmin.Private.Conf.add config k v in
      Irmin.Private.Conf.empty
      |> add Irmin.Private.Conf.root root
      |> add Irmin_git.bare bare
      |> add Irmin_git.head head
      |> add Irmin_http.uri uri
    in
    match branch with
    | None   -> Some (S ((module S), S.Repo.create config >>= S.master task))
    | Some b -> Some (S ((module S), S.Repo.create config >>= S.of_branch_id task b))

let store =
  let branch =
    let doc =
      Arg.info
        ~doc:"The current branch name. Default is the store's master branch."
        ["b"; "branch"]
    in
    Arg.(value & opt (some string) None & doc)
  in
  let create store config branch =
    match store with
    | Some s ->
      let module S = (val s: Irmin.S) in
      (* first look at the command-line options *)
      let t = match branch with
        | None   -> S.Repo.create config >>= S.master task
        | Some t -> S.Repo.create config >>= S.of_branch_id task (S.Ref.of_hum t)
      in
      S ((module S), t)
    | None ->
      (* then look at the config file options *)
      match read_config_file () with
      | Some c -> c
      | None   ->
        let s = mk_store `Git (mk_contents `String) in
        let module S = (val s: Irmin.S) in
        let t = S.Repo.create config >>= S.master task in
        S ((module S), t)
  in
  Term.(pure create $ store_term $ config_term $ branch)

(* FIXME: read the remote configuration in a file *)
let (/) = Filename.concat

(* FIXME: this is a very crude heuristic to choose the remote
   kind. Would be better to read the config file and look for remote
   alias. *)
let infer_remote contents str =
  if Sys.file_exists str then (
    let r =
      if Sys.file_exists (str / ".git")
      then git_store contents
      else irf_store contents
    in
    let module R = (val r) in
    let config =
      let add k v c = Irmin.Private.Conf.add c k v in
      Irmin.Private.Conf.empty
      |> add Irmin_http.uri (Some (Uri.of_string str))
      |> add Irmin.Private.Conf.root (Some str)
    in
    R.Repo.create config >>= R.master task >>= fun r ->
      return (Irmin.remote_store (module R) (r "Clone %s."))
    ) else
      return (Irmin.remote_uri str)

let remote =
  let repo =
    let doc = Arg.info ~docv:"REMOTE"
        ~doc:"The URI of the remote repository to clone from." [] in
    Arg.(required & pos 0 (some string) None & doc) in
  Term.(pure infer_remote $ contents $ repo)
