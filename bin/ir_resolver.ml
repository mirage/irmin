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

open Cmdliner
open Irmin_unix

module P = Irmin.Path.String
module K = Irmin.Hash.SHA1
module T = Irmin.Tag.String_list

type contents = (module Irmin.Contents.S)

module Make (F: Irmin.S_MAKER)(C: Irmin.Contents.S) =  F(P)(C)(T)(K)

let create: (module Irmin.S_MAKER) -> contents -> (module Irmin.S) =
  fun (module M) (module C) ->
    let module X = Make(M)(C) in
    (module X)

let mem_store = create (module Irmin_mem.Make)
let irf_store = create (module Irmin_fs.Make)
let http_store = create (module Irmin_http.Make)
let git_store = create (module Irmin_git.FS)

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

let config =
  let add k v config = Irmin.Private.Conf.add config k v in
  let create root bare branch uri =
    Irmin.Private.Conf.empty
    |> add Irmin.Private.Conf.root root
    |> add Irmin_git.bare bare
    |> add Irmin_git.branch branch
    |> add Irmin_http.uri uri
  in
  Term.(pure create $
        opt_key Irmin.Private.Conf.root $
        flag_key Irmin_git.bare $
        opt_key Irmin_git.branch $
        opt_key Irmin_http.uri)

let kinds = [
  ("git" , git_store);
  ("irf" , irf_store);
  ("http", http_store);
  ("mem" , mem_store);
]

let mk_contents k: contents = match k with
  | `String  -> (module Irmin.Contents.String)
  | `Json    -> (module Irmin.Contents.Json)
  | `Cstruct -> (module Irmin.Contents.Cstruct)

let string = mk_contents `String

let contents_kinds = [
  "string" , string;
  "json"   , mk_contents `Json;
  "cstruct", mk_contents `Cstruct
]

let contents =
  let kind =
    let doc = Arg.info ~doc:"The type of user-defined contents." ["contents";"c"] in
    Arg.(value & opt (enum contents_kinds) (mk_contents `String) & doc)
  in
  Term.(pure (fun x -> x) $ kind)

let store =
  let store =
    let doc = Arg.info ~doc:"The kind of backend stores." ["s";"store"] in
    Arg.(value & opt (some (enum kinds)) None & doc)
  in
  let create store contents =
    match store with
    | Some s -> Some (s contents)
    | None   -> None
  in
  Term.(pure create $ store $ contents)

let cfg = ".irminconfig"

(* FIXME: use a proper configuration format and interface properly
   with cmdliner *)
let read_config_file () =
  if not (Sys.file_exists cfg) then None, Irmin.Private.Conf.empty else
    let oc = open_in cfg in
    let len = in_channel_length oc in
    let buf = Bytes.create len in
    really_input oc buf 0 len;
    let lines = Stringext.split ~on:'\n' buf in
    let lines = List.map String.trim lines in
    let lines = List.map (Stringext.cut ~on:"=") lines in
    let lines =
      List.fold_left (fun l -> function None -> l | Some x -> x::l) [] lines
    in
    let assoc name fn = try Some (fn (List.assoc name lines)) with Not_found -> None in
    let contents =
      match assoc "contents" (fun x -> List.assoc x contents_kinds) with
      | None   -> string
      | Some c -> c
    in
    let store = assoc "store" (fun x -> (List.assoc x kinds) contents) in
    let config =
      let root = assoc "root" (fun x -> x) in
      let bare = match assoc "bare" bool_of_string with
        | None   -> Irmin.Private.Conf.default Irmin_git.bare
        | Some b -> b
      in
      let branch = match assoc "branch" (fun x -> x) with
        | None   -> Irmin.Private.Conf.default Irmin_git.branch
        | Some b -> b
      in
      let uri = assoc "uri" Uri.of_string in
      let add k v config = Irmin.Private.Conf.add config k v in
      Irmin.Private.Conf.empty
      |> add Irmin.Private.Conf.root root
      |> add Irmin_git.bare bare
      |> add Irmin_git.branch branch
      |> add Irmin_http.uri uri
    in
    store, config

let parse =
 let create store config =
    let store, config = match store with
      | None   -> read_config_file ()
      | Some s -> Some s, config
    in
    match store with
    | None   -> `Error (false, "Missing store configuration.")
    | Some s -> `Ok (s, config)
  in
  Term.(ret (pure create $ store $ config))

let remote =
  let repo =
    let doc = Arg.info ~docv:"REMOTE"
        ~doc:"The URI of the remote repository to clone from." [] in
    Arg.(required & pos 0 (some string) None & doc) in
  Term.(pure (fun x -> x) $ repo)
