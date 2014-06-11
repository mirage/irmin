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

open Core_kernel.Std
open Cmdliner
open Irmin_unix

let pr_str = Format.pp_print_string

let uri_conv =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf v = pr_str ppf (Uri.to_string v) in
  parse, print

let default_dir = ".irmin"

(* XXX: ugly hack *)
let init_hook_ref =
  ref (fun () -> ())

let init_hook = !init_hook_ref

let modules x: (module IrminKey.S) * (module IrminContents.S) * (module IrminTag.S) =
  match x with
  | `String -> (module IrminKey.SHA1), (module IrminContents.String), (module IrminTag.String)
  | `JSON   -> (module IrminKey.SHA1), (module IrminContents.JSON)  , (module IrminTag.String)

let in_memory_store (type key) k =
  Log.info (lazy "source: in-memory");
  let (module K), (module C), (module T) = modules k in
  let module M = IrminMemory.Make(K)(C)(T) in
  Irmin.cast (module M)

let local_store k dir =
  Log.infof "source: dir=%s" dir;
  init_hook_ref := (fun () -> if not (Sys.file_exists dir) then Unix.mkdir dir 0o755);
  let (module K), (module C), (module T) = modules k in
  let module M = IrminFS.Make(struct let path = dir end) in
  Irmin.cast (module M.Make(K)(C)(T))

let remote_store k uri =
  let (module K), (module C), (module R) = modules k in
  let module M = IrminCRUD.Make(Cohttp_lwt_unix.Client)(struct let uri = uri end) in
  Log.infof "source: uri=%s" (Uri.to_string uri);
  Irmin.cast (module M.Make(K)(C)(R))

let git_store k g =
  Log.infof "git";
  let (module K), (module C), (module R) = modules k in
  let (module B: Irmin.BACKEND) = match g with
    | `Memory -> (module IrminGit.Memory)
    | `Disk   -> (module IrminGit.FS (struct
        let root = None
        let bare = false
      end)) in
  Irmin.cast (module B.Make(K)(C)(R))

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
    None

let store_of_string_exn str =
  match store_of_string str with
  | None   -> raise Not_found
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

let remote =
  let branch =
    let doc =
      Arg.info ~docv:"BRANCH" ~doc:"Branch name." ["b";"branch"] in
    Arg.(value & opt (some string) None & doc) in
  let repository =
    let doc = Arg.info ~docv:"REPOSITORY"
        ~doc:"The (possibly remote) repository to clone from." [] in
    Arg.(required & pos 0 (some string) None & doc) in
  let create branch repository =
    match store_of_string repository with
    | None            -> IrminSync.uri repository
    | Some (module S) ->
      let module R: IrminBranch.STORE = S in
      IrminSync.store (module R) R.Branch.master in
  Term.(pure create $ branch $ repository)
