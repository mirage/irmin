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

(* Simple UI example: connect to http://localhost:8080/dump *)

open Lwt.Syntax

let fin () =
  let _ = Fmt.kstr Sys.command "cd %s && git reset HEAD --hard" Config.root in
  Lwt.return_unit

type action = {
  message : string;
  files : (string list * (unit -> string)) list;
}

type image = { name : string; actions : action list }

let ubuntu =
  {
    name = "official-images/ubuntu:14.04";
    actions =
      [
        {
          message = "Updating source lists";
          files =
            [
              ( [ "etc"; "source.list" ],
                fun () -> Fmt.str "deb %d" (Random.int 10) );
            ];
        };
        { message = "grep -v '^#' /etc/apt/sources.list"; files = [] };
        { message = "cat /etc/issue"; files = [] };
      ];
  }

let wordpress =
  {
    name = "official-images/wordpress:latest";
    actions =
      [
        {
          message = "user logging";
          files =
            [
              ( [ "wordpress"; "wp-users.php" ],
                fun () -> Fmt.str "<?php ...%d" (Random.int 10) );
            ];
        };
        {
          message = "configuration updates";
          files =
            [
              ( [ "wordpress"; "wp-settings.php" ],
                fun () -> Fmt.str "<?php .. %d" (Random.int 10) );
            ];
        };
      ];
  }

let mysql =
  {
    name = "local/mysql:5.5.41";
    actions =
      [
        { message = "Reading table wp_users"; files = [] };
        {
          message = "Writing table wp_users";
          files =
            [
              ( [ "var"; "lib"; "mysql" ],
                fun () -> Fmt.str "X%duYYt" (Random.int 10) );
            ];
        };
        { message = "Reading table wp_posts"; files = [] };
        {
          message = "Writing table wp_posts";
          files =
            [
              ( [ "var"; "lib"; "mysql" ],
                fun () -> Fmt.str "X%dxYYt" (Random.int 10) );
            ];
        };
      ];
  }

let branch image = String.map (function ':' -> '/' | c -> c) image.name
let images = [| (*ubuntu; *) wordpress; mysql |]

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let head = Git.Reference.v ("refs/heads/" ^ branch images.(0))
let config = Irmin_git.config ~bare:true ~head Config.root

let info image message () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = image.name in
  Store.Info.v ~author ~message date

let master = branch images.(0)

let init () =
  Config.init ();
  let* repo = Store.Repo.v config in
  let* t = Store.of_branch repo master in
  let* () = Store.set_exn t ~info:(info images.(0) "init") [ "0" ] "0" in
  Lwt_list.iter_s
    (fun i ->
      let* _ = Store.clone ~src:t ~dst:(branch i) in
      Lwt.return_unit)
    (Array.to_list images)

let random_array a = a.(Random.int (Array.length a))
let random_list l = random_array (Array.of_list l)

let rec process image =
  let id = branch image in
  Printf.printf "Processing %s\n%!" id;
  let actions = random_list image.actions in
  let key, value =
    try random_list actions.files
    with _ ->
      ([ "log"; id; "0" ], fun () -> id ^ string_of_int (Random.int 10))
  in
  let* repo = Store.Repo.v config in
  let* t = Store.of_branch repo id in
  let* () = Store.set_exn t ~info:(info image actions.message) key (value ()) in
  let* () =
    if Random.int 3 = 0 then
      let branch = branch (random_array images) in
      if branch <> id then (
        Printf.printf "Merging ...%!";
        let* r =
          Store.merge_with_branch t
            ~info:(info image @@ Fmt.str "Merging with %s" branch)
            branch
        in
        match r with
        | Ok () ->
            Printf.printf "ok!\n%!";
            Lwt.return_unit
        | Error _ -> Lwt.fail_with "conflict!")
      else Lwt.return_unit
    else Lwt.return_unit
  in
  let* () = Lwt_unix.sleep (max 0.1 (Random.float 0.3)) in
  process image

let rec protect fn x =
  Lwt.catch
    (fun () -> fn x)
    (fun e ->
      Printf.eprintf "error: %s" (Printexc.to_string e);
      protect fn x)

let rec watchdog () =
  Printf.printf "I'm alive!\n%!";
  let* () = Lwt_unix.sleep 1. in
  watchdog ()

let () =
  let aux () =
    let* () = init () in
    Lwt.choose (watchdog () :: List.map (protect process) (Array.to_list images))
  in
  Lwt_main.run (aux ())
