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

(* Simple UI example: connect to http://localhost:8080/dump *)

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

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

let head = Git.Reference.v ("refs/heads/" ^ branch images.(0))
let config = Irmin_git.config ~bare:true ~head Config.root

let info image message () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = image.name in
  Store.Info.v ~author ~message date

let main = branch images.(0)

let init () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let repo = Store.Repo.v ~sw config in
  let t = Store.of_branch repo main in
  Store.set_exn t ~info:(info images.(0) "init") [ "0" ] "0";
  List.iter
    (fun i ->
      let _ = Store.clone ~src:t ~dst:(branch i) in
      ())
    (Array.to_list images)

let random_array a = a.(Random.int (Array.length a))
let random_list l = random_array (Array.of_list l)

let rec process image =
  Eio.Switch.run @@ fun sw ->
  let id = branch image in
  Printf.printf "Processing %s\n%!" id;
  let actions = random_list image.actions in
  let key, value =
    try random_list actions.files
    with _ ->
      ([ "log"; id; "0" ], fun () -> id ^ string_of_int (Random.int 10))
  in
  let repo = Store.Repo.v ~sw config in
  let t = Store.of_branch repo id in
  Store.set_exn t ~info:(info image actions.message) key (value ());
  let () =
    if Random.int 3 = 0 then
      let branch = branch (random_array images) in
      if branch <> id then (
        Printf.printf "Merging ...%!";
        let r =
          Store.merge_with_branch t
            ~info:(info image @@ Fmt.str "Merging with %s" branch)
            branch
        in
        match r with
        | Ok () -> Printf.printf "ok!\n%!"
        | Error _ -> failwith "conflict!")
  in
  Eio_unix.sleep (max 0.1 (Random.float 0.3));
  process image

let rec protect fn x () =
  try fn x
  with e ->
    Printf.eprintf "error: %s" (Printexc.to_string e);
    protect fn x ()

let rec watchdog () =
  Printf.printf "I'm alive!\n%!";
  Eio_unix.sleep 1.;
  watchdog ()

let main () =
  init ();
  Eio.Fiber.any (watchdog :: List.map (protect process) (Array.to_list images))

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
