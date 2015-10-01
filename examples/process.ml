(* Connect to http://localhost:8080/dump *)

open Lwt
open Irmin_unix
open Printf

let fmt t x = ksprintf (fun s -> t s) x

let fin () =
  let _ =
    Sys.command (sprintf "cd %s && git reset HEAD --hard" Config.root)
  in
  return_unit

type action = {
  message: string;
  files  : (string list * (unit -> string)) list;
}

type image = {
  name   : string;
  actions: action list;
}

let ubuntu = {
  name    = "official-images/ubuntu:14.04";
  actions = [
    { message = "Updating source lists";
      files   = [ ["etc";"source.list"],
                 fun () -> sprintf "deb %d" (Random.int 10)]; };
    { message = "grep -v '^#' /etc/apt/sources.list";
      files   = []; };
    { message = "cat /etc/issue";
      files = []; }
  ]}

let wordpress = {
  name    = "official-images/wordpress:latest";
  actions = [
    { message = "user logging";
      files   = [["wordpress";"wp-users.php"],
                 fun () -> sprintf "<?php ...%d" (Random.int 10)] };
    { message = "configuration updates";
      files   = [["wordpress";"wp-settings.php"],
                 fun () -> sprintf "<?php .. %d" (Random.int 10) ] };
  ]
}

let mysql = {
  name    = "local/mysql:5.5.41";
  actions = [
    { message = "Reading table wp_users";
      files   = []; };
    { message = "Writing table wp_users";
      files   = [ ["var";"lib";"mysql"],
                  fun () -> sprintf "X%duYYt" (Random.int 10) ]};
    { message = "Reading table wp_posts";
      files   = []; };
    { message = "Writing table wp_posts";
      files   = [ ["var";"lib";"mysql"],
                  fun () -> sprintf "X%dxYYt" (Random.int 10) ]};
  ]
}

let branch image =
  String.map (function
      | ':' -> '/'
      | c   -> c
    ) image.name

let images = [| (*ubuntu; *) wordpress; mysql |]

module Store = Irmin.Basic (Irmin_git.FS) (Irmin.Contents.String)
let config = Irmin_git.config
    ~root:Config.root
    ~bare:true
    ~head:(Git.Reference.of_raw ("refs/heads/" ^ branch images.(0)))
    ()

let task image msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = image.name in
  Irmin.Task.create ~date ~owner msg

let master = branch images.(0)

let init () =
  Config.init ();
  Store.of_branch_id config (task images.(0)) master >>= fun t ->
  Store.update (t "init") ["0"] "0" >>= fun () ->
  Lwt_list.iter_s (fun i ->
      Store.clone_force (task images.(0)) (t "Cloning") (branch i) >>= fun _ ->
      Lwt.return_unit
    ) (Array.to_list images)

let random_array a =
  a.(Random.int (Array.length a))

let random_list l = random_array (Array.of_list l)

let rec process image =
  let id = branch image in
  Printf.printf "Processing %s\n%!" id;
  let actions = random_list image.actions in
  let key, value =
    try random_list actions.files
    with _ -> ["log"; id; "0"], fun () -> id ^ string_of_int (Random.int 10)
  in
  Store.of_branch_id config (task image) id >>= fun t ->
  Store.update (t actions.message) key (value ()) >>= fun () ->

  begin if Random.int 3 = 0 then
    let branch = branch (random_array images) in
    if branch <> id then (
      Printf.printf "Merging ...%!";
      Store.merge_branch_exn (fmt t "Merging with %s" branch) branch >>= fun () ->
      Printf.printf "ok!\n%!";
      Lwt.return_unit
    ) else
      Lwt.return_unit
  else
    return_unit
  end >>= fun () ->

  Lwt_unix.sleep (max 0.1 (Random.float 0.3)) >>= fun () ->
  process image

let rec protect fn x =
  Lwt.catch
    (fun () -> fn x)
    (fun e  ->
       Printf.eprintf "error: %s" (Printexc.to_string e);
       protect fn x)

let rec watchdog () =
  Printf.printf "I'm alive!\n%!";
  Lwt_unix.sleep 1. >>= fun () ->
  watchdog ()

let () =
  let aux () =
    init () >>= fun () ->
    Lwt.join (watchdog () :: List.map (protect process) (Array.to_list images))
  in
  Lwt_unix.run (aux ())
