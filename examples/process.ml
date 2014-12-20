(* Connect to http://localhost:8080/dump *)

open Lwt
open Irmin_unix
open Printf

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let () =
  install_dir_polling_listener 0.5

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)
let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
let fmt t x = ksprintf (fun s -> t s) x

let fin () =
  let _ = Sys.command "cd /tmp/irmin/test && git reset HEAD --hard" in
  return_unit

let commands = [|
  "Getting an incoming connection";
  "Reading the contents of the cache";
  "sudo apt-get upgrade";
  "rsync filer://myfiles.com/me";
|]

let branches = [|
  "12345/cron";
  "112323/maildir";
  "1333/apache";
|]

let master = branches.(0)

let init () =
  let _ = Sys.command "rm -rf /tmp/irmin/test" in
  let _ = Sys.command "mkdir -p /tmp/irmin/test" in
  Irmin.of_tag store config task master >>= fun t ->
  Irmin.update (t "Updateing log/%s/0") ["log"; master; "0"] (master ^ ":0")
  >>= fun () ->
  Lwt_list.iter_s (fun tag ->
      Irmin.of_tag store config task tag >>= fun t ->
      let b = branches.(0) in
      Irmin.switch (fmt t "Switching to %s" b) b
    ) (Array.to_list branches)

let random_array a =
  a.(Random.int (Array.length a))

let rec process ~id count =
  Irmin.of_tag store config task id >>= fun t ->
  Irmin.update (t (random_array commands))
    ["log"; id; string_of_int count]
    (id ^ ":" ^ (string_of_int count))
  >>= fun () ->

  begin if Random.int 2 = 0 then
    let branch = random_array branches in
    Irmin.merge_tag_exn (fmt t "Merging %s with %s" branch id) branch
  else
    return_unit
  end >>= fun () ->

  Lwt_unix.sleep (Random.float 10.)
  >>= fun () ->
  process ~id (count+1)

let () =
  let aux () =
    init () >>= fun () ->
    Lwt.join (List.map
                (fun id -> process ~id 1)
                (Array.to_list branches))
  in
  Lwt_unix.run (aux ())
