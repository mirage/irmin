(* Connect to http://localhost:8080/dump *)

open Lwt
open Irmin_unix

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

module Store = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)
module View = Irmin.View(Store)

let config =
  let head = Git.Reference.of_raw "refs/heads/upstream" in
  Irmin_git.config ~root:"/tmp/irmin/test" ~head ~bare:false ()

let task ~user msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = user in
  Irmin.Task.create ~date ~owner msg

(* 1. Cloning the gold image. *)
let provision () =
  let _ = Sys.command "rm -rf /tmp/irmin/test" in
  let _ = Sys.command "mkdir -p /tmp/irmin/test" in
  let task = task ~user:"Automatic VM provisioning" in

  Store.of_tag config task "upstream" >>= fun t ->

  View.empty () >>= fun v ->
  View.update v ["etc"; "manpath"]
    "/usr/share/man\n\
     /usr/local/share/man"
  >>= fun () ->
  View.update v ["bin"; "sh"]
    "�����XpN ������� H__PAGEZERO(__TEXT__text__TEXT [...]"
  >>= fun () ->
  View.merge_path_exn (t "Cloning Ubuntu 14.04 Gold Image.") [] v

  (* 2. VM configuration. *)
let configure () =
  let task = task ~user:"Bob the sysadmin" in
  Store.of_tag config task "local" >>= fun t ->
  Lwt_unix.sleep 2.                >>= fun () ->

  Store.clone_force task (t "Switching to upstream") "upstream" >>= fun t ->
  View.empty ()                    >>= fun v ->

(*
  Store.View.update v ["etc";"passwd"]
    "nobody:*:-2:-2:Unprivileged User:/var/empty:/usr/bin/false\n\
     root:*:0:0:System Administrator:/var/root:/bin/sh\n\
     daemon:*:1:1:System Services:/var/root:/usr/bin/false"
  >>= fun () ->
*)

  View.update v ["etc";"resolv.conf"]
    "domain mydomain.com\n\
     nameserver 128.221.130.23"
  >>= fun () ->

  View.merge_path_exn (t "Network configuration.") [] v

let attack () =

  let task = task ~user:"Remote connection from 132.443.12.444" in

  (* 3. Attacker. *)

  Store.of_tag config task "local" >>= fun t ->
  Lwt_unix.sleep 2.                >>= fun () ->

  Store.update (t "$ vim /etc/resolv.conf")
    ["etc";"resolv.conf"]
    "domain mydomain.com\n\
     nameserver 12.221.130.23"
  >>= fun () ->

  Lwt_unix.sleep 2. >>= fun () ->
  Store.update (t "$ gcc -c /tmp/sh.c -o /bin/sh")
    ["bin";"sh"]
    "�����XpNx ������� H__PAGEZERO(__TEXT__text__TEXT [...]"

let () =
  let error () =
    Printf.eprintf "usage: intrusion [provision|configure|attack]";
    exit 1
  in
  if Array.length Sys.argv <> 2 then error ();
  match Sys.argv.(1) with
  | "provision" -> Lwt_unix.run (provision ())
  | "configure" -> Lwt_unix.run (configure ())
  | "attack"    -> Lwt_unix.run (attack ())
  | _      -> error ()
