(* Connect to http://localhost:8080/dump *)

open Lwt

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let () =
  let origin =
    Printf.sprintf "Irminsule (%s[%d])" (Unix.gethostname()) (Unix.getpid()) in
  IrminOrigin.set_date (fun () -> Int64.of_float (Unix.time ()));
  IrminOrigin.set_id (fun () -> origin);
  IrminOrigin.set_string_of_date (fun d ->
      let tm = Unix.localtime (Int64.to_float d) in
      Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    );
  IrminFS.install_dir_polling_listener 0.5

module Config = struct
  let root = Some "/tmp/irmin/test"
  module Store = Git_fs
  let bare = true
  let disk = true
end

module Git = IrminGit.Make(Config)
module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)

let o id fmt = IrminOrigin.create ~id fmt
let fin () =
  let _ = Sys.command "cd /tmp/irmin/test && git reset HEAD --hard" in
  return_unit

  (* 1. Cloning the gold image. *)
let provision () =
  let _ = Sys.command "rm -rf /tmp/irmin/test" in
  let _ = Sys.command "mkdir -p /tmp/irmin/test" in

  Store.create ~branch:"upstream" () >>= fun t ->

  Store.View.create () >>= fun v ->
  Store.View.update v ["etc"; "manpath"]
    "/usr/share/man\n\
     /usr/local/share/man"
  >>= fun () ->
  Store.View.update v ["bin"; "sh"]
    "�����XpN ������� H__PAGEZERO(__TEXT__text__TEXT [...]"
  >>= fun () ->
  Store.View.merge_path_exn t [] v
    ~origin:(o "Automatic VM provisioning" "Cloning Ubuntu 14.04 Gold Image.")
  >>= fin

  (* 2. VM configuration. *)
let configure () =

  Store.create ~branch:"local" () >>= fun t ->
  Lwt_unix.sleep 2.               >>= fun () ->

  Store.switch t "upstream"       >>= fun () ->
  Store.View.create () >>= fun v ->

(*
  Store.View.update v ["etc";"passwd"]
    "nobody:*:-2:-2:Unprivileged User:/var/empty:/usr/bin/false\n\
     root:*:0:0:System Administrator:/var/root:/bin/sh\n\
     daemon:*:1:1:System Services:/var/root:/usr/bin/false"
  >>= fun () ->
*)

  Store.View.update v ["etc";"resolv.conf"]
    "domain mydomain.com\n\
     nameserver 128.221.130.23"
  >>= fun () ->

  Store.View.merge_path_exn t [] v
    ~origin:(o "Bob the sysadmin" "Network configuration.")
  >>= fin

let attack () =

  (* 3. Attacker. *)

  Store.create ()        >>= fun t ->
  Store.switch t "local" >>= fun () ->
  Lwt_unix.sleep 2.      >>= fun () ->

  Store.update t ["etc";"resolv.conf"]
    "domain mydomain.com\n\
     nameserver 12.221.130.23"
    ~origin:(o "Remote connection from 132.443.12.444" "$ vim /etc/resolv.conf" )
  >>= fun () ->

  Lwt_unix.sleep 2. >>= fun () ->
  Store.update t ["bin";"sh"]
    "�����XpNx ������� H__PAGEZERO(__TEXT__text__TEXT [...]"
    ~origin:(o "Remote connection from 132.443.12.444" "$ gcc -c /tmp/sh.c -o /bin/sh")
  >>= fin

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
