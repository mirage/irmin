open Lwt
open Irmin_unix

(* Install the polling thread for FS notification to work. *)
let () = install_dir_polling_listener 0.5

module Store = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)
module View = Irmin.View(Store)

let config =
  let head = Git.Reference.of_raw "refs/heads/upstream" in
  Irmin_git.config ~root:Config.root ~head ~bare:false ()

let task ~user msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = user in
  Irmin.Task.create ~date ~owner msg

(* 1. Cloning the gold image. *)
let provision () =
  Config.init ();
  let provision = task ~user:"Automatic VM provisioning" in

  Store.of_branch_id config provision "upstream" >>= fun t ->

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
let sysadmin = task ~user:"Bob the sysadmin"
let configure () =
  Store.of_branch_id config sysadmin "upstream" >>= fun t ->

  Lwt_unix.sleep 2.  >>= fun () ->
  Store.clone_force sysadmin (t "Cloning upstream") "dev" >>= fun t ->

  Lwt_unix.sleep 2.  >>= fun () ->
  Store.update (t "DNS configuration") ["etc";"resolv.conf"]
    "domain mydomain.com\nnameserver 128.221.130.23" >>= fun () ->

  Lwt_unix.sleep 2.  >>= fun () ->
  Store.clone_force sysadmin (t "Stable") "prod" >>= fun _ ->
  Lwt.return_unit

let attack () =
  let task = task ~user:"Remote connection from 132.443.12.444" in

  (* 3. Attacker. *)
  Store.of_branch_id config task "prod" >>= fun t ->

  Lwt_unix.sleep 2. >>= fun () ->
  Store.update (t "$ vim /etc/resolv.conf")
    ["etc";"resolv.conf"]
    "domain mydomain.com\n\
     nameserver 12.221.130.23"
  >>= fun () ->

  Lwt_unix.sleep 2. >>= fun () ->
  Store.update (t "$ gcc -c /tmp/sh.c -o /bin/sh")
    ["bin";"sh"]
    "�����XpNx ������� H__PAGEZERO(__TEXT__text__TEXT [...]"

let revert () =
  Store.of_branch_id config sysadmin "prod" >>= fun prod ->
  Store.of_branch_id config sysadmin "dev"  >>= fun dev ->

  Store.head_exn (prod "head") >>= fun h1 ->
  Store.head_exn (dev  "head") >>= fun h2 ->
  if h1 <> h2 then (
    Printf.printf
      "WARNING: the filesystem is different in dev and prod, \
       intrusion detected!\n\
       Reverting the production system to the dev environment.\n%!";
    Lwt_unix.sleep 2. >>= fun () ->
    Store.update_head (prod "rev") h2
  ) else
    Lwt.return_unit

let () =
  let cmd = Sys.argv.(0) in
  let help () =
    Printf.eprintf
      "This demo models a simple deployment scenario in three phases:\n\
       \n\
      \  - [%s provision] first a VM is provisioned;\n\
      \  - [%s configure] then a sysadmin connects to the machine \n\
      \    and install some software packages;\n\
      \  - [%s attack] an attacker connects to the machine and \n\
      \    injects random code.\n\
      \  - [%s revert] the sysadmin revert the VM in a consistent state.\n\
       \n\
       Run `irmin init -d --root=%s` and Connect your browser \n\
       to http://127.0.0.1:8080/graph to see the system state evolving in \n\
       real-time during the different phases.\n\
       \n\
       Using a VCS-style filesystem allows file modifications to be tracked, with \n\
       user origin and dates. It also supports quickly reverting to a consistent \n\
       state when needed.\n"
      cmd cmd cmd cmd Config.root
  in
  if Array.length Sys.argv <> 2 then help ()
  else match Sys.argv.(1) with

    | "provision" ->
      Lwt_unix.run (provision ());
      Printf.printf
        "The VM is now provisioned. Run `%s configure` to simulate a sysadmin \n\
         configuration.\n" cmd

    | "configure" ->
      Lwt_unix.run (configure ());
      Printf.printf
        "The VM is now configured. Run `%s attack` to simulate an attack by an \n\
         intruder.\n" cmd

    | "attack" ->
      Lwt_unix.run (attack ());
      Printf.printf
        "The VM has been attacked. Run `%s revert` to revert the VM state to a \
          safe one.\n" cmd

    | "revert" ->
      Lwt_unix.run (revert ())

    | _  -> help ()
