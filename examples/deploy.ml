open Lwt.Infix
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let config =
  let head = Store.Git.Reference.of_string "refs/heads/upstream" in
  Irmin_git.config ~head ~bare:false Config.root

let info ~user msg () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = user in
  Irmin.Info.v ~date ~author msg

(* 1. Cloning the gold image. *)
let provision repo =
  Config.init ();
  let provision = info ~user:"Automatic VM provisioning" in
  Store.of_branch repo "upstream" >>= fun t ->
  Store.Tree.empty |> fun v ->
  Store.Tree.add v [ "etc"; "manpath" ] "/usr/share/man\n/usr/local/share/man"
  >>= fun v ->
  Store.Tree.add v [ "bin"; "sh" ]
    "�����XpN ������� H__PAGEZERO(__TEXT__text__TEXT \
     [...]"
  >>= fun v ->
  Store.set_tree_exn t ~info:(provision "Cloning Ubuntu 14.04 Gold Image.") [] v

(* 2. VM configuration. *)
let sysadmin = info ~user:"Bob the sysadmin"

let configure repo =
  Store.of_branch repo "upstream" >>= fun t ->
  Lwt_unix.sleep 2. >>= fun () ->
  Store.clone ~src:t ~dst:"dev" >>= fun t ->
  Lwt_unix.sleep 2. >>= fun () ->
  Store.set_exn t
    ~info:(sysadmin "DNS configuration")
    [ "etc"; "resolv.conf" ] "domain mydomain.com\nnameserver 128.221.130.23"
  >>= fun () ->
  Lwt_unix.sleep 2. >>= fun () ->
  Store.clone ~src:t ~dst:"prod" >>= fun _ -> Lwt.return_unit

let attack repo =
  let info = info ~user:"Remote connection from 132.443.12.444" in
  (* 3. Attacker. *)
  Store.of_branch repo "prod" >>= fun t ->
  Lwt_unix.sleep 2. >>= fun () ->
  Store.set_exn t
    ~info:(info "$ vim /etc/resolv.conf")
    [ "etc"; "resolv.conf" ] "domain mydomain.com\nnameserver 12.221.130.23"
  >>= fun () ->
  Lwt_unix.sleep 2. >>= fun () ->
  Store.set_exn t
    ~info:(info "$ gcc -c /tmp/sh.c -o /bin/sh")
    [ "bin"; "sh" ]
    "�����XpNx ������� H__PAGEZERO(__TEXT__text__TEXT \
     [...]"

let revert repo =
  Store.of_branch repo "prod" >>= fun prod ->
  Store.of_branch repo "dev" >>= fun dev ->
  Store.Head.get prod >>= fun h1 ->
  Store.Head.get dev >>= fun h2 ->
  if h1 <> h2 then (
    Printf.printf
      "WARNING: the filesystem is different in dev and prod, intrusion detected!\n\
       Reverting the production system to the dev environment.\n\
       %!";
    Lwt_unix.sleep 2. >>= fun () -> Store.Head.set prod h2)
  else Lwt.return_unit

let () =
  let cmd = Sys.argv.(0) in
  let help () =
    Printf.eprintf
      "This demo models a simple deployment scenario in three phases:\n\n\
      \  - [%s provision] first a VM is provisioned;\n\
      \  - [%s configure] then a sysadmin connects to the machine \n\
      \    and install some software packages;\n\
      \  - [%s attack] an attacker connects to the machine and \n\
      \    injects random code.\n\
      \  - [%s revert] the sysadmin revert the VM in a consistent state.\n\n\
       Run `irmin init -d --root=%s` and Connect your browser \n\
       to http://127.0.0.1:8080/graph to see the system state evolving in \n\
       real-time during the different phases.\n\n\
       Using a VCS-style filesystem allows file modifications to be tracked, \
       with \n\
       user origin and dates. It also supports quickly reverting to a \
       consistent \n\
       state when needed.\n"
      cmd cmd cmd cmd Config.root
  in
  if Array.length Sys.argv <> 2 then help ()
  else
    match Sys.argv.(1) with
    | "provision" ->
        Lwt_main.run (Store.Repo.v config >>= provision);
        Printf.printf
          "The VM is now provisioned. Run `%s configure` to simulate a sysadmin \n\
           configuration.\n"
          cmd
    | "configure" ->
        Lwt_main.run (Store.Repo.v config >>= configure);
        Printf.printf
          "The VM is now configured. Run `%s attack` to simulate an attack by \
           an \n\
           intruder.\n"
          cmd
    | "attack" ->
        Lwt_main.run (Store.Repo.v config >>= attack);
        Printf.printf
          "The VM has been attacked. Run `%s revert` to revert the VM state to \
           a safe one.\n"
          cmd
    | "revert" -> Lwt_main.run (Store.Repo.v config >>= revert)
    | _ -> help ()
