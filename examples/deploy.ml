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
module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

let config =
  let head = Git.Reference.v "refs/heads/upstream" in
  Irmin_git.config ~head ~bare:false Config.root

let info ~user message () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = user in
  Store.Info.v ~author ~message date

(* 1. Cloning the gold image. *)
let provision repo =
  Config.init ();
  let provision = info ~user:"Automatic VM provisioning" in
  let t = Store.of_branch repo "upstream" in
  let v =
    Store.Tree.singleton [ "etc"; "manpath" ]
      "/usr/share/man\n/usr/local/share/man"
  in
  let v =
    Store.Tree.add v [ "bin"; "sh" ]
      "�����XpN ������� H__PAGEZERO(__TEXT__text__TEXT [...]"
  in
  Store.set_tree_exn t ~info:(provision "Cloning Ubuntu 14.04 Gold Image.") [] v

(* 2. VM configuration. *)
let sysadmin = info ~user:"Bob the sysadmin"

let configure repo =
  let t = Store.of_branch repo "upstream" in
  Eio_unix.sleep 2.;
  let t = Store.clone ~src:t ~dst:"dev" in
  Eio_unix.sleep 2.;
  Store.set_exn t
    ~info:(sysadmin "DNS configuration")
    [ "etc"; "resolv.conf" ] "domain mydomain.com\nnameserver 128.221.130.23";
  Eio_unix.sleep 2.;
  let _ = Store.clone ~src:t ~dst:"prod" in
  ()

let attack repo =
  let info = info ~user:"Remote connection from 132.443.12.444" in
  (* 3. Attacker. *)
  let t = Store.of_branch repo "prod" in
  Eio_unix.sleep 2.;
  Store.set_exn t
    ~info:(info "$ vim /etc/resolv.conf")
    [ "etc"; "resolv.conf" ] "domain mydomain.com\nnameserver 12.221.130.23";
  Eio_unix.sleep 2.;
  Store.set_exn t
    ~info:(info "$ gcc -c /tmp/sh.c -o /bin/sh")
    [ "bin"; "sh" ] "�����XpNx ������� H__PAGEZERO(__TEXT__text__TEXT [...]"

let revert repo =
  let prod = Store.of_branch repo "prod" in
  let dev = Store.of_branch repo "dev" in
  let h1 = Store.Head.get prod in
  let h2 = Store.Head.get dev in
  if h1 <> h2 then (
    Printf.printf
      "WARNING: the filesystem is different in dev and prod, intrusion detected!\n\
       Reverting the production system to the dev environment.\n\
       %!";
    Eio_unix.sleep 2.;
    Store.Head.set prod h2)

let main () =
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
  Eio.Switch.run @@ fun sw ->
  if Array.length Sys.argv <> 2 then help ()
  else
    match Sys.argv.(1) with
    | "provision" ->
        (let repo = Store.Repo.v ~sw config in
         provision repo);
        Printf.printf
          "The VM is now provisioned. Run `%s configure` to simulate a sysadmin \n\
           configuration.\n"
          cmd
    | "configure" ->
        (let repo = Store.Repo.v ~sw config in
         configure repo);
        Printf.printf
          "The VM is now configured. Run `%s attack` to simulate an attack by \
           an \n\
           intruder.\n"
          cmd
    | "attack" ->
        (let repo = Store.Repo.v ~sw config in
         attack repo);
        Printf.printf
          "The VM has been attacked. Run `%s revert` to revert the VM state to \
           a safe one.\n"
          cmd
    | "revert" ->
        let repo = Store.Repo.v ~sw config in
        revert repo
    | _ -> help ()

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
