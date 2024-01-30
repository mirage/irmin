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

(* Simple example of reading and writing in a Git repository *)

let info = Irmin_git_unix.info

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

let update t k v =
  let msg = Fmt.str "Updating /%s" (String.concat "/" k) in
  print_endline msg;
  Store.set_exn t ~info:(info "%s" msg) k v

let read_exn t k =
  let msg = Fmt.str "Reading /%s" (String.concat "/" k) in
  print_endline msg;
  Store.get t k

let main () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let config = Irmin_git.config ~bare:true Config.root in
  let repo = Store.Repo.v ~sw config in
  let t = Store.main repo in
  update t [ "root"; "misc"; "1.txt" ] "Hello world!";
  update t [ "root"; "misc"; "2.txt" ] "Hi!";
  update t [ "root"; "misc"; "3.txt" ] "How are you ?";
  let _ = read_exn t [ "root"; "misc"; "2.txt" ] in
  let x = Store.clone ~src:t ~dst:"test" in
  print_endline "cloning ...";
  update t [ "root"; "misc"; "3.txt" ] "Hohoho";
  update x [ "root"; "misc"; "2.txt" ] "Cool!";
  let r = Store.merge_into ~info:(info "t: Merge with 'x'") x ~into:t in
  match r with
  | Error _ -> failwith "conflict!"
  | Ok () ->
      print_endline "merging ...";
      let _ = read_exn t [ "root"; "misc"; "2.txt" ] in
      let _ = read_exn t [ "root"; "misc"; "3.txt" ] in
      ()

let main () =
  Printf.printf
    "This example creates a Git repository in %s and use it to read \n\
     and write data:\n"
    Config.root;
  let _ = Sys.command (Printf.sprintf "rm -rf %s" Config.root) in
  main ();
  Printf.printf "You can now run `cd %s && tig` to inspect the store.\n"
    Config.root

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
