(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

let stats () =
  let stats = Irmin_watcher.stats () in
  (stats.Irmin_watcher.watchdogs, Irmin.Backend.Watch.workers ())

let test_db = Test_fs.test_db
let store = Irmin_test.store (module Irmin_fs_unix) (module Irmin.Metadata.None)

let clean_dirs config =
  let test_db =
    Irmin.Backend.Conf.find_root config |> Option.value ~default:test_db
  in
  if Sys.file_exists test_db then
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in
    ()

let init ~config =
  clean_dirs config;
  Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook

let clean ~config =
  clean_dirs config;
  Irmin.Backend.Watch.(set_listen_dir_hook none)

let suite ~path ~clock =
  let config = Irmin_fs_unix.config ~root:path ~clock in
  Irmin_test.Suite.create ~name:"FS.UNIX" ~init ~store ~config ~clean ~stats ()

module KV = Irmin_fs_unix.KV.Make (Irmin.Contents.String)

(* The files of a store can be symbolic links, as in the dune sandbox. *)
let test_symlink ~path ~clock () =
  let root = Eio.Path.(path / "_build" / "test-fs-symlink") in
  Eio.Path.rmtree ~missing_ok:true root;
  let config = Irmin_fs_unix.config ~root ~clock in
  let repo = KV.Repo.v config in
  KV.set_exn (KV.main repo) ~info:KV.Info.none [ "a" ] "x";
  KV.Repo.close repo;
  let branch = Eio.Path.(root / "refs" / "main") in
  let target = Eio.Path.(root / "refs" / "main.target") in
  Eio.Path.rename branch target;
  (* A relative link, since [path] is sandboxed. *)
  Unix.symlink "main.target" (Eio.Path.native_exn branch);
  let old_head = Eio.Path.load target in
  let repo = KV.Repo.v config in
  Fun.protect
    ~finally:(fun () -> KV.Repo.close repo)
    (fun () ->
      Alcotest.(check (option string))
        "read through a symbolic link" (Some "x")
        (KV.find (KV.main repo) [ "a" ]);
      KV.set_exn (KV.main repo) ~info:KV.Info.none [ "a" ] "y";
      Alcotest.(check (option string))
        "write" (Some "y")
        (KV.find (KV.main repo) [ "a" ]));
  (* A file is written to a temporary file then renamed, which replaces the
     link and leaves its target untouched. *)
  Alcotest.(check bool)
    "the link is replaced" true
    (Eio.Path.kind ~follow:false branch = `Regular_file);
  Alcotest.(check string) "target untouched" old_head (Eio.Path.load target)

(* A path where no file can be is an absent value, other errors are raised
   rather than taken for an absent value. *)
let test_read_errors ~path ~clock () =
  let root = Eio.Path.(path / "_build" / "test-fs-read-errors") in
  Eio.Path.rmtree ~missing_ok:true root;
  let config = Irmin_fs_unix.config ~root ~clock in
  let repo = KV.Repo.v config in
  Fun.protect
    ~finally:(fun () -> KV.Repo.close repo)
    (fun () ->
      let set branch =
        KV.set_exn (KV.of_branch repo branch) ~info:KV.Info.none [ "a" ] "x"
      in
      let find branch = KV.Branch.find repo branch in
      let absent branch =
        Alcotest.(check bool)
          (branch ^ " is absent") true
          (Option.is_none (find branch))
      in
      set "dir/file";
      absent "dir";
      absent "dir/file/below";
      absent (String.make 300 'a');
      let loop = Eio.Path.(root / "refs" / "loop") in
      Unix.symlink "loop" (Eio.Path.native_exn loop);
      (match find "loop" with
      | _ -> Alcotest.fail "reading a loop of symbolic links should fail"
      | exception Eio.Io _ -> ());
      (* [test_and_set] must not take the file for an absent one. *)
      let head = KV.Commit.key (KV.Head.get (KV.of_branch repo "dir/file")) in
      let branches = KV.Backend.Repo.branch_t repo in
      (match
         KV.Backend.Branch.test_and_set branches "loop" ~test:None
           ~set:(Some head)
       with
      | _ -> Alcotest.fail "test_and_set on a loop of symbolic links"
      | exception Eio.Io _ -> ());
      Alcotest.(check bool)
        "the loop is kept" true
        (Eio.Path.kind ~follow:false loop = `Symbolic_link))

let misc ~path ~clock =
  ( "symlinks",
    [
      Alcotest.test_case "store file" `Quick (test_symlink ~path ~clock);
      Alcotest.test_case "read errors" `Quick (test_read_errors ~path ~clock);
    ] )
