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

let stats watcher () =
  let stats = Irmin_watcher.stats watcher in
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

let init ~sw ~fs ~watcher ~config =
  clean_dirs config;
  Irmin.Backend.Watch.set_listen_dir_eio_hook ~sw fs
    (Irmin_watcher.hook watcher)

let clean ~config =
  clean_dirs config;
  Irmin.Backend.Watch.(set_listen_dir_hook none)

let suite ~sw ~fs ~watcher ~path ~clock =
  let config = Irmin_fs_unix.config ~root:path ~clock in
  Irmin_test.Suite.create ~name:"FS.UNIX" ~init:(init ~sw ~fs ~watcher) ~store
    ~config ~clean ~stats:(stats watcher) ()

module W =
  Irmin.Backend.Watch.Make
    (struct
      type t = string

      let t = Irmin.Type.string
    end)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

(* [await ~clock msg cond] waits until [cond ()] holds, and fails with [msg]
   after 10s. *)
let await ~clock msg cond =
  let rec loop () =
    if not (cond ()) then (
      Eio.Time.sleep clock 0.01;
      loop ())
  in
  match Eio.Time.with_timeout clock 10. (fun () -> Ok (loop ())) with
  | Ok () -> ()
  | Error `Timeout -> Alcotest.fail msg

(* [take_within ~clock s] is [Some v] for the next element [v] of [s], or
   [None] if there is none within 60s. *)
let take_within ~clock s =
  match Eio.Time.with_timeout clock 60. (fun () -> Ok (Eio.Stream.take s)) with
  | Ok v -> Some v
  | Error `Timeout -> None

(* [with_hook ~sw ~fs hook f] is [f ()] with the listen hook [hook] on [fs] and
   the watch notifications on [sw]. The hook is reset to [none] afterwards. *)
let with_hook ~sw ~fs hook f =
  Irmin.Backend.Watch.set_listen_dir_eio_hook ~sw fs hook;
  Fun.protect f ~finally:(fun () ->
      Irmin.Backend.Watch.(set_listen_dir_hook none))

(* [with_dir ~fs name f] is [f dir] for a new empty directory [dir] named
   [name], which is removed afterwards. *)
let with_dir ~fs name f =
  let dir = Eio.Path.(fs / name) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdir ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () -> f dir

(* A file written in a watched directory, outside of Irmin, is reported
   through the listen hook, with its path relative to the directory. *)
let test_listen_dir ~sw ~fs ~watcher ~clock () =
  with_hook ~sw ~fs (Irmin_watcher.hook watcher) @@ fun () ->
  with_dir ~fs "test-listen-dir" @@ fun dir ->
  let value key =
    try Some (Eio.Path.load Eio.Path.(dir / key))
    with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  in
  let workers = Irmin.Backend.Watch.workers () in
  let w = W.v () in
  let stop = W.listen_dir w "test-listen-dir" ~key:Option.some ~value in
  let changes = Eio.Stream.create max_int in
  let id = W.watch_key w "foo" (Eio.Stream.add changes) in
  ( Fun.protect ~finally:(fun () ->
        W.unwatch w id;
        stop ())
  @@ fun () ->
    (* Write [foo] atomically, so that it is never read empty. *)
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(dir / "foo.tmp") "bar";
    Eio.Path.rename Eio.Path.(dir / "foo.tmp") Eio.Path.(dir / "foo");
    match take_within ~clock changes with
    | Some (`Added "bar") -> ()
    | Some _ -> Alcotest.fail "foo should be added with the value bar"
    | None -> Alcotest.fail "no change reported for foo within 60s" );
  (* The notification worker stops once the watch is removed. *)
  await ~clock "the notification worker did not stop" (fun () ->
      Irmin.Backend.Watch.workers () <= workers)

(* Closing a repository stops looking for changes in its directory, even if
   its watches are not removed. *)
let test_close ~sw ~fs ~watcher ~path ~clock () =
  let config = Irmin_fs_unix.config ~root:path ~clock in
  init ~sw ~fs ~watcher ~config;
  Fun.protect ~finally:(fun () -> clean ~config) @@ fun () ->
  let (module S : Irmin_test.S) = store in
  let watchdogs, workers = stats watcher () in
  let repo = S.Repo.v config in
  let _ : S.watch = S.watch (S.main repo) (fun _ -> ()) in
  Alcotest.(check int)
    "a watchdog runs" (watchdogs + 1) (Irmin_watcher.stats watcher).watchdogs;
  S.Repo.close repo;
  await ~clock "the watchdog did not stop" (fun () ->
      stats watcher () = (watchdogs, workers))

module Store = Irmin_fs_unix.KV.Make (Irmin.Contents.String)

let info () = Store.Info.v ~author:"test" ~message:"test" 0L

(* [with_repo ~clock root f] is [f repo] for the store [repo] in [root], which
   is closed afterwards. *)
let with_repo ~clock root f =
  let repo = Store.Repo.v (Irmin_fs_unix.config ~root ~clock) in
  Fun.protect ~finally:(fun () -> Store.Repo.close repo) @@ fun () -> f repo

(* [with_store ~fs ~clock name f] is [f root repo] for a new store [repo] in the
   directory [root] named [name]. *)
let with_store ~fs ~clock name f =
  with_dir ~fs name @@ fun root -> with_repo ~clock root (f root)

(* [in_process_only ~sw ~fs f] is [f ()] with a listen hook that reports no
   change, to test the notifications within the process only. *)
let in_process_only ~sw ~fs f = with_hook ~sw ~fs (fun _ _ _ () -> ()) f

(* Watches on stores whose root is not the current directory see the changes
   made to their files outside of the process, each store watching its own
   files. *)
let test_store_root ~sw ~fs ~watcher ~clock () =
  with_hook ~sw ~fs (Irmin_watcher.hook watcher) @@ fun () ->
  with_store ~fs ~clock "test-store-other" @@ fun _ other ->
  let other_main = Store.main other in
  Store.set_exn other_main ~info [ "a" ] "0";
  let other_changes = Eio.Stream.create max_int in
  let other_watch =
    Store.Branch.watch other "main"
      ~init:(Store.Head.get other_main)
      (Eio.Stream.add other_changes)
  in
  Fun.protect ~finally:(fun () -> Store.unwatch other_watch) @@ fun () ->
  with_store ~fs ~clock "test-store-root" @@ fun root repo ->
  let main = Store.main repo in
  let ref_file = Eio.Path.(root / "refs" / "main") in
  Store.set_exn main ~info [ "a" ] "1";
  let first = Eio.Path.load ref_file in
  Store.set_exn main ~info [ "a" ] "2";
  let changes = Eio.Stream.create max_int in
  let w =
    Store.Branch.watch repo "main" ~init:(Store.Head.get main)
      (Eio.Stream.add changes)
  in
  Fun.protect ~finally:(fun () -> Store.unwatch w) @@ fun () ->
  (* Move [main] back to the first commit, as another process would. *)
  let tmp = Eio.Path.(root / "main.tmp") in
  Eio.Path.save ~create:(`Exclusive 0o600) tmp first;
  Eio.Path.rename tmp ref_file;
  match take_within ~clock changes with
  | Some (`Updated _) ->
      Alcotest.(check int)
        "no change reported for the other store" 0
        (Eio.Stream.length other_changes)
  | Some _ -> Alcotest.fail "main should be updated"
  | None -> Alcotest.fail "no change reported for main within 60s"

(* Stores with the same root share their watch state, also after a garbage
   collection. *)
let test_shared_state ~sw ~fs ~clock () =
  in_process_only ~sw ~fs @@ fun () ->
  with_store ~fs ~clock "test-store-shared" @@ fun root repo ->
  Gc.full_major ();
  with_repo ~clock root @@ fun other ->
  let changes = Eio.Stream.create max_int in
  let w = Store.Branch.watch repo "main" (Eio.Stream.add changes) in
  Fun.protect ~finally:(fun () -> Store.unwatch w) @@ fun () ->
  Store.set_exn (Store.main other) ~info [ "a" ] "1";
  match take_within ~clock changes with
  | Some _ -> ()
  | None ->
      Alcotest.fail
        "no change reported through another store with the same root"

(* Closing a store removes its watches, but not the ones of the other stores
   with the same root. *)
let test_close_shared ~sw ~fs ~clock () =
  in_process_only ~sw ~fs @@ fun () ->
  with_store ~fs ~clock "test-store-close-shared" @@ fun root repo ->
  let changes = Eio.Stream.create max_int in
  let w = Store.Branch.watch repo "main" (Eio.Stream.add changes) in
  Fun.protect ~finally:(fun () -> Store.unwatch w) @@ fun () ->
  let other_changes = Eio.Stream.create max_int in
  ( with_repo ~clock root @@ fun other ->
    (* A watch of the backend, as the callback of [Store.Branch.watch] fails
       once its store is closed. *)
    let _ : Store.Backend.Branch.watch =
      Store.Backend.Branch.watch_key
        (Store.Backend.Repo.branch_t other)
        "main"
        (Eio.Stream.add other_changes)
    in
    () );
  Store.set_exn (Store.main repo) ~info [ "a" ] "1";
  match take_within ~clock changes with
  | Some _ ->
      Alcotest.(check int)
        "no change reported for the closed store" 0
        (Eio.Stream.length other_changes)
  | None -> Alcotest.fail "closing another store removed the watch"

(* A watch whose store is closed while its hook starts fails, and is removed. *)
let test_watch_while_closing ~sw ~fs ~clock () =
  let started, start = Eio.Promise.create () in
  let resumed, resume = Eio.Promise.create () in
  let stopped = ref false in
  let hook _ _ _ =
    Eio.Promise.resolve start ();
    Eio.Promise.await resumed;
    fun () -> stopped := true
  in
  with_hook ~sw ~fs hook @@ fun () ->
  with_store ~fs ~clock "test-store-watch-closing" @@ fun _ repo ->
  Eio.Fiber.both
    (fun () ->
      match Store.Branch.watch repo "main" ignore with
      | _ -> Alcotest.fail "the watch should fail"
      | exception Irmin.Closed -> ())
    (fun () ->
      Eio.Promise.await started;
      Store.Repo.close repo;
      Eio.Promise.resolve resume ());
  Alcotest.(check bool) "the hook is stopped" true !stopped

let misc ~sw ~fs ~watcher ~path ~clock =
  [
    ("listen hook", `Quick, test_listen_dir ~sw ~fs ~watcher ~clock);
    ("close", `Quick, test_close ~sw ~fs ~watcher ~path ~clock);
    ("store root", `Quick, test_store_root ~sw ~fs ~watcher ~clock);
    ("shared state", `Quick, test_shared_state ~sw ~fs ~clock);
    ("close shared", `Quick, test_close_shared ~sw ~fs ~clock);
    ("watch while closing", `Quick, test_watch_while_closing ~sw ~fs ~clock);
  ]
