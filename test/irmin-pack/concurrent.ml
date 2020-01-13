open Lwt.Infix

let root = Filename.concat "_build" "test-concurrent"

let src = Logs.Src.create "tests.unix.concurrent" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let index_log_size = Some 1_000

let parent_pid = Unix.getpid ()

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let copy_in_upper = true
end

module Hash = Irmin.Hash.SHA1
module S =
  Irmin_pack.Make_layered (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true)
    ?(copy_in_upper = Conf.copy_in_upper) root =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_pack.config_layers ~conf ~copy_in_upper ()

let random_char () = char_of_int (33 + Random.int 94)

let random_string string_size =
  String.init string_size (fun _i -> random_char ())

let info () = Irmin.Info.empty

let init () =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ())

(* Helpers for Unix pipe *)
let write input =
  try
    let m = Bytes.of_string (string_of_int (Unix.getpid ())) in
    let n = Unix.write input m 0 (Bytes.length m) in
    assert (n = Bytes.length m)
  with Unix.Unix_error (n, f, arg) ->
    Alcotest.failf "Unix error %s %s %s " f arg (Unix.error_message n)

let read output =
  let buff = Bytes.create 5 in
  match Unix.read output buff 0 5 with
  | 0 -> Alcotest.fail "Something wrong when reading from the pipe"
  | n -> int_of_string (Bytes.to_string (Bytes.sub buff 0 n))

let to_bin_string t = Irmin.Type.(unstage (to_bin_string t))

let of_bin_string t = Irmin.Type.(unstage (of_bin_string t))

let write_hash input h =
  let hash_size = Hash.hash_size in
  try
    let buf = Bytes.of_string (to_bin_string Hash.t h) in
    assert (hash_size = Bytes.length buf);
    let n = Unix.write input buf 0 (Bytes.length buf) in
    assert (n = Bytes.length buf)
  with Unix.Unix_error (n, f, arg) ->
    Alcotest.failf "Unix error %s %s %s " f arg (Unix.error_message n)

let read_hash output =
  let hash_size = Hash.hash_size in
  let buf = Bytes.create hash_size in
  let n = Unix.read output buf 0 (Bytes.length buf) in
  assert (n = Bytes.length buf);
  match of_bin_string Hash.t (Bytes.to_string buf) with
  | Ok t -> t
  | Error (`Msg e) -> Fmt.failwith "read hash: %s" e

let wait pid =
  Lwt_unix.waitpid [ Unix.WUNTRACED ] pid >>= fun (pid', status) ->
  if pid <> pid' then
    Alcotest.failf "I'm %d, expecting child %d, but got %d instead"
      (Unix.getpid ()) pid pid';
  match status with
  | Unix.WEXITED 0 ->
      Log.debug (fun l -> l "Child %d finished work" pid);
      Lwt.return_unit
  | Unix.WSTOPPED s ->
      Alcotest.failf "Child %d died unexpectedly stopped by %d" pid s
  | Unix.WEXITED s ->
      Alcotest.failf "Child %d died unexpectedly exited with %d" pid s
  | Unix.WSIGNALED s ->
      Alcotest.failf "Child %d died unexpectedly signaled by %d" pid s

let nb_batch_writes = 5

let branch i = Printf.sprintf "b%d" i

let key i = Printf.sprintf "a%d" i

let value i = Printf.sprintf "x%d" i

let test_find_present_branch r n =
  Log.debug (fun l -> l "[%d] Checking round %d" (Unix.getpid ()) n);
  let rec aux i =
    if i = n then Lwt.return_unit
    else
      S.of_branch r (branch i) >>= fun t ->
      Log.debug (fun l -> l "[%d] head %s" (Unix.getpid ()) (branch i));
      S.Head.get t >>= fun c ->
      S.Commit.of_hash r (S.Commit.hash c) >>= function
      | None -> Alcotest.fail "no hash"
      | Some commit ->
          let msg = Printf.sprintf "RO find %d %d" n i in
          let tree = S.Commit.tree commit in
          S.Tree.find tree [ key i ] >>= fun x ->
          Alcotest.(check (option string)) msg (Some (value i)) x;
          aux (i + 1)
  in
  aux 0

let test_find_present r n h =
  Log.debug (fun l -> l "[%d] Checking round %d" (Unix.getpid ()) n);
  let rec aux i =
    if i = n then Lwt.return_unit
    else
      S.Commit.of_hash r h >>= function
      | None -> Alcotest.fail "no hash"
      | Some commit ->
          let msg = Printf.sprintf "[%d] RO find %d %d" (Unix.getpid ()) n i in
          let tree = S.Commit.tree commit in
          S.Tree.find tree [ key i ] >>= fun x ->
          Alcotest.(check (option string)) msg (Some (value i)) x;
          aux (i + 1)
  in
  aux 0

let _add_on_branch rw tree i =
  Log.debug (fun l -> l "[%d] Adding round %d" parent_pid i);
  S.of_branch rw (branch i) >>= fun t ->
  S.Tree.add tree [ key i ] (value i) >>= fun tree ->
  S.set_tree_exn ~parents:[] ~info t [] tree >>= fun () ->
  S.Head.get t >|= fun c ->
  Log.debug (fun l -> l "Set %s to %a" (branch i) S.Commit.pp_hash c);
  (tree, c)

let add rw tree i =
  Log.debug (fun l -> l "Adding round %d" i);
  S.Tree.add tree [ key i ] (value i) >>= fun tree ->
  S.Commit.v rw ~parents:[] ~info:(info ()) tree >|= fun c ->
  Log.debug (fun l -> l "Commit %a" S.Commit.pp_hash c);
  (tree, c)

let worker w_pipe r_pipe =
  Log.debug (fun l -> l "Worker started %d" (Unix.getpid ()));
  ignore (read r_pipe);
  Log.debug (fun l -> l "Signal received from parent to create store");
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  let rec aux round_nb =
    if round_nb = nb_batch_writes then Lwt.return_unit
    else (
      write w_pipe;
      let h = read_hash r_pipe in
      Log.debug (fun l ->
          l "Signal received from parent to read data %a" (Irmin.Type.pp Hash.t)
            h);
      S.sync ro;
      test_find_present ro (round_nb + 1) h >>= fun () -> aux (round_nb + 1))
  in
  aux 0 >>= fun () -> S.Repo.close ro

let concurrent_reads () =
  (* create two pipes per main/worker: one from main to worker, and another from
     worker to main. *)
  let read_main1, write_worker1 = Unix.pipe ()
  and read_worker1, write_main1 = Unix.pipe ()
  and read_main2, write_worker2 = Unix.pipe ()
  and read_worker2, write_main2 = Unix.pipe () in
  init ();
  Printexc.record_backtrace true;
  match Lwt_unix.fork () with
  | 0 -> (
      Log.debug (fun l -> l "[%d] Worker creates child" (Unix.getpid ()));
      match Lwt_unix.fork () with
      | 0 -> worker write_worker1 read_worker1 >|= fun () -> exit 0
      | pid ->
          worker write_worker2 read_worker2 >>= fun () ->
          wait pid >|= fun () ->
          Log.debug (fun l -> l "Child %d died" pid);
          exit 0)
  | pid ->
      S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
      write write_main1;
      write write_main2;
      let rec aux tree round_nb =
        if round_nb = nb_batch_writes then Lwt.return_unit
        else
          let pid = read read_main1 in
          Log.debug (fun l -> l "Ack from %d" pid);
          let pid = read read_main2 in
          Log.debug (fun l -> l "Ack from %d" pid);
          add rw tree round_nb >>= fun (tree, c) ->
          S.freeze rw ~max:[ c ] >>= fun () ->
          S.flush rw;
          let h = S.Commit.hash c in
          write_hash write_main1 h;
          write_hash write_main2 h;
          aux tree (round_nb + 1)
      in
      aux S.Tree.empty 0 >>= fun () ->
      wait pid >>= fun () ->
      Unix.close read_main1;
      Unix.close read_main2;
      Unix.close write_worker1;
      Unix.close write_worker2;
      Unix.close read_worker1;
      Unix.close read_worker2;
      Unix.close write_main1;
      Unix.close write_main2;
      S.Repo.close rw

let check_os () =
  if Sys.os_type = "Win32" then Lwt.return_unit else concurrent_reads ()

let run_test () = if Unix.getpid () = parent_pid then Lwt_main.run (check_os ())

(*let () =
  report ();
  if Unix.getpid () = parent_pid then Lwt_main.run (check_os ())
*)
let tests =
  [ Alcotest.test_case "Test concurrent reads" `Quick (fun () -> run_test ()) ]
