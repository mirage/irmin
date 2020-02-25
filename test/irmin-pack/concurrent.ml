open Lwt.Infix

let root = Filename.concat "_build" "test-concurrent"

let src = Logs.Src.create "tests.unix.concurrent" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

let index_log_size = Some 1_000

let parent_pid = Unix.getpid ()

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let keep_max = true
end

module Hash = Irmin.Hash.SHA1
module S =
  Irmin_pack.Make_layered (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Hash)

let config ?(readonly = false) ?(fresh = true) ?(keep_max = Conf.keep_max) root
    =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_layers.config ~conf ~keep_max root

let random_char () = char_of_int (33 + Random.int 94)

let random_string string_size =
  String.init string_size (fun _i -> random_char ())

let info () = Irmin.Info.empty

let init () =
  if Sys.file_exists root then (
    let cmd = Printf.sprintf "rm -rf %s" root in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    () )

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

let test_find_present r n =
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

let add rw tree i =
  Log.debug (fun l -> l "[%d] Adding round %d" parent_pid i);
  S.of_branch rw (branch i) >>= fun t ->
  S.Tree.add tree [ key i ] (value i) >>= fun tree ->
  S.set_tree_exn ~parents:[] ~info t [] tree >>= fun () ->
  S.Head.get t >|= fun c -> (tree, c)

let worker w_pipe r_pipe =
  Log.debug (fun l -> l "Worker started %d" (Unix.getpid ()));
  ignore (read r_pipe);
  Log.debug (fun l -> l "Signal received from parent to create store");
  S.Repo.v (config ~readonly:true ~fresh:false root) >>= fun ro ->
  let rec aux round_nb =
    if round_nb = nb_batch_writes then Lwt.return_unit
    else (
      write w_pipe;
      ignore (read r_pipe);
      Log.debug (fun l -> l "Signal received from parent to read data");
      test_find_present ro (round_nb + 1) >>= fun () -> aux (round_nb + 1) )
  in
  aux 0 >>= fun () -> S.Repo.close ro

let concurrent_reads () =
  let read_main, write_worker = Unix.pipe ()
  and read_worker, write_main = Unix.pipe () in
  init ();
  Printexc.record_backtrace true;
  match Lwt_unix.fork () with
  | 0 -> (
      Log.debug (fun l -> l "[%d] Worker creates child" (Unix.getpid ()));
      match Lwt_unix.fork () with
      | 0 -> worker write_worker read_worker >|= fun () -> exit 0
      | pid ->
          worker write_worker read_worker >>= fun () ->
          wait pid >|= fun () ->
          Log.debug (fun l -> l "Child %d died" pid);
          exit 0 )
  | pid ->
      S.Repo.v (config ~readonly:false ~fresh:true root) >>= fun rw ->
      for _ = 0 to 1 do
        write write_main
      done;
      let rec aux tree round_nb =
        if round_nb = nb_batch_writes then Lwt.return_unit
        else (
          for _ = 0 to 1 do
            let pid = read read_main in
            Log.debug (fun l -> l "Ack from %d" pid)
          done;
          add rw tree round_nb >>= fun (tree, c) ->
          S.freeze rw ~max:[ c ] >>= fun () ->
          S.sync rw;
          for _ = 0 to 1 do
            write write_main
          done;
          (* test_find_present rw (round_nb + 1) >>= fun () -> *)
          aux tree (round_nb + 1) )
      in
      aux S.Tree.empty 0 >>= fun () ->
      wait pid >>= fun () ->
      Unix.close read_main;
      Unix.close write_worker;
      Unix.close read_worker;
      Unix.close write_main;
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
