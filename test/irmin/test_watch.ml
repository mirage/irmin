(*
 * Copyright (c) 2026 Tarides <contact@tarides.com>
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

module Watch = Irmin.Backend.Watch

module W =
  Watch.Make
    (struct
      type t = string

      let t = Irmin.Type.string
    end)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

(* [f ()] is [Some _] after letting the other fibers run a few times. *)
let eventually f =
  let rec loop n =
    match f () with
    | Some x -> Some x
    | None when n = 0 -> None
    | None ->
        Eio.Fiber.yield ();
        loop (n - 1)
  in
  loop 100

let take changes msg =
  match eventually (fun () -> Eio.Stream.take_nonblocking changes) with
  | Some v -> v
  | None -> Alcotest.failf "%s: no notification" msg

let check_stopped workers msg =
  match
    eventually (fun () -> if Watch.workers () = workers then Some () else None)
  with
  | Some () -> ()
  | None -> Alcotest.failf "%s: the notification worker did not stop" msg

let diff =
  Alcotest.testable (Irmin.Type.pp (Irmin.Diff.t Irmin.Type.string)) ( = )

(* [clear] also stops the notification worker. *)
let test_clear_stops_worker ~sw () =
  Watch.set_watch_switch sw;
  let workers = Watch.workers () in
  let w = W.v () in
  let changes = Eio.Stream.create max_int in
  let _ : W.watch = W.watch_key w "k" (Eio.Stream.add changes) in
  W.notify w "k" (Some "v");
  Alcotest.check diff "notified" (`Added "v") (take changes "k");
  Alcotest.(check int) "a worker runs" (workers + 1) (Watch.workers ());
  W.clear w;
  check_stopped workers "clear"

(* [on_failed_switch ~sw f] runs [f sw'] with a new switch [sw'] as the switch
   of the notifications, then sets it back to [sw]. [f] fails [sw'] with
   [Exit]. *)
let on_failed_switch ~sw f =
  Fun.protect ~finally:(fun () -> Watch.set_watch_switch sw) @@ fun () ->
  match
    Eio.Switch.run @@ fun sw' ->
    Watch.set_watch_switch sw';
    f sw'
  with
  | () -> Alcotest.fail "the switch should have failed"
  | exception Exit -> ()

(* A notification sent once the switch of the notifications is off is
   dropped, and the next ones start a new worker on the new switch. *)
let test_switch_off ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let changes = Eio.Stream.create max_int in
  let id = W.watch_key w "k" (Eio.Stream.add changes) in
  on_failed_switch ~sw (fun sw' ->
      Eio.Switch.fail sw' Exit;
      Eio.Cancel.protect (fun () -> W.notify w "k" (Some "lost")));
  W.notify w "k" (Some "v");
  Alcotest.check diff "notified on the new switch"
    (`Updated ("lost", "v"))
    (take changes "k");
  Alcotest.(check bool) "nothing else" true (Eio.Stream.is_empty changes);
  W.unwatch w id;
  check_stopped workers "unwatch"

(* A worker running when its switch is cancelled ends, and the next
   notification starts a new worker on the new switch, even when it is sent
   before the cancelled worker ended. *)
let test_worker_cancelled ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let changes = Eio.Stream.create max_int in
  let id = W.watch_key w "k" (Eio.Stream.add changes) in
  on_failed_switch ~sw (fun sw' ->
      W.notify w "k" (Some "a");
      Alcotest.check diff "notified on the first switch" (`Added "a")
        (take changes "k");
      Eio.Switch.fail sw' Exit;
      Watch.set_watch_switch sw;
      Eio.Cancel.protect (fun () -> W.notify w "k" (Some "b"));
      Alcotest.(check int)
        "a new worker starts before the first one ends" (workers + 2)
        (Watch.workers ()));
  Alcotest.(check int) "the first worker ended" (workers + 1) (Watch.workers ());
  Alcotest.check diff "notified on the new switch"
    (`Updated ("a", "b"))
    (take changes "k");
  W.unwatch w id;
  check_stopped workers "unwatch"

(* [wait_for_calls calls n] lets the other fibers run until [calls] has [n]
   elements. *)
let wait_for_calls calls n =
  match
    eventually (fun () -> if List.length !calls >= n then Some () else None)
  with
  | Some () -> ()
  | None -> Alcotest.failf "%d callbacks expected" n

(* [held_callback calls] is a key handler recording its calls in [calls]. On
   [`Added _], it cannot be cancelled and waits until the returned function is
   called. *)
let held_callback calls =
  let release, set_release = Eio.Promise.create () in
  let f d =
    calls := d :: !calls;
    match d with
    | `Added _ -> Eio.Cancel.protect (fun () -> Eio.Promise.await release)
    | _ -> ()
  in
  (f, fun () -> Eio.Promise.resolve set_release ())

(* A worker cancelled during a callback drops the notifications it has not run
   yet, once the callback returns. *)
let test_cancelled_in_callback ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let calls = ref [] in
  let f, release = held_callback calls in
  let id = W.watch_key w "k" f in
  on_failed_switch ~sw (fun sw' ->
      Fun.protect ~finally:release @@ fun () ->
      W.notify w "k" (Some "a");
      W.notify w "k" (Some "b");
      wait_for_calls calls 1;
      Eio.Switch.fail sw' Exit);
  Alcotest.(check int) "the worker ended" workers (Watch.workers ());
  Alcotest.(check (list diff))
    "only the first callback ran"
    [ `Added "a" ]
    !calls;
  W.unwatch w id

(* [yields ()] lets the other fibers run a few times. *)
let yields () =
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done

(* A worker cancelled while a callback waits stops at once: the next callbacks
   of the same notification do not run. *)
let test_cancelled_waiting ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let calls = ref [] in
  let blocked, _ = Eio.Promise.create () in
  let record d = calls := d :: !calls in
  (* The handlers of a notification run from the last one registered. *)
  let id = W.watch_key w "k" record in
  let id' =
    W.watch_key w "k" (fun d ->
        record d;
        Eio.Promise.await blocked)
  in
  on_failed_switch ~sw (fun sw' ->
      W.notify w "k" (Some "a");
      wait_for_calls calls 1;
      Eio.Switch.fail sw' Exit);
  Alcotest.(check int) "the worker ended" workers (Watch.workers ());
  Alcotest.(check (list diff))
    "only the waiting callback ran"
    [ `Added "a" ]
    !calls;
  W.unwatch w id;
  W.unwatch w id'

(* A new worker waits for a cancelled one to finish its callback, so that the
   callbacks run one at a time and in order. *)
let test_workers_in_order ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let calls = ref [] in
  let f, release = held_callback calls in
  let id = W.watch_key w "k" f in
  on_failed_switch ~sw (fun sw' ->
      Fun.protect ~finally:release @@ fun () ->
      W.notify w "k" (Some "a");
      wait_for_calls calls 1;
      Eio.Switch.fail sw' Exit;
      Watch.set_watch_switch sw;
      Eio.Cancel.protect @@ fun () ->
      W.notify w "k" (Some "b");
      yields ();
      Alcotest.(check (list diff))
        "the second callback waits for the first one"
        [ `Added "a" ]
        !calls);
  wait_for_calls calls 2;
  Alcotest.(check (list diff))
    "in order"
    [ `Updated ("a", "b"); `Added "a" ]
    !calls;
  W.unwatch w id;
  check_stopped workers "unwatch"

(* The callbacks also run in order when the switch of the waiting worker is
   cancelled too. *)
let test_cancelled_twice ~sw () =
  let workers = Watch.workers () in
  let w = W.v () in
  let calls = ref [] in
  let f, release = held_callback calls in
  let id = W.watch_key w "k" f in
  on_failed_switch ~sw (fun sw1 ->
      Fun.protect ~finally:release @@ fun () ->
      W.notify w "k" (Some "a");
      wait_for_calls calls 1;
      Eio.Switch.fail sw1 Exit;
      Eio.Cancel.protect @@ fun () ->
      (* The worker of [b] waits for the first one, and is cancelled. *)
      on_failed_switch ~sw (fun sw2 ->
          W.notify w "k" (Some "b");
          Eio.Switch.fail sw2 Exit);
      W.notify w "k" (Some "c");
      yields ();
      Alcotest.(check (list diff))
        "the third callback waits for the first one"
        [ `Added "a" ]
        !calls);
  wait_for_calls calls 2;
  Alcotest.(check (list diff))
    "in order, without b"
    [ `Updated ("b", "c"); `Added "a" ]
    !calls;
  W.unwatch w id;
  check_stopped workers "unwatch"

(* A callback raising [Cancelled] while the worker is not cancelled fails, and
   the worker goes on. *)
let test_callback_cancelled ~sw () =
  Watch.set_watch_switch sw;
  let workers = Watch.workers () in
  let w = W.v () in
  let changes = Eio.Stream.create max_int in
  let id =
    W.watch_key w "k" (function
      | `Added _ -> raise (Eio.Cancel.Cancelled Exit)
      | d -> Eio.Stream.add changes d)
  in
  W.notify w "k" (Some "a");
  W.notify w "k" (Some "b");
  Alcotest.check diff "notified" (`Updated ("a", "b")) (take changes "k");
  Alcotest.(check bool)
    "the switch is still on" true
    (Option.is_none (Eio.Switch.get_error sw));
  W.unwatch w id;
  check_stopped workers "unwatch"

(* A listen hook that only counts how often it is started and stopped. Like
   the hook of irmin-watcher, it suspends before returning. *)
let counting_hook () =
  let starts = ref 0 and stops = ref 0 in
  let hook _id _dir _f =
    incr starts;
    Eio.Fiber.yield ();
    fun () -> incr stops
  in
  Watch.set_listen_dir_hook hook;
  (starts, stops)

let listen w = W.listen_dir w "dir" ~key:Option.some ~value:(fun _ -> None)

let check_counts msg (starts, stops) (s, s') =
  Alcotest.(check (pair int int)) msg (s, s') (!starts, !stops)

let with_hook f =
  Fun.protect ~finally:(fun () -> Watch.set_listen_dir_hook Watch.none)
  @@ fun () -> f (counting_hook ())

(* Two watches started at the same time share a single listen hook. *)
let test_concurrent_listen () =
  with_hook @@ fun counts ->
  let w = W.v () in
  let stop, stop' = Eio.Fiber.pair (fun () -> listen w) (fun () -> listen w) in
  check_counts "one hook for two listeners" counts (1, 0);
  stop ();
  stop ();
  check_counts "stopping twice counts once" counts (1, 0);
  stop' ();
  check_counts "the last listener stops the hook" counts (1, 1)

(* [clear], which [close] calls, stops the listen hook, and the stop functions
   returned before are then harmless. *)
let test_clear_stops_listening () =
  with_hook @@ fun counts ->
  let w = W.v () in
  let stop = listen w in
  W.clear w;
  check_counts "clear stops the hook" counts (1, 1);
  stop ();
  check_counts "a stale stop does nothing" counts (1, 1);
  let stop = listen w in
  check_counts "listening again starts a new hook" counts (2, 1);
  stop ();
  check_counts "and stops it" counts (2, 2)

let suite ~sw =
  [
    Alcotest.test_case "clear stops the worker" `Quick
      (test_clear_stops_worker ~sw);
    Alcotest.test_case "switch off" `Quick (test_switch_off ~sw);
    Alcotest.test_case "worker cancelled" `Quick (test_worker_cancelled ~sw);
    Alcotest.test_case "cancelled in a callback" `Quick
      (test_cancelled_in_callback ~sw);
    Alcotest.test_case "cancelled while waiting" `Quick
      (test_cancelled_waiting ~sw);
    Alcotest.test_case "workers in order" `Quick (test_workers_in_order ~sw);
    Alcotest.test_case "cancelled twice" `Quick (test_cancelled_twice ~sw);
    Alcotest.test_case "callback cancelled" `Quick (test_callback_cancelled ~sw);
    Alcotest.test_case "concurrent listen" `Quick test_concurrent_listen;
    Alcotest.test_case "clear stops listening" `Quick test_clear_stops_listening;
  ]
