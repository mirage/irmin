(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

open! Import
include Async_intf

module Unix = struct
  let kill_no_err pid =
    try Unix.kill pid 9
    with Unix.Unix_error (e, s1, s2) ->
      [%log.warn
        "Killing process with pid %d failed with error (%s, %s, %s)" pid
          (Unix.error_message e) s1 s2]

  (** [Exit] is a stack of PIDs that will be killed [at_exit]. *)
  module Exit = struct
    let proc_list = ref []
    let m = Mutex.create ()

    let add pid =
      Mutex.lock m;
      proc_list := pid :: !proc_list;
      Mutex.unlock m

    let remove pid =
      Mutex.lock m;
      proc_list := List.filter (fun pid' -> pid <> pid') !proc_list;
      Mutex.unlock m

    let () = at_exit @@ fun () -> List.iter kill_no_err !proc_list
  end

  type outcome = [ `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type status = [ `Running | `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type t = { pid : int; mutable status : status }

  let async f =
    Stdlib.flush_all ();
    match Lwt_unix.fork () with
    | 0 ->
        Lwt_main.Exit_hooks.remove_all ();
        Lwt_main.abandon_yielded_and_paused ();
        (try f ()
         with e ->
           [%log.err
             "Unhandled exception in child process %s" (Printexc.to_string e)]);
        (* Once finished, the child process kills itself to avoid calling
           at_exit functions and also to avoid executing the rest of the call
           stack (this would typically be a problem with alcotest). *)
        Unix.kill (Unix.getpid ()) 9;
        assert false (* unreachable *)
    | pid ->
        Exit.add pid;
        { pid; status = `Running }

  let status_of_process_outcome = function
    | Unix.WSIGNALED x when x = Sys.sigkill ->
        (* x is actually -7; -7 is the Sys.sigkill definition (not the OS' 9 as
           might be expected) *)
        `Success
        (* the child is killing itself when it's done *)
    | Unix.WSIGNALED n -> `Failure (Fmt.str "Signaled %d" n)
    | Unix.WEXITED n -> `Failure (Fmt.str "Exited %d" n)
    | Unix.WSTOPPED n -> `Failure (Fmt.str "Stopped %d" n)

  let cancel t =
    let () =
      match t.status with
      | `Running ->
          let pid, _ = Unix.waitpid [ Unix.WNOHANG ] t.pid in
          if pid = 0 then (
            (* Child process is still running. *)
            kill_no_err t.pid;
            Exit.remove t.pid)
      | _ -> ()
    in
    t.status <- `Cancelled

  let status t =
    match t.status with
    | `Running ->
        let pid, status = Unix.waitpid [ Unix.WNOHANG ] t.pid in
        if pid = 0 then `Running
        else
          let s = status_of_process_outcome status in
          Exit.remove pid;
          t.status <- s;
          s
    | #outcome as s -> s

  let await t =
    match t.status with
    | `Running ->
        let pid, status = Unix.waitpid [] t.pid in
        let s = status_of_process_outcome status in
        Exit.remove pid;
        t.status <- s;
        s
    | #outcome as s -> s
end
