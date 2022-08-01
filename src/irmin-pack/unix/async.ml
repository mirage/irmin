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
  (* Async using fork/waitpid*)

  (** This module supports async below. There is a per-process list of pid, that
      will be killed when [clean_up] is called. *)
  module Exit = struct
    let proc_list = ref []
    let m = Mutex.create ()

    let add gc =
      Mutex.lock m;
      proc_list := gc :: !proc_list;
      Mutex.unlock m

    let remove gc =
      Mutex.lock m;
      proc_list := List.filter (fun gc' -> gc <> gc') !proc_list;
      Mutex.unlock m

    let clean_up () =
      List.iter
        (fun gc ->
          try Unix.kill gc 9
          with Unix.Unix_error (e, s1, s2) ->
            [%log.warn
              "Killing gc process with pid %d failed with error (%s, %s, %s)" gc
                (Unix.error_message e) s1 s2])
        !proc_list
  end

  (* Register function to be called when process terminates. If there is a gc
     process running, make sure to terminate it. *)
  let () = at_exit Exit.clean_up

  type status = [ `Running | `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type task = { pid : int; mutable status : status }

  let async f =
    Stdlib.flush_all ();
    match Lwt_unix.fork () with
    | 0 ->
        Lwt_main.Exit_hooks.remove_all ();
        Lwt_main.abandon_yielded_and_paused ();
        f ();
        (* Once the gc is finished, the child process kills itself to
           avoid calling at_exit functions in upstream code. *)
        Unix.kill (Unix.getpid ()) 9;
        assert false (* unreachable *)
    | pid ->
        Exit.add pid;
        { pid; status = `Running }

  let status_of_process_status = function
    | Lwt_unix.WSIGNALED x when x = Sys.sigkill ->
        (* x is actually -7; -7 is the Sys.sigkill definition (not the OS' 9 as might be
           expected) *)
        `Success
        (* the child is killing itself when it's done *)
    | Lwt_unix.WSIGNALED n -> `Failure (Fmt.str "Signaled %d" n)
    | Lwt_unix.WEXITED n -> `Failure (Fmt.str "Exited %d" n)
    | Lwt_unix.WSTOPPED n -> `Failure (Fmt.str "Stopped %d" n)

  let cancel t =
    let () =
      match t.status with
      | `Running ->
          let pid, _ = Unix.waitpid [ Unix.WNOHANG ] t.pid in
          (* Do not block if no child has died yet. In this case the waitpid
             returns immediately with a pid equal to 0. *)
          if pid = 0 then (
            Unix.kill t.pid 9;
            Exit.remove t.pid)
      | _ -> ()
    in
    t.status <- `Cancelled

  let status t : status =
    match t.status with
    | `Running ->
        let pid, status = Unix.waitpid [ Unix.WNOHANG ] t.pid in
        (* Do not block if no child has died yet. In this case the waitpid
           returns immediately with a pid equal to 0. *)
        if pid = 0 then `Running
        else
          let s = status_of_process_status status in
          Exit.remove pid;
          t.status <- s;
          s
    | s -> s

  let await t =
    match t.status with
    | `Running ->
        let+ pid, status = Lwt_unix.waitpid [] t.pid in
        let s = status_of_process_status status in
        Exit.remove pid;
        t.status <- s;
        s
    | s -> Lwt.return send
end
