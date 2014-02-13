(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Test_store

let uri = Uri.of_string "http://127.0.0.1:8080"

let file = Filename.temp_file "irmin" ".signal"

let signal () =
  let oc = open_out file in
  output_string oc "Server started";
  flush oc;
  close_out oc;
  return_unit

let rec wait_for_the_server_to_start () =
  if Sys.file_exists file then (
    Unix.unlink file;
    return_unit
  ) else
    Lwt_unix.sleep 0.1 >>= fun () ->
    wait_for_the_server_to_start ()

let suite k server =
  let server_pid = ref 0 in
  { name = Printf.sprintf "CRUD.%s" server.name;

    init = begin fun () ->
      let (module Server) = server.store in
      let server () =
        server.init ()   >>= fun () ->
        Server.create () >>= fun t  ->
        signal ()        >>= fun () ->
        IrminHTTP.start_server (module Server) t uri
      in
      let () =
        try Unix.unlink file
        with _ -> () in
      Lwt_io.flush_all () >>= fun () ->
      match Lwt_unix.fork () with
      | 0   ->
        Lwt_unix.set_default_async_method Lwt_unix.Async_none;
        server ()
      | pid ->
        server_pid := pid;
        wait_for_the_server_to_start ()
    end;

    kind = k;

    clean = begin fun () ->
      Unix.kill !server_pid 9;
      let () =
        try ignore (Unix.waitpid [Unix.WUNTRACED] !server_pid)
        with _ -> () in
      server.clean ()
    end;

    store =
      let module CRUD = IrminCRUD.Make(Cohttp_lwt_unix.Client) in
      CRUD.create k uri
  }
