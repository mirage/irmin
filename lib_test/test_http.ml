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
open Test_common
open Irmin_unix

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

let suite ?(content_type=`Raw) server =
  let server_pid = ref 0 in
  let ct_str = Irmin_http_common.string_of_ct content_type in
  { name = Printf.sprintf "HTTP.%s.%s" server.name ct_str;

    init = begin fun () ->
      let (module Server) = server.store in
      let module HTTP = Irmin_http_server.Make(Server) in
      let server () =
        server.init () >>= fun () ->
        Server.Repo.create server.config >>= Server.master task >>= fun t  ->
        signal () >>= fun () ->
        let spec = HTTP.http_spec (t "server") ~strict:true in
        Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8080)) spec
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

    cont = server.cont;
    kind = `Http server.kind;

    clean = begin fun () ->
      Unix.kill !server_pid 9;
      let () =
        try ignore (Unix.waitpid [Unix.WUNTRACED] !server_pid)
        with _ -> () in
      server.clean ()
    end;

    config = Irmin_http.config ~content_type uri;
    store = http_store server.cont;
  }
