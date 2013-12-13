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

let debug fmt =
  IrminLog.debug "TEST-CRUD" fmt

let suite server =
  let server_pid = ref 0 in
  {
    name = Printf.sprintf "CRUD.%s" server.name;

    init = begin fun () ->
      let server () =
        server.init ()    >>= fun () ->
        let (module Server) = server.store in
        Server.create () >>= fun t  ->
        IrminHTTP.start_server (module Server) t uri in
      Lwt_io.flush_all () >>= fun () ->
      match Lwt_unix.fork () with
      | 0   ->
        Lwt_unix.set_default_async_method Lwt_unix.Async_none;
        server ()
      | pid -> server_pid := pid; Lwt_unix.sleep 1.
    end;

    clean = begin fun () ->
      IrminHTTP.stop_server uri >>= fun () ->
      Unix.kill !server_pid 9;
      server.clean ();
  end;

  store =
    let module CRUD = IrminCRUD.Make(Cohttp_lwt_unix.Client) in
    let module S = (val CRUD.simple uri) in
    (module S: Irmin.S);
}
