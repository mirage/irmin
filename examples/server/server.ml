(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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

open Lwt.Syntax

module Server =
  Irmin_server_unix.Make_ext (Irmin_server.Conn.Codec.Bin) (Config.Store)

let main () =
  let config = Irmin_mem.config () in
  let dashboard = `TCP (`Port 1234) in
  let uri = Config.uri in
  let* server = Server.v ~uri ~dashboard config in
  Logs.debug (fun l -> l "Listening on %a@." Uri.pp uri);
  Server.serve server

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level @@ Some Debug);
  Irmin.Export_for_backends.Logging.reporter (module Mtime_clock)
  |> Logs.set_reporter;
  Lwt_main.run @@ main ()
