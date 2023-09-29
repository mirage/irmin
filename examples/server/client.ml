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

module Client =
  Irmin_client_unix.Make_codec (Irmin_server.Conn.Codec.Bin) (Config.Store)

module Info = Irmin_client_unix.Info (Client.Info)

let info msg = Info.v ~author:"tester" msg

let main () =
  let* client = Client.connect Config.uri in
  let* main = Client.main client in
  let* () =
    Client.set_exn ~info:(info "set testing") main [ "testing" ] "testing"
  in
  let* () =
    Client.set_exn ~info:(info "set remove") main [ "remove" ] "remove"
  in
  let* () = Client.remove_exn ~info:(info "remove remove") main [ "remove" ] in
  let batch =
    Client.Batch.(
      v ()
      |> add_value [ "a"; "b"; "c" ] "123"
      |> add_value [ "foo" ] "bar"
      |> remove [ "testing" ])
  in
  let* c = Client.Batch.apply ~info:(info "apply batch") ~path:[] main batch in
  Logs.info (fun l ->
      l "Applied batch -> commit %a" Irmin.Type.(pp Client.commit_key_t) c);

  let* abc = Client.get main [ "a"; "b"; "c" ] in
  assert (String.equal abc "123");

  let* foo = Client.get main [ "foo" ] in
  assert (foo = "bar");

  let* testing = Client.find main [ "testing" ] in
  assert (Option.is_none testing);

  let* remove = Client.mem main [ "remove" ] in
  assert (remove = false);

  let* commit = Client.Commit.of_key client c in
  let tree = Client.Commit.tree (Option.get commit) in
  let* concrete = Client.Tree.to_concrete tree in

  Logs.info (fun l -> l "%a" Irmin.Type.(pp Client.Tree.concrete_t) concrete);

  Lwt.return_unit

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level @@ Some Debug);
  Irmin.Export_for_backends.Logging.reporter (module Mtime_clock)
  |> Logs.set_reporter;
  Lwt_main.run @@ main ()
