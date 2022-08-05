(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
open Lwt.Infix
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Error = Irmin_client.Error

let main =
  let info () = Client.Info.empty in
  let uri = Uri.of_string Sys.argv.(1) in
  let* client = Client.connect uri in

  let* main = Client.main client in
  let* () = Client.set_exn ~info main [ "testing" ] "testing" in
  let* head = Client.Branch.get client Client.Branch.main in
  let* tree =
    Client.Batch.Tree.of_commit client (Client.Commit.hash head) >|= Option.get
  in
  let* tree = Client.Batch.Tree.add client tree [ "b"; "c" ] "123" in
  let* tree = Client.Batch.Tree.add_tree client tree [ "a" ] tree in
  let* tree = Client.Batch.Tree.remove client tree [ "testing" ] in
  let* commit = Client.Batch.commit ~parents:[ head ] ~info client tree in
  let* () = Client.Branch.set client Client.Branch.main commit in
  let* foo = Client.get main [ "foo" ] in
  let* abc = Client.get main [ "a"; "b"; "c" ] in
  let* testing = Client.find main [ "testing" ] in
  assert (foo = "bar");
  assert (abc = "123");
  assert (Option.is_none testing);
  Lwt_io.printlf "foo => %s\na/b/c => %s" foo abc

let () = Lwt_main.run main
