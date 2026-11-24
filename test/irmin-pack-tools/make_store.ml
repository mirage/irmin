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

(* [make_store root] creates a small store of the Tezos kind at [root]. The
   date of its commit is fixed: the files are the same at each run. *)

module Store = Irmin_tezos.Store

let () =
  let root = Sys.argv.(1) in
  let config =
    Irmin_pack.config ~fresh:true
      ~indexing_strategy:Irmin_pack.Indexing_strategy.always root
  in
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let* repo = Store.Repo.v config in
  let* main = Store.main repo in
  let info () = Store.Info.v ~author:"test" ~message:"test" 0L in
  let* () =
    Store.set_exn ~info main [ "a"; "b"; "c" ] (Bytes.of_string "123")
  in
  Store.Repo.close repo
