(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Irmin.Export_for_backends

let store = Irmin_test.store (module Irmin_mem) (module Irmin.Metadata.None)
let config = Irmin_mem.config ()

let clean () =
  let (module S : Irmin_test.S) = store in
  let module P = S.Private in
  let clear repo =
    Lwt.join
      [
        P.Commit.clear (P.Repo.commit_t repo);
        P.Node.clear (P.Repo.node_t repo);
        P.Contents.clear (P.Repo.contents_t repo);
        P.Branch.clear (P.Repo.branch_t repo);
      ]
  in
  let* repo = S.Repo.v config in
  let* () = clear repo in
  S.Repo.close repo

let init () = Lwt.return_unit
let stats = None
let lower_name = "lower"

let suite =
  Irmin_test.Suite.create ~name:"MEM" ~init ~store ~config ~clean ~stats:None
    ~layered_store:None
