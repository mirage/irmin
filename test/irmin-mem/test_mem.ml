(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let store = Irmin_test.store (module Irmin_mem.Make) (module Irmin.Metadata.None)

module Make_layered = Irmin_layers.Make (Irmin_mem.Make)

let layered_store =
  Irmin_test.layered_store (module Make_layered) (module Irmin.Metadata.None)

let config = Irmin_mem.config ()

let clean () =
  let (module S : Irmin_test.S) = store in
  S.Repo.v config >>= fun repo ->
  S.Private.Repo.clear repo >>= fun () ->
  S.Repo.close repo >>= fun () ->
  let (module S : Irmin_test.LAYERED_STORE) = layered_store in
  S.Repo.v config >>= fun repo ->
  S.Private.Repo.clear repo >>= fun () -> S.Repo.close repo

let init () = Lwt.return_unit

let stats = None

let lower_name = "lower"

let suite =
  {
    Irmin_test.name = "MEM";
    init;
    clean;
    config;
    store;
    stats;
    layered_store = Some layered_store;
  }
