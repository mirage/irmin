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

let store =
  Irmin_test.store (module Irmin_mem.Make) (module Irmin.Metadata.None)

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
  S.Repo.v config >>= fun repo ->
  clear repo >>= fun () -> S.Repo.close repo

let init () = Lwt.return_unit

let stats = None

let lower_name = "lower"

module S = Irmin_mem.KV (Irmin.Contents.String)

module Tree = struct
  let clear () =
    let x = List.init 830829 (fun i -> string_of_int i) in
    Lwt_list.fold_left_s (fun acc i -> S.Tree.add acc [ i ] i) S.Tree.empty x
    (* Testing [Tree.remove] *)
    >|= fun large_tree -> S.Tree.clear large_tree
end

let misc =
  ( "tree",
    [
      Alcotest.test_case "clear" `Quick (fun () -> Lwt_main.run (Tree.clear ()));
    ] )

let suite =
  {
    Irmin_test.name = "MEM";
    init;
    clean;
    config;
    store;
    stats;
    layered_store = None;
  }
