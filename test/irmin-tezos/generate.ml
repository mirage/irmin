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

let rm_dir data_dir =
  if Sys.file_exists data_dir then
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    let _ = Sys.command cmd in
    ()

module Generator = struct
  module Conf = struct
    include Irmin_tezos.Conf

    let entries = 2
    let stable_hash = 3
  end

  module Schema = Irmin.Schema.KV (Irmin.Contents.String)

  module Store = struct
    open Irmin_pack_unix.Maker (Conf)
    include Make (Schema)
  end

  let config ~indexing_strategy root =
    Irmin_pack.config ~indexing_strategy ~readonly:false ~fresh:true root

  let info = Store.Info.empty

  let create_store ~sw ~fs ?(before_closing = fun _repo _head -> ())
      indexing_strategy path =
    rm_dir path;
    let large_contents = String.make 4096 'Z' in
    let rw =
      Store.Repo.v (config ~sw ~fs ~indexing_strategy Eio.Path.(fs / path))
    in
    let tree = Store.Tree.singleton [ "a"; "b1"; "c1"; "d1"; "e1" ] "x1" in
    let tree = Store.Tree.add tree [ "a"; "b1"; "c1"; "d2"; "e2" ] "x2" in
    let tree = Store.Tree.add tree [ "a"; "b1"; "c1"; "d3"; "e3" ] "x2" in
    let tree = Store.Tree.add tree [ "a"; "b2"; "c2"; "e3" ] "x2" in
    let c1 = Store.Commit.v rw ~parents:[] ~info tree in

    let tree = Store.Tree.add tree [ "a"; "b3" ] large_contents in
    let c2 = Store.Commit.v rw ~parents:[ Store.Commit.key c1 ] ~info tree in

    let tree = Store.Tree.remove tree [ "a"; "b1"; "c1" ] in
    let c3 = Store.Commit.v rw ~parents:[ Store.Commit.key c2 ] ~info tree in

    let () = before_closing rw (Store.Commit.key c3) in

    let _ = Store.Repo.close rw in

    c3

  let create_gced_store ~sw ~fs ~domain_mgr path =
    let before_closing repo head =
      let _ = Store.Gc.start_exn ~fs ~domain_mgr repo head in
      let _ = Store.Gc.wait repo in
      ()
    in
    create_store ~sw ~fs ~before_closing Irmin_pack.Indexing_strategy.minimal
      path

  let create_snapshot_store ~sw ~fs ~domain_mgr ~src ~dest =
    let before_closing repo head =
      rm_dir dest;
      Store.create_one_commit_store ~fs ~domain_mgr repo head
        Eio.Path.(fs / dest)
    in
    create_store ~sw ~fs ~before_closing Irmin_pack.Indexing_strategy.minimal
      src
end

let ensure_data_dir () =
  if not (Sys.file_exists "data") then Unix.mkdir "data" 0o755

let generate ~sw ~fs ~domain_mgr =
  ensure_data_dir ();
  let _ =
    Generator.create_store ~sw ~fs Irmin_pack.Indexing_strategy.minimal
      "data/minimal"
  in
  let _ =
    Generator.create_store ~sw ~fs Irmin_pack.Indexing_strategy.always
      "data/always"
  in
  let _ = Generator.create_gced_store ~sw ~fs ~domain_mgr "data/gced" in
  let _ =
    Generator.create_snapshot_store ~sw ~fs ~domain_mgr ~src:"data/snapshot_src"
      ~dest:"data/snapshot"
  in
  ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.cwd env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  generate ~sw ~fs ~domain_mgr
