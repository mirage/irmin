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
  if Sys.file_exists data_dir then (
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ())

module Simple = struct
  let data_dir = "data/pack"

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

  let config root = Irmin_pack.config ~readonly:false ~fresh:true root
  let info = Store.Info.empty

  let create_store () =
    rm_dir data_dir;
    (* make sure the parent directory data/ exists; by default Store.Repo.v will not
       create the containing directory *)
    if not (Sys.file_exists "data") then Unix.mkdir "data" 0o755;
    let rw = Store.Repo.v (config data_dir) in
    let tree = Store.Tree.singleton [ "a"; "b1"; "c1"; "d1"; "e1" ] "x1" in
    let tree = Store.Tree.add tree [ "a"; "b1"; "c1"; "d2"; "e2" ] "x2" in
    let tree = Store.Tree.add tree [ "a"; "b1"; "c1"; "d3"; "e3" ] "x2" in
    let tree = Store.Tree.add tree [ "a"; "b2"; "c2"; "e3" ] "x2" in
    let c1 = Store.Commit.v rw ~parents:[] ~info tree in

    let tree = Store.Tree.add tree [ "a"; "b3" ] "x3" in
    let c2 = Store.Commit.v rw ~parents:[ Store.Commit.key c1 ] ~info tree in

    let tree = Store.Tree.remove tree [ "a"; "b1"; "c1" ] in
    let _ = Store.Commit.v rw ~parents:[ Store.Commit.key c2 ] ~info tree in

    Store.Repo.close rw
end

let generate () = Simple.create_store ()
let () = Eio_main.run @@ fun _env -> generate ()
