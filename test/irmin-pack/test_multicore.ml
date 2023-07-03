(*
 * Copyright (c) 2018-2023 Tarides <contact@tarides.com>
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

open! Import
open Common

let root = Filename.concat "_build" "test-multicore"
let src = Logs.Src.create "tests.multicore" ~doc:"Tests"

module Log = (val Logs.src_log src : Logs.LOG)

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)

  let config ?(readonly = false) ?(fresh = true) root =
    Irmin_pack.config ~readonly ?index_log_size ~fresh root
end

module Tree = Store.Tree

let info () = Store.Info.empty

type shape = [ `Contents of string | `Node of (string * shape) list ]

let shape : shape =
  `Node
    [
      ("a", `Contents "a");
      ("b", `Contents "b");
      ("c", `Node [ ("d", `Contents "cd"); ("e", `Contents "ce") ]);
      ("f", `Node [ ("g", `Node [ ("h", `Contents "fgh") ]) ]);
      ("i", `Contents "i");
    ]

let rec flatten_shape acc path : shape -> _ = function
  | `Contents c -> (List.rev path, c) :: acc
  | `Node children ->
      List.fold_left
        (fun acc (name, child) -> flatten_shape acc (name :: path) child)
        acc children

let flatten_shape shape = flatten_shape [] [] shape

let make_tree shape =
  List.fold_left
    (fun tree (k, v) -> Tree.add tree k v)
    (Tree.empty ()) (flatten_shape shape)

let make_store shape =
  let repo = Store.Repo.v (Store.config ~fresh:true root) in
  (* let store = Store.empty repo in *)
  let main = Store.main repo in
  let tree = make_tree shape in
  let () = Store.set_tree_exn ~info main [] tree in
  Store.Repo.close repo

let domains_spawn d_mgr ?(nb = 2) fn =
  let count = Atomic.make 0 in
  let fibers =
    List.init nb (fun _ () ->
        Eio.Domain_manager.run d_mgr (fun () ->
            Atomic.incr count;
            while Atomic.get count < nb do
              Domain.cpu_relax ()
            done;
            fn ()))
  in
  Eio.Fiber.all fibers

let test_find d_mgr =
  Logs.set_level None;
  make_store shape;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let paths = flatten_shape shape in
  let find_all () =
    List.iter
      (fun (path, expected) ->
        match Store.Tree.find tree path with
        | None -> assert false
        | Some value -> assert (expected = value))
      paths
  in
  domains_spawn d_mgr find_all;
  Store.Repo.close repo

let rec expected_lengths acc path : shape -> _ = function
  | `Contents _ -> (List.rev path, None) :: acc
  | `Node children ->
      let acc = (List.rev path, Some (List.length children)) :: acc in
      List.fold_left
        (fun acc (name, child) -> expected_lengths acc (name :: path) child)
        acc children

let expected_lengths shape = expected_lengths [] [] shape

let test_length d_mgr =
  Logs.set_level None;
  make_store shape;
  let repo = Store.Repo.v (Store.config ~readonly:true ~fresh:false root) in
  let tree = Store.main repo |> Store.Head.get |> Store.Commit.tree in
  let lengths = expected_lengths shape in
  let find_all () =
    List.iter
      (fun (path, expected) ->
        let value = Store.Tree.length tree path in
        let value = if value = 0 then None else Some value in
        assert (expected = value))
      lengths
  in
  domains_spawn ~nb:8 d_mgr find_all;
  Store.Repo.close repo

let tests d_mgr =
  let tc name fn = Alcotest.test_case name `Quick (fun () -> fn d_mgr) in
  [ tc "find" test_find; tc "length" test_length ]
