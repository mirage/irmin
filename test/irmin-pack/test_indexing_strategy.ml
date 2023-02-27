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

open! Import
open Common

let src =
  Logs.Src.create "tests.indexing_strategy" ~doc:"Test indexing strategy"

module Log = (val Logs.src_log src : Logs.LOG)

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ~indexing_strategy ?(readonly = false) ?(fresh = false) () =
  let root = Filename.concat "_build" "test_indexing_strategy" in
  Irmin_pack.config ~readonly ~indexing_strategy ~fresh root

let test_unique_when_switched () =
  let value = "Welt" in
  let get_contents_key store path =
    let* k = Store.key store path in
    match Option.get k with
    | `Node _ -> assert false
    | `Contents contents_key -> Lwt.return contents_key
  in
  let get_direct_key key =
    match Irmin_pack_unix.Pack_key.inspect key with
    | Direct { offset; hash; length; _ } -> (offset, hash, length)
    | _ -> assert false
  in
  let get_key_offset key =
    let offset, _, _ = get_direct_key key in
    offset
  in
  let check_hash msg a b =
    let _, hash_a, _ = get_direct_key a in
    let _, hash_b, _ = get_direct_key b in
    Alcotest.(check_repr Store.hash_t) msg hash_a hash_b
  in

  (* 1. open store with always indexing, verify same offsets *)
  let* repo =
    Store.Repo.v
    @@ config ~indexing_strategy:Irmin_pack.Indexing_strategy.always ~fresh:true
         ()
  in
  let* store = Store.main repo in
  let* first_key =
    let first_path = [ "hello" ] in
    let* () =
      Store.set_exn ~info:(fun () -> Store.Info.empty) store first_path value
    in
    get_contents_key store first_path
  in
  let* second_key =
    let second_path = [ "salut" ] in
    let* () =
      Store.set_exn ~info:(fun () -> Store.Info.empty) store second_path value
    in
    get_contents_key store second_path
  in
  Alcotest.(check int63)
    "offsets should be equal when using always indexing"
    (get_key_offset first_key)
    (get_key_offset second_key);

  let* () = Store.Repo.close repo in

  (* 2. re-open store with minimal indexing, verify new offset *)
  let* repo =
    Store.Repo.v
    @@ config ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
         ~fresh:false ()
  in
  let* store = Store.main repo in
  let* third_key =
    let third_path = [ "hola" ] in
    let* () =
      Store.set_exn ~info:(fun () -> Store.Info.empty) store third_path value
    in
    get_contents_key store third_path
  in
  Alcotest.(check bool)
    "offsets (3rd, 1st) should not be equal when using minimal indexing" false
    (Int63.equal (get_key_offset third_key) (get_key_offset first_key));
  Alcotest.(check bool)
    "offsets (3rd, 2nd) should not be equal when using minimal indexing" false
    (Int63.equal (get_key_offset third_key) (get_key_offset second_key));

  (* 3. verify all hashes are equal *)
  check_hash "hashes are equal (1st, 2nd)" first_key second_key;
  check_hash "hashes are equal (2nd, 3rd)" second_key third_key;

  Store.Repo.close repo

let tests =
  [
    Alcotest_lwt.test_case "test unique when switching strategies" `Quick
      (fun _switch () -> test_unique_when_switched ());
  ]
