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

module Config = struct
  let entries = 2

  let stable_hash = 3
end

let store =
  Irmin_test.store
    (module Irmin_pack.Make (Config))
    (module Irmin.Metadata.None)

let test_file = Filename.concat "_build" "test-db-pack"

let config = Irmin_pack.config ~fresh:false ~lru_size:0 test_file

let clean () =
  let (module S : Irmin_test.S) = store in
  let config = Irmin_pack.config ~fresh:true ~lru_size:0 test_file in
  S.Repo.v config >>= fun repo ->
  S.Repo.branches repo >>= Lwt_list.iter_p (S.Branch.remove repo)

let init () =
  if Sys.file_exists test_file then (
    let cmd = Printf.sprintf "rm -rf %s" test_file in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    () );
  Lwt.return_unit

let stats = None

let suite = { Irmin_test.name = "PACK"; init; clean; config; store; stats }

module Dict = Irmin_pack.Dict

let test_dict () =
  let dict = Dict.v ~fresh:true test_file in
  let x1 = Dict.index dict "foo" in
  Alcotest.(check int) "foo" 0 x1;
  let x1 = Dict.index dict "foo" in
  Alcotest.(check int) "foo" 0 x1;
  let x2 = Dict.index dict "bar" in
  Alcotest.(check int) "bar" 1 x2;
  let x3 = Dict.index dict "toto" in
  Alcotest.(check int) "toto" 2 x3;
  let x4 = Dict.index dict "titiabc" in
  Alcotest.(check int) "titiabc" 3 x4;
  let x1 = Dict.index dict "foo" in
  Alcotest.(check int) "foo" 0 x1;
  let dict2 = Dict.v ~fresh:false test_file in
  let x4 = Dict.index dict2 "titiabc" in
  Alcotest.(check int) "titiabc" 3 x4;
  let v1 = Dict.find dict2 x1 in
  Alcotest.(check (option string)) "find x1" (Some "foo") v1;
  let v2 = Dict.find dict2 x2 in
  Alcotest.(check (option string)) "find x2" (Some "bar") v2;
  let v3 = Dict.find dict2 x3 in
  Alcotest.(check (option string)) "find x3" (Some "toto") v3

let get = function Some x -> x | None -> Alcotest.fail "None"

let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

let hash = Alcotest.testable pp_hash (Irmin.Type.equal Irmin.Hash.SHA1.t)

let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)

module S = struct
  include Irmin.Contents.String

  let magic _ = 'S'

  module H = Irmin.Hash.Typed (Irmin.Hash.SHA1) (Irmin.Contents.String)

  let hash = H.hash

  let encode_bin ~dict:_ ~offset:_ x _k =
    Irmin.Type.encode_bin ~headers:false t x

  let decode_bin ~dict:_ ~hash:_ x off =
    let _, v = Irmin.Type.decode_bin ~headers:false t x off in
    v
end

module H = Irmin.Hash.SHA1
module I = Irmin_pack.Index.Make (H)
module P = Irmin_pack.Pack.File (I) (H)
module Pack = P.Make (S)
module Index = Irmin_pack.Index.Make (Irmin.Hash.SHA1)

let get_index = Index.v ~log_size:10_000_000 ~fan_out_size:16

let test_pack _switch () =
  let root = Filename.dirname test_file in
  let index = get_index ~fresh:true root in
  Pack.v ~fresh:true ~lru_size:0 ~index test_file >>= fun t ->
  let x1 = "foo" in
  let x2 = "bar" in
  let x3 = "otoo" in
  let x4 = "sdadsadas" in
  let h1 = sha1 x1 in
  let h2 = sha1 x2 in
  let h3 = sha1 x3 in
  let h4 = sha1 x4 in
  Pack.batch t (fun w ->
      Lwt_list.iter_s
        (fun (k, v) -> Pack.unsafe_add w k v)
        [ (h1, x1); (h2, x2); (h3, x3); (h4, x4) ] )
  >>= fun () ->
  let test t =
    Pack.find t h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1" x1 y1;
    Pack.find t h3 >|= get >>= fun y3 ->
    Alcotest.(check string) "x3" x3 y3;
    Pack.find t h2 >|= get >>= fun y2 ->
    Alcotest.(check string) "x2" x2 y2;
    Pack.find t h4 >|= get >>= fun y4 ->
    Alcotest.(check string) "x4" x4 y4;
    Lwt.return ()
  in
  test t >>= fun () -> Pack.v ~fresh:false ~index test_file >>= test

let test_readonly_pack _switch () =
  let root = Filename.dirname test_file in
  let index = get_index ~fresh:true root in
  Pack.v ~fresh:true ~index test_file >>= fun w ->
  let index = get_index ~fresh:false ~shared:false ~readonly:true root in
  Pack.v ~fresh:false ~shared:false ~readonly:true ~index test_file
  >>= fun r ->
  let adds l = List.iter (fun (k, v) -> Pack.unsafe_append w k v) l in
  let x1 = "foo" in
  let x2 = "bar" in
  let h1 = sha1 x1 in
  let h2 = sha1 x2 in
  adds [ (h1, x1); (h2, x2) ];
  Pack.find r h2 >>= fun y2 ->
  Alcotest.(check (option string)) "before sync" None y2;
  Pack.sync w;
  Pack.find r h2 >>= fun y2 ->
  Alcotest.(check (option string)) "after sync" (Some x2) y2;
  let x3 = "otoo" in
  let x4 = "sdadsadas" in
  let h3 = sha1 x3 in
  let h4 = sha1 x4 in
  adds [ (h3, x3); (h4, x4) ];
  Pack.sync w;
  Pack.find r h2 >>= fun y2 ->
  Alcotest.(check (option string)) "y2" (Some x2) y2;
  Pack.find r h3 >>= fun y3 ->
  Alcotest.(check (option string)) "y3" (Some x3) y3;
  Lwt.return ()

let test_readonly_dict () =
  let ignore_int (_ : int) = () in
  let w = Dict.v ~fresh:true test_file in
  let r = Dict.v ~fresh:false ~shared:false ~readonly:true test_file in
  let check_index k i = Alcotest.(check int) k i (Dict.index r k) in
  let check_find k i =
    Alcotest.(check (option string)) k (Some k) (Dict.find r i)
  in
  let check_none k i =
    Alcotest.(check (option string)) k None (Dict.find r i)
  in
  let check_raise k =
    try
      ignore_int (Dict.index r k);
      Alcotest.fail "RO dict should not be writable"
    with Irmin_pack.RO_Not_Allowed -> ()
  in
  ignore_int (Dict.index w "foo");
  ignore_int (Dict.index w "foo");
  ignore_int (Dict.index w "bar");
  ignore_int (Dict.index w "toto");
  ignore_int (Dict.index w "titiabc");
  ignore_int (Dict.index w "foo");
  Dict.sync w;
  check_index "titiabc" 3;
  check_index "bar" 1;
  check_index "toto" 2;
  check_find "foo" 0;
  check_raise "xxx";
  ignore_int (Dict.index w "hello");
  check_raise "hello";
  check_none "hello" 4;
  Dict.sync w;
  check_find "hello" 4

module Branch = Irmin_pack.Atomic_write (Irmin.Branch.String) (Irmin.Hash.SHA1)

let test_branch _switch () =
  let branches = [ "foo"; "bar/toto"; "titi" ] in
  let test t =
    Lwt_list.iter_s (fun k -> Branch.set t k (sha1 k)) branches >>= fun () ->
    let check h =
      Branch.find t h >|= fun v ->
      Alcotest.(check (option hash)) h (Some (sha1 h)) v
    in
    Lwt_list.iter_p check branches
  in
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:false test_file >>= fun t ->
  test t >>= fun () ->
  let x = sha1 "XXX" in
  Branch.set t "foo" x >>= fun () ->
  Branch.v ~fresh:false test_file >>= fun t ->
  Branch.find t "foo" >>= fun v ->
  Alcotest.(check (option hash)) "foo" (Some x) v;
  Branch.list t >>= fun br ->
  Alcotest.(check (slist string compare)) "branches" branches br;
  Branch.remove t "foo" >>= fun () ->
  Branch.v ~fresh:false test_file >>= fun t ->
  Branch.find t "foo" >>= fun v ->
  Alcotest.(check (option hash)) "foo none" None v;
  Branch.list t >>= fun br ->
  Alcotest.(check (slist string compare))
    "branches"
    (List.filter (( <> ) "foo") branches)
    br;
  Lwt.return ()

let misc =
  ( "misc",
    [ Alcotest.test_case "dict" `Quick test_dict;
      Alcotest.test_case "RO dict" `Quick test_readonly_dict;
      Alcotest_lwt.test_case "pack" `Quick test_pack;
      Alcotest_lwt.test_case "RO pack" `Quick test_readonly_pack;
      Alcotest_lwt.test_case "branch" `Quick test_branch
    ] )
