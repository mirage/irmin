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
  Irmin_test.store (module Irmin_pack.Make) (module Irmin.Metadata.None)

let test_file = "test-db-pack"

let config = Irmin_pack.config ~fresh:false test_file

let clean () =
  let (module S : Irmin_test.S) = store in
  let config = Irmin_pack.config ~fresh:true test_file in
  S.Repo.v config >>= fun repo ->
  S.Repo.branches repo >>= Lwt_list.iter_p (S.Branch.remove repo)

let init () =
  ( if Sys.file_exists test_file then
    let cmd = Printf.sprintf "rm -rf %s" test_file in
    let _ = Sys.command cmd in
    () );
  Lwt.return_unit

let stats = None

let suite = { Irmin_test.name = "PACK"; init; clean; config; store; stats }

module Dict = Irmin_pack.Dict

let test_dict _switch () =
  Dict.v ~fresh:true test_file >>= fun dict ->
  Dict.index dict "foo" >>= fun x1 ->
  Alcotest.(check int) "foo" 0 x1;
  Dict.index dict "foo" >>= fun x1 ->
  Alcotest.(check int) "foo" 0 x1;
  Dict.index dict "bar" >>= fun x2 ->
  Alcotest.(check int) "bar" 1 x2;
  Dict.index dict "toto" >>= fun x3 ->
  Alcotest.(check int) "toto" 2 x3;
  Dict.index dict "titiabc" >>= fun x4 ->
  Alcotest.(check int) "titiabc" 3 x4;
  Dict.index dict "foo" >>= fun x1 ->
  Alcotest.(check int) "foo" 0 x1;
  Dict.v ~fresh:false test_file >>= fun dict2 ->
  Dict.index dict2 "titiabc" >>= fun x4 ->
  Alcotest.(check int) "titiabc" 3 x4;
  Dict.find dict2 x1 >>= fun v1 ->
  Alcotest.(check (option string)) "find x1" (Some "foo") v1;
  Dict.find dict2 x2 >>= fun v2 ->
  Alcotest.(check (option string)) "find x2" (Some "bar") v2;
  Dict.find dict2 x3 >>= fun v3 ->
  Alcotest.(check (option string)) "find x3" (Some "toto") v3;
  Lwt.return ()

module Index = Irmin_pack.Index (Irmin.Hash.SHA1)

let get = function Some x -> x | None -> Alcotest.fail "None"

let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

let hash = Alcotest.testable pp_hash (Irmin.Type.equal Irmin.Hash.SHA1.t)

let test_index _switch () =
  Index.v ~fresh:true test_file >>= fun t ->
  let h1 = Irmin.Hash.SHA1.digest "foo" in
  let o1 = 42L in
  let h2 = Irmin.Hash.SHA1.digest "bar" in
  let o2 = 142L in
  let h3 = Irmin.Hash.SHA1.digest "otoo" in
  let o3 = 10_098L in
  let h4 = Irmin.Hash.SHA1.digest "sdadsadas" in
  let o4 = 8978_232L in
  Lwt_list.iter_s
    (fun (h, off) -> Index.append t h ~off ~len:42)
    [ (h1, o1); (h2, o2); (h3, o3); (h4, o4) ]
  >>= fun () ->
  let test t =
    Index.find t h1 >|= get >>= fun x1 ->
    Alcotest.(check int64) "h1" o1 x1.offset;
    Index.find t h2 >|= get >>= fun x2 ->
    Alcotest.(check int64) "h2" o2 x2.offset;
    Alcotest.(check int) "h2 len" 42 x2.len;
    Index.find t h3 >|= get >>= fun x3 ->
    Alcotest.(check int64) "h3" o3 x3.offset;
    Index.find t h4 >|= get >>= fun x4 ->
    Alcotest.(check int64) "h4" o4 x4.offset;
    Lwt_list.iter_s
      (fun (h, o) ->
        Index.find t h >|= get >|= fun e ->
        Alcotest.(check int64) "entry off" o e.offset;
        Alcotest.(check hash) "entry hash" h e.hash )
      [ (h1, o1); (h2, o2); (h3, o3) ]
  in
  test t >>= fun () -> Index.v ~fresh:false test_file >>= test

module S = struct
  include Irmin.Contents.String

  type hash = Irmin.Hash.SHA1.t

  let to_bin ~dict:_ ~offset:_ x _k = Lwt.return (Irmin.Type.to_bin_string t x)

  let decode_bin ~dict:_ ~hash:_ x off =
    Fmt.epr "XXX 1\n%!";
    let _, v = Irmin.Type.decode_bin ~headers:false t x off in
    Fmt.epr "XXX 2\n%!";
    Lwt.return v
end

module P = Irmin_pack.Pack (Irmin.Hash.SHA1)
module Pack = P.Make (S)

let test_pack _switch () =
  Pack.v ~fresh:true test_file >>= fun t ->
  let x1 = "foo" in
  let x2 = "bar" in
  let x3 = "otoo" in
  let x4 = "sdadsadas" in
  let h1 = Irmin.Hash.SHA1.digest x1 in
  let h2 = Irmin.Hash.SHA1.digest x2 in
  let h3 = Irmin.Hash.SHA1.digest x3 in
  let h4 = Irmin.Hash.SHA1.digest x4 in
  Lwt_list.iter_s
    (fun (k, v) -> Pack.append t k v)
    [ (h1, x1); (h2, x2); (h3, x3); (h4, x4) ]
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
  test t >>= fun () -> Pack.v ~fresh:false test_file >>= test

module Branch = Irmin_pack.Atomic_write (Irmin.Branch.String) (Irmin.Hash.SHA1)

let test_branch _switch () =
  let branches = [ "foo"; "bar/toto"; "titi" ] in
  let test t =
    Lwt_list.iter_s
      (fun k -> Branch.set t k (Irmin.Hash.SHA1.digest k))
      branches
    >>= fun () ->
    let check h =
      Branch.find t h >|= fun v ->
      Alcotest.(check (option hash)) h (Some (Irmin.Hash.SHA1.digest h)) v
    in
    Lwt_list.iter_p check branches
  in
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:true test_file >>= test >>= fun () ->
  Branch.v ~fresh:false test_file >>= fun t ->
  test t >>= fun () ->
  let x = Irmin.Hash.SHA1.digest "XXX" in
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
    [ Alcotest_lwt.test_case "dict" `Quick test_dict;
      Alcotest_lwt.test_case "index" `Quick test_index;
      Alcotest_lwt.test_case "pack" `Quick test_pack;
      Alcotest_lwt.test_case "branch" `Quick test_branch
    ] )
