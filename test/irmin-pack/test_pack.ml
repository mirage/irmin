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

let test_dir = Filename.concat "_build" "test-db-pack"

let suite =
  let store =
    Irmin_test.store
      (module Irmin_pack.Make (Config))
      (module Irmin.Metadata.None)
  in
  let config = Irmin_pack.config ~fresh:false ~lru_size:0 test_dir in
  let init () =
    if Sys.file_exists test_dir then (
      let cmd = Printf.sprintf "rm -rf %s" test_dir in
      Fmt.epr "exec: %s\n%!" cmd;
      let _ = Sys.command cmd in
      () );
    Lwt.return_unit
  in
  let clean () =
    let (module S : Irmin_test.S) = store in
    let config = Irmin_pack.config ~fresh:true ~lru_size:0 test_dir in
    S.Repo.v config >>= fun repo ->
    S.Repo.branches repo >>= Lwt_list.iter_p (S.Branch.remove repo)
  in
  let stats = None in
  { Irmin_test.name = "PACK"; init; clean; config; store; stats }

let fresh_name =
  let c = ref 0 in
  fun () ->
    incr c;
    let name = Filename.concat test_dir ("pack_" ^ string_of_int !c) in
    name

module Dict = Irmin_pack.Dict

let get = function None -> Alcotest.fail "get" | Some x -> x

let test_dict () =
  let dict_name = fresh_name () in
  let dict = Dict.v ~fresh:true dict_name in
  let x1 = Dict.index dict "foo" in
  Alcotest.(check (option int)) "foo" (Some 0) x1;
  let x1 = Dict.index dict "foo" in
  Alcotest.(check (option int)) "foo" (Some 0) x1;
  let x2 = Dict.index dict "bar" in
  Alcotest.(check (option int)) "bar" (Some 1) x2;
  let x3 = Dict.index dict "toto" in
  Alcotest.(check (option int)) "toto" (Some 2) x3;
  let x4 = Dict.index dict "titiabc" in
  Alcotest.(check (option int)) "titiabc" (Some 3) x4;
  let x1 = Dict.index dict "foo" in
  Alcotest.(check (option int)) "foo" (Some 0) x1;
  let dict2 = Dict.v ~fresh:false dict_name in
  let x4 = Dict.index dict2 "titiabc" in
  Alcotest.(check (option int)) "titiabc" (Some 3) x4;
  let v1 = Dict.find dict2 (get x1) in
  Alcotest.(check (option string)) "find x1" (Some "foo") v1;
  let v2 = Dict.find dict2 (get x2) in
  Alcotest.(check (option string)) "find x2" (Some "bar") v2;
  let v3 = Dict.find dict2 (get x3) in
  Alcotest.(check (option string)) "find x3" (Some "toto") v3;
  Dict.close dict;
  let dict3 = Dict.v ~fresh:false dict_name in
  let v1 = Dict.find dict3 (get x1) in
  Alcotest.(check (option string)) "find x1" (Some "foo") v1

let test_readonly_dict () =
  let dict_name = fresh_name () in
  let ignore_int (_ : int option) = () in
  let w = Dict.v ~fresh:true dict_name in
  let r = Dict.v ~fresh:false ~readonly:true dict_name in
  let check_index k i =
    Alcotest.(check (option int)) k (Some i) (Dict.index r k)
  in
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

let get = function Some x -> x | None -> Alcotest.fail "None"

let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

let hash = Alcotest.testable pp_hash (Irmin.Type.equal Irmin.Hash.SHA1.t)

let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)

module S = struct
  include Irmin.Contents.String

  let magic _ = 'S'

  module H = Irmin.Hash.Typed (Irmin.Hash.SHA1) (Irmin.Contents.String)

  let hash = H.hash

  let encode_bin ~dict:_ ~offset:_ x k =
    Irmin.Type.(encode_bin ~headers:false (pair H.t t) (k, x))

  let decode_bin ~dict:_ ~hash:_ x off =
    let _, (_, v) =
      Irmin.Type.(decode_bin ~headers:false (pair H.t t) x off)
    in
    v
end

module H = Irmin.Hash.SHA1
module I = Irmin_pack.Index.Make (H)
module P = Irmin_pack.Pack.File (I) (H)
module Pack = P.Make (S)
module Index = Irmin_pack.Index.Make (Irmin.Hash.SHA1)

let get_index = Index.v ~log_size:10_000_000

let test_pack _switch () =
  let name = fresh_name () in
  let index = get_index ~fresh:true name in
  Pack.v ~fresh:true ~lru_size:0 ~index name >>= fun t ->
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
        [ (h1, x1); (h2, x2); (h3, x3); (h4, x4) ])
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
    Lwt.return_unit
  in
  test t >>= fun () -> Pack.v ~fresh:false ~index name >>= test

let test_readonly_pack _switch () =
  let name = fresh_name () in
  let index = get_index ~fresh:true name in
  Pack.v ~fresh:true ~index name >>= fun w ->
  let index = get_index ~fresh:false ~readonly:true name in
  Pack.v ~fresh:false ~readonly:true ~index name >>= fun r ->
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
  Lwt.return_unit

let test_reuse_index _switch () =
  (* reuse index in new pack *)
  let index = get_index ~fresh:true (fresh_name ()) in
  Pack.v ~fresh:true ~index (fresh_name ()) >>= fun w1 ->
  let x1 = "foo" in
  let h1 = sha1 x1 in
  Pack.unsafe_append w1 h1 x1;
  Pack.find w1 h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "x1" x1 y1;
  Lwt.return ()

let test_close_pack_more _switch () =
  (*open and close in rw*)
  let name = fresh_name () in
  let index = get_index ~fresh:true name in
  Pack.v ~fresh:true ~index name >>= fun w ->
  let x1 = "foo" in
  let h1 = sha1 x1 in
  Pack.unsafe_append w h1 x1;
  Pack.sync w;
  Index.close index;
  Pack.close w >>= fun () ->
  (*open and close in ro*)
  let index = get_index ~fresh:false ~readonly:true name in
  Pack.v ~fresh:false ~readonly:true ~index name >>= fun w1 ->
  Pack.find w1 h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "x1.1" x1 y1;
  Index.close index;
  Pack.close w1 >>= fun () ->
  (* reopen in ro *)
  let index = get_index ~fresh:false name in
  Pack.v ~fresh:false ~index name >>= fun w2 ->
  Pack.find w2 h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "x1.2" x1 y1;

  (*reopen in ro *)
  let index = get_index ~fresh:false ~readonly:true name in
  Pack.v ~fresh:false ~readonly:true ~index name >>= fun w3 ->
  Pack.find w3 h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "x1.3" x1 y1;
  Lwt.return ()

let test_close_pack _switch () =
  let name = fresh_name () in
  let index = get_index ~fresh:true name in
  Pack.v ~fresh:true ~index name >>= fun w ->
  let x1 = "foo" in
  let x2 = "bar" in
  let h1 = sha1 x1 in
  let h2 = sha1 x2 in
  Pack.batch w (fun w ->
      Lwt_list.iter_s
        (fun (k, v) -> Pack.unsafe_add w k v)
        [ (h1, x1); (h2, x2) ])
  >>= fun () ->
  Index.close index;
  Pack.close w >>= fun () ->
  (*reopen pack and index *)
  let index = get_index ~fresh:false name in
  Pack.v ~fresh:false ~index name >>= fun w ->
  Pack.find w h2 >|= get >>= fun y2 ->
  Alcotest.(check string) "close1" x2 y2;
  Pack.find w h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "close2" x1 y1;

  (*open and close two packs *)
  let x3 = "toto" in
  let h3 = sha1 x3 in
  Pack.unsafe_append w h3 x3;
  Pack.v ~fresh:false ~index name >>= fun w2 ->
  Pack.close w >>= fun () ->
  Pack.find w2 h2 >|= get >>= fun y2 ->
  Alcotest.(check string) "close3" x2 y2;
  Pack.find w2 h3 >|= get >>= fun y3 ->
  Alcotest.(check string) "close4" x3 y3;
  Pack.find w2 h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "close5" x1 y1;
  Index.close index;
  Pack.close w2 >>= fun () ->
  (*reopen pack and index in readonly *)
  let index = get_index ~fresh:false ~readonly:true name in
  Pack.v ~fresh:false ~readonly:true ~index name >>= fun r ->
  Pack.find r h1 >|= get >>= fun y1 ->
  Alcotest.(check string) "close6" x1 y1;
  Pack.find r h2 >|= get >>= fun y2 ->
  Alcotest.(check string) "close7" x2 y2;
  Index.close index;
  Pack.close r >>= fun () ->
  (*close index while in use*)
  let index = get_index ~fresh:false name in
  Pack.v ~fresh:false ~index name >>= fun r ->
  Index.close index;
  Pack.find r h1 >>= fun y1 ->
  Alcotest.(check (option string)) "x1" None y1;
  Lwt.return ()

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
  let name = fresh_name () in
  Branch.v ~fresh:true name >>= test >>= fun () ->
  Branch.v ~fresh:true name >>= test >>= fun () ->
  Branch.v ~fresh:true name >>= test >>= fun () ->
  Branch.v ~fresh:false name >>= fun t ->
  test t >>= fun () ->
  let x = sha1 "XXX" in
  Branch.set t "foo" x >>= fun () ->
  Branch.v ~fresh:false name >>= fun t ->
  Branch.find t "foo" >>= fun v ->
  Alcotest.(check (option hash)) "foo" (Some x) v;
  Branch.list t >>= fun br ->
  Alcotest.(check (slist string compare)) "branches" branches br;
  Branch.remove t "foo" >>= fun () ->
  Branch.v ~fresh:false name >>= fun t ->
  Branch.find t "foo" >>= fun v ->
  Alcotest.(check (option hash)) "foo none" None v;
  Branch.list t >>= fun br ->
  Alcotest.(check (slist string compare))
    "branches"
    (List.filter (( <> ) "foo") branches)
    br;
  Lwt.return_unit

let test_close_branch _switch () =
  let branches = [ "foo"; "bar/toto"; "titi" ] in
  let add t =
    Lwt_list.iter_s
      (fun k ->
        Logs.debug (fun l -> l "k = %s, v= %a" k pp_hash (sha1 k));
        Branch.set t k (sha1 k))
      branches
  in
  let test t =
    let check h =
      Branch.find t h >|= fun v ->
      Alcotest.(check (option hash)) h (Some (sha1 h)) v
    in
    Lwt_list.iter_p check branches
  in
  let name = fresh_name () in
  Branch.v ~fresh:true name >>= fun t ->
  add t >>= fun () ->
  test t >>= fun () ->
  Branch.close t >>= fun () ->
  (* restart in readonly *)
  Branch.v ~fresh:false ~readonly:true name >>= fun t ->
  test t >>= fun () ->
  Branch.close t >>= fun () ->
  (*open two instances and close one*)
  let name = fresh_name () in
  Branch.v ~fresh:true name >>= fun t1 ->
  Branch.v ~fresh:false name >>= fun t2 ->
  add t1 >>= fun () ->
  Branch.close t1 >>= fun () -> test t2

let misc =
  ( "misc",
    [
      Alcotest.test_case "dict" `Quick test_dict;
      Alcotest.test_case "RO dict" `Quick test_readonly_dict;
      Alcotest_lwt.test_case "pack" `Quick test_pack;
      Alcotest_lwt.test_case "RO pack" `Quick test_readonly_pack;
      Alcotest_lwt.test_case "branch" `Quick test_branch;
      Alcotest_lwt.test_case "index" `Quick test_reuse_index;
      Alcotest_lwt.test_case "close" `Quick test_close_pack;
      Alcotest_lwt.test_case "close readonly" `Quick test_close_pack_more;
      Alcotest_lwt.test_case "branch close" `Quick test_close_branch;
    ] )
