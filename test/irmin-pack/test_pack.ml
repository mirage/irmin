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
open Common

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
    >>= fun () -> S.Repo.close repo
  in
  let stats = None in
  { Irmin_test.name = "PACK"; init; clean; config; store; stats }

module Context = Make_context (struct
  let root = test_dir
end)

module Dict = struct
  let test_dict () =
    let Context.{ dict; clone } = Context.get_dict () in
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
    let dict2 = clone ~readonly:false in
    let x4 = Dict.index dict2 "titiabc" in
    Alcotest.(check (option int)) "titiabc" (Some 3) x4;
    let v1 = Dict.find dict2 (get x1) in
    Alcotest.(check (option string)) "find x1" (Some "foo") v1;
    let v2 = Dict.find dict2 (get x2) in
    Alcotest.(check (option string)) "find x2" (Some "bar") v2;
    let v3 = Dict.find dict2 (get x3) in
    Alcotest.(check (option string)) "find x3" (Some "toto") v3;
    Dict.close dict;
    let dict3 = clone ~readonly:false in
    let v1 = Dict.find dict3 (get x1) in
    Alcotest.(check (option string)) "find x1" (Some "foo") v1;
    Dict.close dict2;
    Dict.close dict3

  let test_readonly_dict () =
    let ignore_int (_ : int option) = () in
    let Context.{ dict; clone } = Context.get_dict () in
    let r = clone ~readonly:true in
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
    ignore_int (Dict.index dict "foo");
    ignore_int (Dict.index dict "foo");
    ignore_int (Dict.index dict "bar");
    ignore_int (Dict.index dict "toto");
    ignore_int (Dict.index dict "titiabc");
    ignore_int (Dict.index dict "foo");
    Dict.flush dict;
    Dict.sync r;
    check_index "titiabc" 3;
    check_index "bar" 1;
    check_index "toto" 2;
    check_find "foo" 0;
    check_raise "xxx";
    ignore_int (Dict.index dict "hello");
    check_raise "hello";
    check_none "hello" 4;
    Dict.flush dict;
    Dict.sync r;
    check_find "hello" 4;
    Dict.close dict;
    Dict.close r

  let tests =
    [
      Alcotest.test_case "dict" `Quick test_dict;
      Alcotest.test_case "RO dict" `Quick test_readonly_dict;
    ]
end

module Pack = struct
  let test_pack () =
    Context.get_pack () >>= fun t ->
    let x1 = "foo" in
    let x2 = "bar" in
    let x3 = "otoo" in
    let x4 = "sdadsadas" in
    let h1 = sha1 x1 in
    let h2 = sha1 x2 in
    let h3 = sha1 x3 in
    let h4 = sha1 x4 in
    Pack.batch t.pack (fun w ->
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
    test t.pack >>= fun () ->
    t.clone_pack ~readonly:false >>= fun pack2 ->
    test pack2 >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Pack.close pack2

  let test_readonly_pack () =
    Context.get_pack () >>= fun t ->
    t.clone_index_pack ~readonly:true >>= fun (i, r) ->
    let test w =
      let adds l = List.iter (fun (k, v) -> Pack.unsafe_append w k v) l in
      let x1 = "foo" in
      let x2 = "bar" in
      let h1 = sha1 x1 in
      let h2 = sha1 x2 in
      adds [ (h1, x1); (h2, x2) ];
      Pack.find r h2 >>= fun y2 ->
      Alcotest.(check (option string)) "before sync" None y2;
      Pack.flush w;
      Pack.sync r;
      Pack.find r h2 >>= fun y2 ->
      Alcotest.(check (option string)) "after sync" (Some x2) y2;
      let x3 = "otoo" in
      let x4 = "sdadsadas" in
      let h3 = sha1 x3 in
      let h4 = sha1 x4 in
      adds [ (h3, x3); (h4, x4) ];
      Pack.flush w;
      Pack.sync r;
      Pack.find r h2 >>= fun y2 ->
      Alcotest.(check (option string)) "y2" (Some x2) y2;
      Pack.find r h3 >>= fun y3 ->
      Alcotest.(check (option string)) "y3" (Some x3) y3;
      Lwt.return_unit
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let test_reuse_index () =
    (* index and pack with different names. However, this behaviour is not exposed by irmin_pack.*)
    let index = Index.v ~log_size:4 ~fresh:true (Context.fresh_name "index") in
    Pack.v ~fresh:true ~index (Context.fresh_name "pack") >>= fun w1 ->
    let x1 = "foo" in
    let h1 = sha1 x1 in
    Pack.unsafe_append w1 h1 x1;
    Pack.find w1 h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1" x1 y1;
    Index.close index;
    Pack.close w1

  let test_close_pack_more () =
    (*open and close in rw*)
    Context.get_pack () >>= fun t ->
    let w = t.pack in
    let x1 = "foo" in
    let h1 = sha1 x1 in
    Pack.unsafe_append w h1 x1;
    Pack.flush w;
    Index.close t.index;
    Pack.close w >>= fun () ->
    (*open and close in ro*)
    t.clone_index_pack ~readonly:true >>= fun (i1, w1) ->
    Pack.find w1 h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.1" x1 y1;
    Context.close i1 w1 >>= fun () ->
    (* reopen in rw *)
    t.clone_index_pack ~readonly:false >>= fun (i2, w2) ->
    Pack.find w2 h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.2" x1 y1;

    (*reopen in ro *)
    t.clone_index_pack ~readonly:true >>= fun (i3, w3) ->
    Pack.find w3 h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.3" x1 y1;
    Context.close i2 w2 >>= fun () -> Context.close i3 w3

  let test_close_pack () =
    Context.get_pack () >>= fun t ->
    let w = t.pack in
    let x1 = "foo" in
    let x2 = "bar" in
    let h1 = sha1 x1 in
    let h2 = sha1 x2 in
    Pack.batch w (fun w ->
        Lwt_list.iter_s
          (fun (k, v) -> Pack.unsafe_add w k v)
          [ (h1, x1); (h2, x2) ])
    >>= fun () ->
    Context.close t.index w >>= fun () ->
    (*reopen pack and index *)
    t.clone_index_pack ~readonly:false >>= fun (i, w) ->
    Pack.find w h2 >|= get >>= fun y2 ->
    Alcotest.(check string) "x2.1" x2 y2;
    Pack.find w h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.1" x1 y1;

    (*open and close two packs *)
    let x3 = "toto" in
    let h3 = sha1 x3 in
    Pack.unsafe_append w h3 x3;
    t.clone_pack ~readonly:false >>= fun w2 ->
    Pack.close w >>= fun () ->
    Pack.find w2 h2 >|= get >>= fun y2 ->
    Alcotest.(check string) "x2.2" x2 y2;
    Pack.find w2 h3 >|= get >>= fun y3 ->
    Alcotest.(check string) "x3.2" x3 y3;
    Pack.find w2 h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.2" x1 y1;
    Context.close i w2 >>= fun () ->
    (*reopen pack and index in readonly *)
    t.clone_index_pack ~readonly:true >>= fun (i, r) ->
    Pack.find r h1 >|= get >>= fun y1 ->
    Alcotest.(check string) "x1.3" x1 y1;
    Pack.find r h2 >|= get >>= fun y2 ->
    Alcotest.(check string) "x2.3" x2 y2;
    Context.close i r >>= fun () ->
    (*close index while in use*)
    t.clone_index_pack ~readonly:false >>= fun (i, r) ->
    Index.close i;
    Lwt.catch
      (fun () ->
        Pack.find r h1 >>= fun _ ->
        Alcotest.fail "Add after closing the index should not be allowed")
      (function I.Closed -> Lwt.return_unit | exn -> Lwt.fail exn)

  (** Index can be flushed to disk independently of pack, we simulate this in
      the tests using [Index.filter] and [Index.flush]. Regression test for PR
      1008 in which values were indexed before being reachable in pack. *)
  let readonly_sync_index_flush () =
    Context.get_pack () >>= fun t ->
    t.clone_index_pack ~readonly:true >>= fun (i, r) ->
    let test w =
      let x1 = "foo" in
      let h1 = sha1 x1 in
      Pack.unsafe_append w h1 x1;
      Pack.sync r;
      Pack.find r h1 >>= fun y1 ->
      Alcotest.(check (option string)) "sync before filter" None y1;
      Index.filter t.index (fun _ -> true);
      Pack.sync r;
      Pack.find r h1 >>= fun y1 ->
      Alcotest.(check (option string)) "sync after filter" (Some x1) y1;
      let x2 = "foo" in
      let h2 = sha1 x2 in
      Pack.unsafe_append w h2 x2;
      Index.flush t.index;
      Pack.find r h2 >|= fun y2 ->
      Alcotest.(check (option string)) "sync after flush" (Some x2) y2
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let readonly_find_index_flush () =
    Context.get_pack () >>= fun t ->
    t.clone_index_pack ~readonly:true >>= fun (i, r) ->
    let check h x msg =
      Pack.find r h >|= fun y -> Alcotest.(check (option string)) msg (Some x) y
    in
    let test w =
      let x1 = "foo" in
      let h1 = sha1 x1 in
      Pack.unsafe_append w h1 x1;
      Pack.flush t.pack;
      Pack.sync r;
      check h1 x1 "find before filter" >>= fun () ->
      Index.filter t.index (fun _ -> true);
      check h1 x1 "find after filter" >>= fun () ->
      let x2 = "bar" in
      let h2 = sha1 x2 in
      Pack.unsafe_append w h2 x2;
      Pack.flush t.pack;
      Pack.sync r;
      check h2 x2 "find before flush" >>= fun () ->
      let x3 = "toto" in
      let h3 = sha1 x3 in
      Pack.unsafe_append w h3 x3;
      Index.flush t.index;
      check h2 x2 "find after flush" >>= fun () ->
      Pack.flush t.pack;
      Pack.sync r;
      check h3 x3 "find after flush new values"
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let tests =
    [
      Alcotest.test_case "pack" `Quick (fun () -> Lwt_main.run (test_pack ()));
      Alcotest.test_case "RO pack" `Quick (fun () ->
          Lwt_main.run (test_readonly_pack ()));
      Alcotest.test_case "index" `Quick (fun () ->
          Lwt_main.run (test_reuse_index ()));
      Alcotest.test_case "close" `Quick (fun () ->
          Lwt_main.run (test_close_pack ()));
      Alcotest.test_case "close readonly" `Quick (fun () ->
          Lwt_main.run (test_close_pack_more ()));
      Alcotest.test_case "readonly sync, index flush" `Quick (fun () ->
          Lwt_main.run (readonly_sync_index_flush ()));
      Alcotest.test_case "readonly find, index flush" `Quick (fun () ->
          Lwt_main.run (readonly_find_index_flush ()));
    ]
end

module Branch = struct
  module Branch =
    Irmin_pack.Atomic_write (Irmin.Branch.String) (Irmin.Hash.SHA1)

  let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

  let hash = Alcotest.testable pp_hash (Irmin.Type.equal Irmin.Hash.SHA1.t)

  let test_branch () =
    let branches = [ "foo"; "bar/toto"; "titi" ] in
    let test t =
      Lwt_list.iter_s (fun k -> Branch.set t k (sha1 k)) branches >>= fun () ->
      let check h =
        Branch.find t h >|= fun v ->
        Alcotest.(check (option hash)) h (Some (sha1 h)) v
      in
      Lwt_list.iter_p check branches
    in
    let name = Context.fresh_name "branch" in
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

  let test_close_branch () =
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
    let name = Context.fresh_name "branch" in
    Branch.v ~fresh:true name >>= fun t ->
    add t >>= fun () ->
    test t >>= fun () ->
    Branch.close t >>= fun () ->
    (* restart in readonly *)
    Branch.v ~fresh:false ~readonly:true name >>= fun t ->
    test t >>= fun () ->
    Branch.close t >>= fun () ->
    (*open two instances and close one*)
    let name = Context.fresh_name "branch" in
    Branch.v ~fresh:true name >>= fun t1 ->
    Branch.v ~fresh:false name >>= fun t2 ->
    add t1 >>= fun () ->
    Branch.close t1 >>= fun () -> test t2

  let tests =
    [
      Alcotest.test_case "branch" `Quick (fun () ->
          Lwt_main.run (test_branch ()));
      Alcotest.test_case "branch close" `Quick (fun () ->
          Lwt_main.run (test_close_branch ()));
    ]
end

let misc =
  ("misc", Dict.tests @ Pack.tests @ Branch.tests @ Multiple_instances.tests)
