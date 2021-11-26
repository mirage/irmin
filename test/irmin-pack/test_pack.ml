(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

(* XXX(craigfe): these tests previously use hashes as addresses. I've changed
   them to use keys instead, which means that they're mostly reading direct offsets
   and not going via the index anymore. TODO: ensure the indexing mechanism is
   still properly tested. *)

module Config = struct
  let entries = 2
  let stable_hash = 3
  let contents_length_header = `Varint
end

let test_dir = Filename.concat "_build" "test-db-pack"

module Schema = struct
  (* TODO: expose Irmin_test.Metadata, or something like it *)
  module Hash = Irmin.Hash.SHA1
  module Commit = Irmin.Commit.Make (Hash)
  module Path = Irmin.Path.String_list
  module Metadata = Irmin.Metadata.None
  module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
  module Branch = Irmin.Branch.String
  module Info = Irmin.Info.Default
  module Contents = Irmin.Contents.String
end

module Irmin_pack_store : Irmin_test.Generic_key = struct
  open Irmin_pack.V2 (Config)

  include Make (struct
    include Schema
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
    module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
    module Commit = Commit_maker.Make (Hash)
  end)
end

module Irmin_pack_layered_store : Irmin_test.Layered_store_generic_key = struct
  open Irmin_pack_layered.Maker (Config)

  include Make (struct
    include Schema
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
    module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
    module Commit = Commit_maker.Make (Hash)
  end)
end

let suite_pack name_suffix indexing_strategy =
  let store = (module Irmin_pack_store : Irmin_test.Generic_key) in
  let _layered_store =
    (* TODO: re-add once the layered store is suppored by Irmin_pack. *)
    (module Irmin_pack_layered_store : Irmin_test.Layered_store_generic_key)
  in
  let config =
    Irmin_pack.config ~fresh:false ~lru_size:0 ~indexing_strategy test_dir
  in
  let init () =
    rm_dir test_dir;
    Lwt.return_unit
  in
  let clean () =
    rm_dir test_dir;
    Lwt.return_unit
  in
  Irmin_test.Suite.create_generic_key ~clear_supported:false
    ~import_supported:false ~name:("PACK" ^ name_suffix) ~init ~store ~config
    ~clean ~layered_store:None ()

module Irmin_pack_mem_maker : Irmin_test.Generic_key = struct
  open Irmin_pack_mem.Maker (Config)

  include Make (struct
    include Schema
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
    module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
    module Commit = Commit_maker.Make (Hash)
  end)
end

let suite_mem =
  let store = (module Irmin_pack_mem_maker : Irmin_test.Generic_key) in
  let config = Irmin_pack.config ~fresh:false ~lru_size:0 test_dir in
  Irmin_test.Suite.create_generic_key ~import_supported:false ~name:"PACK MEM"
    ~store ~config ~layered_store:None ()

let suite =
  let module Index = Irmin_pack.Pack_store.Indexing_strategy in
  [
    suite_pack " { index = always }" Index.always;
    suite_pack " { index = minimal }" Index.minimal;
    suite_mem;
  ]

let check_hash pos msg ~expected actual =
  Alcotest.check_repr ~pos Schema.Hash.t msg expected actual

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

  let ignore_int (_ : int option) = ()

  let test_readonly_dict () =
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
      with Irmin_pack.RO_not_allowed -> ()
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

  let test_clear () =
    let Context.{ dict; _ } = Context.get_dict () in
    ignore_int (Dict.index dict "foo");
    Dict.flush dict;
    Alcotest.(check (option string)) "find foo" (Some "foo") (Dict.find dict 0);
    Dict.clear dict;
    Alcotest.(check (option string))
      "find foo after clear" None (Dict.find dict 0);
    Dict.close dict

  (** Readonly can read old values, after a RW clear but before a sync. Set
      ~force_refill:true for the first sync after a clear. *)
  let readonly_find_old () =
    let Context.{ dict; clone } = Context.get_dict () in
    let r = clone ~readonly:true in
    let check_find msg k i =
      Alcotest.(check (option string)) msg k (Dict.find r i)
    in
    ignore_int (Dict.index dict "foo");
    Dict.flush dict;
    Dict.sync r;
    check_find "find before clear" (Some "foo") 0;
    Dict.clear dict;
    Dict.flush dict;
    check_find "find after clear but before sync" (Some "foo") 0;
    Dict.sync r;
    check_find "find after clear and sync" None 0;
    Dict.close dict;
    Dict.close r

  (** Readonly can read old values, after RW clear and add, but before sync. Set
      ~force_refill:true for the first sync after a clear. *)
  let readonly_find_old_after_rewrite () =
    let Context.{ dict; clone } = Context.get_dict () in
    let r = clone ~readonly:true in
    let check_find msg k i =
      Alcotest.(check (option string)) msg k (Dict.find r i)
    in
    ignore_int (Dict.index dict "foo");
    Dict.flush dict;
    Dict.sync r;
    check_find "find before clear" (Some "foo") 0;
    Dict.clear dict;
    ignore_int (Dict.index dict "bar");
    Dict.flush dict;
    check_find "find after clear and new values added, but before sync"
      (Some "foo") 0;
    Dict.sync r;
    check_find "find new values after sync" (Some "bar") 0;
    Dict.close dict;
    Dict.close r

  let tests =
    [
      Alcotest.test_case "dict" `Quick test_dict;
      Alcotest.test_case "RO dict" `Quick test_readonly_dict;
      Alcotest.test_case "clear" `Quick test_clear;
      Alcotest.test_case "RO find old values after clear" `Quick
        readonly_find_old;
      Alcotest.test_case "RO find old values after clear and add" `Quick
        readonly_find_old_after_rewrite;
    ]
end

module Pack = struct
  let test_pack () =
    let* t = Context.get_pack () in
    let x1 = "foo" in
    let x2 = "bar" in
    let x3 = "otoo" in
    let x4 = "sdadsadas" in
    let h1 = contents_hash x1 in
    let h2 = contents_hash x2 in
    let h3 = contents_hash x3 in
    let h4 = contents_hash x4 in
    let* k1, k2, k3, k4 =
      Pack.batch t.pack (fun w ->
          Lwt_list.map_s
            (fun (k, v) -> Pack.unsafe_add w k v)
            [ (h1, x1); (h2, x2); (h3, x3); (h4, x4) ])
      >|= function
      | [ k1; k2; k3; k4 ] -> (k1, k2, k3, k4)
      | _ -> assert false
    in

    let test t =
      let* y1 = Pack.find t k1 >|= get in
      Alcotest.(check string) "x1" x1 y1;
      let* y3 = Pack.find t k3 >|= get in
      Alcotest.(check string) "x3" x3 y3;
      let* y2 = Pack.find t k2 >|= get in
      Alcotest.(check string) "x2" x2 y2;
      let* y4 = Pack.find t k4 >|= get in
      Alcotest.(check string) "x4" x4 y4;
      Lwt.return_unit
    in
    test t.pack >>= fun () ->
    let* pack2 = t.clone_pack ~readonly:false in
    test pack2 >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Pack.close pack2

  let test_readonly_pack () =
    let* t = Context.get_pack () in
    let* i, r = t.clone_index_pack ~readonly:true in
    let test w =
      let adds l =
        List.map
          (fun (k, v) ->
            Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w k
              v)
          l
      in
      let x1 = "foo" in
      let x2 = "bar" in
      let h1 = contents_hash x1 in
      let h2 = contents_hash x2 in
      let[@warning "-8"] [ _k1; k2 ] = adds [ (h1, x1); (h2, x2) ] in
      let* y2 = Pack.find r k2 in
      Alcotest.(check (option string)) "before sync" None y2;
      Pack.flush w;
      Pack.sync r;
      let* y2 = Pack.find r k2 in
      Alcotest.(check (option string)) "after sync" (Some x2) y2;
      let x3 = "otoo" in
      let x4 = "sdadsadas" in
      let h3 = contents_hash x3 in
      let h4 = contents_hash x4 in
      let[@warning "-8"] [ k3; _k4 ] = adds [ (h3, x3); (h4, x4) ] in
      Pack.flush w;
      Pack.sync r;
      let* y2 = Pack.find r k2 in
      Alcotest.(check (option string)) "y2" (Some x2) y2;
      let* y3 = Pack.find r k3 in
      Alcotest.(check (option string)) "y3" (Some x3) y3;
      Lwt.return_unit
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let test_reuse_index () =
    (* index and pack with different names. However, this behaviour is not exposed by irmin_pack.*)
    let index = Index.v ~log_size:4 ~fresh:true (Context.fresh_name "index") in
    let indexing_strategy = Irmin_pack.Pack_store.Indexing_strategy.always in
    let* w1 =
      Pack.v ~fresh:true ~index ~indexing_strategy (Context.fresh_name "pack")
    in
    let x1 = "foo" in
    let h1 = contents_hash x1 in
    let k1 =
      Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w1 h1 x1
    in
    let* y1 = Pack.find w1 k1 >|= get in
    Alcotest.(check string) "x1" x1 y1;
    Index.close index;
    Pack.close w1

  let test_close_pack_more () =
    (*open and close in rw*)
    let* t = Context.get_pack () in
    let w = t.pack in
    let x1 = "foo" in
    let h1 = contents_hash x1 in
    let k1 =
      Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h1 x1
    in
    Pack.flush w;
    Index.close t.index;
    Pack.close w >>= fun () ->
    (*open and close in ro*)
    let* i1, w1 = t.clone_index_pack ~readonly:true in
    let* y1 = Pack.find w1 k1 >|= get in
    Alcotest.(check string) "x1.1" x1 y1;
    Context.close i1 w1 >>= fun () ->
    (* reopen in rw *)
    let* i2, w2 = t.clone_index_pack ~readonly:false in
    let* y1 = Pack.find w2 k1 >|= get in
    Alcotest.(check string) "x1.2" x1 y1;

    (*reopen in ro *)
    let* i3, w3 = t.clone_index_pack ~readonly:true in
    let* y1 = Pack.find w3 k1 >|= get in
    Alcotest.(check string) "x1.3" x1 y1;
    Context.close i2 w2 >>= fun () -> Context.close i3 w3

  let test_close_pack () =
    let* t = Context.get_pack () in
    let w = t.pack in
    let x1 = "foo" in
    let x2 = "bar" in
    let h1 = contents_hash x1 in
    let h2 = contents_hash x2 in
    let* k1, k2 =
      Pack.batch w (fun w ->
          Lwt_list.map_s
            (fun (k, v) -> Pack.unsafe_add w k v)
            [ (h1, x1); (h2, x2) ])
      >|= function
      | [ k1; k2 ] -> (k1, k2)
      | _ -> assert false
    in

    Context.close t.index w >>= fun () ->
    (*reopen pack and index *)
    let* i, w = t.clone_index_pack ~readonly:false in
    let* y2 = Pack.find w k2 >|= get in
    Alcotest.(check string) "x2.1" x2 y2;
    let* y1 = Pack.find w k1 >|= get in
    Alcotest.(check string) "x1.1" x1 y1;
    (*open and close two packs *)
    let x3 = "toto" in
    let h3 = contents_hash x3 in
    let k3 =
      Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h3 x3
    in
    let* w2 = t.clone_pack ~readonly:false in
    Pack.close w >>= fun () ->
    let* y2 = Pack.find w2 k2 >|= get in
    Alcotest.(check string) "x2.2" x2 y2;
    let* y3 = Pack.find w2 k3 >|= get in
    Alcotest.(check string) "x3.2" x3 y3;
    let* y1 = Pack.find w2 k1 >|= get in
    Alcotest.(check string) "x1.2" x1 y1;
    Context.close i w2 >>= fun () ->
    (*reopen pack and index in readonly *)
    let* i, r = t.clone_index_pack ~readonly:true in
    let* y1 = Pack.find r k1 >|= get in
    Alcotest.(check string) "x1.3" x1 y1;
    let* y2 = Pack.find r k2 >|= get in
    Alcotest.(check string) "x2.3" x2 y2;
    Context.close i r >>= fun () ->
    (*close index while in use*)
    let* i, r = t.clone_index_pack ~readonly:false in
    Index.close i;
    (* [find] after Index [close] is OK, provided we still have the key *)
    let* () = Pack.find r k1 >|= get >|= Alcotest.(check string) "x1.2" x1 in
    Lwt.return_unit

  (** Index can be flushed to disk independently of pack, we simulate this in
      the tests using [Index.filter] and [Index.flush]. Regression test for PR
      1008 in which values were indexed before being reachable in pack. *)
  let readonly_sync_index_flush () =
    let* t = Context.get_pack () in
    let* i, r = t.clone_index_pack ~readonly:true in
    let test w =
      let x1 = "foo" in
      let h1 = contents_hash x1 in
      let k1 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h1 x1
      in
      Pack.sync r;
      let* y1 = Pack.find r k1 in
      Alcotest.(check (option string)) "sync before filter" None y1;
      Index.filter t.index (fun _ -> true);
      Pack.sync r;
      let* y1 = Pack.find r k1 in
      Alcotest.(check (option string)) "sync after filter" (Some x1) y1;
      let x2 = "foo" in
      let h2 = contents_hash x2 in
      let k2 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h2 x2
      in
      Index.flush t.index;
      let+ y2 = Pack.find r k2 in
      Alcotest.(check (option string)) "sync after flush" (Some x2) y2
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let readonly_find_index_flush () =
    let* t = Context.get_pack () in
    let* i, r = t.clone_index_pack ~readonly:true in
    let check h x msg =
      let+ y = Pack.find r h in
      Alcotest.(check (option string)) msg (Some x) y
    in
    let test w =
      let x1 = "foo" in
      let h1 = contents_hash x1 in
      let k1 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h1 x1
      in
      Pack.flush t.pack;
      Pack.sync r;
      check k1 x1 "find before filter" >>= fun () ->
      Index.filter t.index (fun _ -> true);
      check k1 x1 "find after filter" >>= fun () ->
      let x2 = "bar" in
      let h2 = contents_hash x2 in
      let k2 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h2 x2
      in
      Pack.flush t.pack;
      Pack.sync r;
      check k2 x2 "find before flush" >>= fun () ->
      let x3 = "toto" in
      let h3 = contents_hash x3 in
      let k3 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false w h3 x3
      in
      Index.flush t.index;
      check k2 x2 "find after flush" >>= fun () ->
      Pack.flush t.pack;
      Pack.sync r;
      check k3 x3 "find after flush new values"
    in
    test t.pack >>= fun () ->
    Context.close t.index t.pack >>= fun () -> Context.close i r

  let test_clear () =
    let* t = Context.get_pack ~lru_size:10 () in
    let v = "foo" in
    let h = contents_hash v in
    let k =
      Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false t.pack h
        v
    in
    Pack.flush t.pack;
    let* v1 = Pack.find t.pack k in
    Alcotest.(check (option string)) "before clear" (Some v) v1;
    Pack.clear t.pack >>= fun () ->
    let* v2 = Pack.find t.pack k in
    Alcotest.(check (option string)) "after clear" None v2;
    Context.close t.index t.pack

  (** Readonly can read old values, after a RW clear but before a sync. Old
      values can be either in the log or in the data file. [persist] calls
      [Index.flush] to the test for old values in log; it calls [Index.filter]
      to force a merge, to add the values in the data file. *)

  (** TODO: we need to ensure that the pack-file actually indexes here,
      otherwise the test is uesless*)
  let readonly_find_old () =
    let* t = Context.get_pack () in
    let* i, r = t.clone_index_pack ~readonly:true in
    let check h x msg =
      let+ y = Pack.find r h in
      Alcotest.(check (option string)) msg x y
    in
    let x1 = "foo" in
    let h1 = contents_hash x1 in
    let find_before_and_after_sync persist file =
      let k1 =
        Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false t.pack
          h1 x1
      in
      persist ();
      Pack.sync r;
      Pack.clear t.pack >>= fun () ->
      Pack.flush t.pack;
      let* () =
        check k1 (Some x1) ("find in " ^ file ^ " after clear but before sync")
      in
      Pack.sync r;
      check k1 None ("find in " ^ file ^ " after clear and sync")
    in
    let* () =
      find_before_and_after_sync (fun () -> Index.flush t.index) "log"
    in
    let* () =
      find_before_and_after_sync
        (fun () -> Index.filter t.index (fun _ -> true))
        "data"
    in
    Context.close t.index t.pack >>= fun () -> Context.close i r

  (** Similar to the test above, but the read-write pack adds new values after a
      clear, and before a readonly sync. *)
  let readonly_find_old_after_rewrite () =
    let* t = Context.get_pack () in
    let* i, r = t.clone_index_pack ~readonly:true in
    let check h x msg =
      let+ y =
        Pack.index r h >>= function
        | None -> Lwt.return_none
        | Some key -> Pack.find r key
      in
      Alcotest.(check (option string)) msg x y
    in
    let x1 = "foo" in
    let h1 = contents_hash x1 in
    let x2 = "bar" in
    let h2 = contents_hash x2 in
    let find_before_and_after_sync persist file =
      let () =
        let k1 =
          Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false
            t.pack h1 x1
        in
        check_hash __POS__ "Returned key is consistent with pre-computed hash"
          ~expected:h1 (Key.to_hash k1)
      in
      persist ();
      Pack.sync r;
      Pack.clear t.pack >>= fun () ->
      let () =
        let k2 =
          Pack.unsafe_append ~ensure_unique_indexed:true ~overcommit:false
            t.pack h2 x2
        in
        check_hash __POS__ "Returned key is consistent with pre-computed hash"
          ~expected:h2 (Key.to_hash k2)
      in
      persist ();
      let* () =
        check h1 (Some x1)
          ("find old values in " ^ file ^ " after clear but before sync")
      in
      Pack.sync r;
      let* () =
        check h1 None ("do not find old values in " ^ file ^ " after sync")
      in
      check h2 (Some x2) ("find new values in " ^ file ^ " after sync")
    in
    let* () =
      find_before_and_after_sync (fun () -> Index.flush t.index) "log"
    in
    let* () =
      find_before_and_after_sync
        (fun () -> Index.filter t.index (fun _ -> true))
        "data"
    in
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
      Alcotest.test_case "clear" `Quick (fun () -> Lwt_main.run (test_clear ()));
      Alcotest.test_case "readonly find old values after clear" `Quick
        (fun () -> Lwt_main.run (readonly_find_old ()));
      Alcotest.test_case "readonly find old values after clear and add" `Quick
        (fun () -> Lwt_main.run (readonly_find_old_after_rewrite ()));
    ]
end

module Branch = struct
  module Branch =
    Irmin_pack.Atomic_write.Make_persistent
      (Irmin_pack.Version.V2)
      (Irmin.Branch.String)
      (Irmin_pack.Atomic_write.Value.Of_hash (Irmin.Hash.SHA1))

  let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

  let hash =
    Alcotest.testable pp_hash Irmin.Type.(unstage (equal Irmin.Hash.SHA1.t))

  let test_branch () =
    let branches = [ "foo"; "bar/toto"; "titi" ] in
    let test t =
      Lwt_list.iter_s (fun k -> Branch.set t k (sha1 k)) branches >>= fun () ->
      let check h =
        let+ v = Branch.find t h in
        Alcotest.(check (option hash)) h (Some (sha1 h)) v
      in
      Lwt_list.iter_p check branches
    in
    let name = Context.fresh_name "branch" in
    Branch.v ~fresh:true name >>= test >>= fun () ->
    Branch.v ~fresh:true name >>= test >>= fun () ->
    Branch.v ~fresh:true name >>= test >>= fun () ->
    let* t = Branch.v ~fresh:false name in
    test t >>= fun () ->
    let x = sha1 "XXX" in
    Branch.set t "foo" x >>= fun () ->
    let* t = Branch.v ~fresh:false name in
    let* v = Branch.find t "foo" in
    Alcotest.(check (option hash)) "foo" (Some x) v;
    let* br = Branch.list t in
    Alcotest.(check (slist string compare)) "branches" branches br;
    Branch.remove t "foo" >>= fun () ->
    let* t = Branch.v ~fresh:false name in
    let* v = Branch.find t "foo" in
    Alcotest.(check (option hash)) "foo none" None v;
    let* br = Branch.list t in
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
          [%logs.debug "k = %s, v= %a" k pp_hash (sha1 k)];
          Branch.set t k (sha1 k))
        branches
    in
    let test t =
      let check h =
        let+ v = Branch.find t h in
        Alcotest.(check (option hash)) h (Some (sha1 h)) v
      in
      Lwt_list.iter_p check branches
    in
    let name = Context.fresh_name "branch" in
    let* t = Branch.v ~fresh:true name in
    add t >>= fun () ->
    test t >>= fun () ->
    Branch.close t >>= fun () ->
    let* t = Branch.v ~fresh:false ~readonly:true name in
    test t >>= fun () ->
    Branch.close t >>= fun () ->
    let name = Context.fresh_name "branch" in
    let* t1 = Branch.v ~fresh:true name in
    let* t2 = Branch.v ~fresh:false name in
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
  [
    ("dict-files", Dict.tests);
    ("pack-files", Pack.tests);
    ("branch-files", Branch.tests);
    ("instances", Multiple_instances.tests);
    ("existing stores", Test_existing_stores.tests);
    (* NOTE: temporarily disabled as [clear] is no longer supported by [irmin-pack]. *)
    (* ("layers", Layered.tests); *)
    ("inodes", Test_inode.tests);
    ("trees", Test_tree.tests);
  ]
