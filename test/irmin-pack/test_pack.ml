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

open! Import
open Common

let test_dir = Filename.concat "_build" "test-db-pack"

module Irmin_pack_store (Config : Irmin_pack.Conf.S) : Irmin_test.Generic_key =
struct
  open Irmin_pack_unix.Maker (Config)

  include Make (struct
    include Irmin_test.Schema
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
    module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
    module Commit = Commit_maker.Make (Hash)
  end)
end

let suite_pack name_suffix indexing_strategy (module Config : Irmin_pack.Conf.S)
    =
  let store = (module Irmin_pack_store (Config) : Irmin_test.Generic_key) in
  let config =
    Irmin_pack.config ~fresh:false ~lru_size:0 ~indexing_strategy test_dir
  in
  let init ~config =
    let test_dir =
      Irmin.Backend.Conf.find_root config |> Option.value ~default:test_dir
    in
    rm_dir test_dir
  in
  let clean = init in
  Irmin_test.Suite.create_generic_key ~name:("PACK" ^ name_suffix)
    ~import_supported:false ~init ~store ~config ~clean ()

module Irmin_tezos_conf = struct
  include Irmin_tezos.Conf

  (* The generic test suite relies a lot on the empty tree. Let's allow it. *)
  let forbid_empty_dir_persistence = false
end

module Irmin_pack_mem_maker : Irmin_test.Generic_key = struct
  open Irmin_pack_mem.Maker (Irmin_tezos_conf)

  include Make (struct
    include Irmin_test.Schema
    module Node = Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata)
    module Commit_maker = Irmin.Commit.Generic_key.Maker (Info)
    module Commit = Commit_maker.Make (Hash)
  end)
end

let suite_mem =
  let store = (module Irmin_pack_mem_maker : Irmin_test.Generic_key) in
  let config = Irmin_pack.config ~fresh:false ~lru_size:0 test_dir in
  Irmin_test.Suite.create_generic_key ~import_supported:false ~name:"PACK MEM"
    ~store ~config ()

let suite =
  let module Index = Irmin_pack.Indexing_strategy in
  let module Conf_small_nodes = struct
    (* Parameters chosen to be different from those in [Irmin_tezos.Conf]: *)
    let entries = 2
    let stable_hash = 3
    let contents_length_header = None
    let inode_child_order = `Hash_bits
    let forbid_empty_dir_persistence = false
  end in
  [
    suite_pack " { Tezos }" Index.minimal (module Irmin_tezos_conf);
    suite_pack " { Small_nodes }" Index.always (module Conf_small_nodes);
    suite_mem;
  ]

module Context = Make_context (struct
  let root = test_dir
end)

let flush fm = File_manager.flush fm |> Errs.raise_if_error
let reload fm = File_manager.reload fm |> Errs.raise_if_error

module Dict = struct
  let test_dict () =
    let (d : Context.d) = Context.get_dict ~readonly:false ~fresh:true () in
    let x1 = Dict.index d.dict "foo" in
    Alcotest.(check (option int)) "foo" (Some 0) x1;
    let x1 = Dict.index d.dict "foo" in
    Alcotest.(check (option int)) "foo" (Some 0) x1;
    let x2 = Dict.index d.dict "bar" in
    Alcotest.(check (option int)) "bar" (Some 1) x2;
    let x3 = Dict.index d.dict "toto" in
    Alcotest.(check (option int)) "toto" (Some 2) x3;
    let x4 = Dict.index d.dict "titiabc" in
    Alcotest.(check (option int)) "titiabc" (Some 3) x4;
    let x1 = Dict.index d.dict "foo" in
    Alcotest.(check (option int)) "foo" (Some 0) x1;
    flush d.fm;
    let (d2 : Context.d) =
      Context.get_dict ~name:d.name ~readonly:false ~fresh:false ()
    in
    let x4 = Dict.index d2.dict "titiabc" in
    Alcotest.(check (option int)) "titiabc" (Some 3) x4;
    let v1 = Dict.find d2.dict (get x1) in
    Alcotest.(check (option string)) "find x1" (Some "foo") v1;
    let v2 = Dict.find d2.dict (get x2) in
    Alcotest.(check (option string)) "find x2" (Some "bar") v2;
    let v3 = Dict.find d2.dict (get x3) in
    Alcotest.(check (option string)) "find x3" (Some "toto") v3;
    Context.close_dict d;
    let (d3 : Context.d) =
      Context.get_dict ~name:d.name ~readonly:false ~fresh:false ()
    in
    let v1 = Dict.find d3.dict (get x1) in
    Alcotest.(check (option string)) "find x1" (Some "foo") v1;
    Context.close_dict d2;
    Context.close_dict d3

  let ignore_int (_ : int option) = ()

  let test_readonly_dict () =
    let (d : Context.d) = Context.get_dict ~readonly:false ~fresh:true () in
    let (d2 : Context.d) =
      Context.get_dict ~name:d.name ~readonly:true ~fresh:false ()
    in
    let check_index k i =
      Alcotest.(check (option int)) k (Some i) (Dict.index d2.dict k)
    in
    let check_find k i =
      Alcotest.(check (option string)) k (Some k) (Dict.find d2.dict i)
    in
    let check_none k i =
      Alcotest.(check (option string)) k None (Dict.find d2.dict i)
    in
    let check_raise k =
      try
        ignore_int (Dict.index d2.dict k);
        Alcotest.fail "RO dict should not be writable"
      with Irmin_pack.RO_not_allowed -> ()
    in
    ignore_int (Dict.index d.dict "foo");
    ignore_int (Dict.index d.dict "foo");
    ignore_int (Dict.index d.dict "bar");
    ignore_int (Dict.index d.dict "toto");
    ignore_int (Dict.index d.dict "titiabc");
    ignore_int (Dict.index d.dict "foo");
    flush d.fm;
    reload d2.fm;
    check_index "titiabc" 3;
    check_index "bar" 1;
    check_index "toto" 2;
    check_find "foo" 0;
    check_raise "xxx";
    ignore_int (Dict.index d.dict "hello");
    check_raise "hello";
    check_none "hello" 4;
    flush d.fm;
    reload d2.fm;
    check_find "hello" 4;
    Context.close_dict d;
    Context.close_dict d2

  let tests =
    [
      Alcotest.test_case "dict" `Quick test_dict;
      Alcotest.test_case "RO dict" `Quick test_readonly_dict;
    ]
end

module Pack = struct
  let test_pack () =
    let t = Context.get_rw_pack () in
    let x1 = "foo" in
    let x2 = "bar" in
    let x3 = "otoo" in
    let x4 = "sdadsadas" in
    let h1 = sha1_contents x1 in
    let h2 = sha1_contents x2 in
    let h3 = sha1_contents x3 in
    let h4 = sha1_contents x4 in
    let k1, k2, k3, k4 =
      Pack.batch t.Context.pack (fun w ->
          List.map
            (fun (k, v) -> Pack.unsafe_add w k v)
            [ (h1, x1); (h2, x2); (h3, x3); (h4, x4) ])
      |> function
      | [ k1; k2; k3; k4 ] -> (k1, k2, k3, k4)
      | _ -> assert false
    in

    let test t =
      let y1 = Pack.find t k1 |> get in
      Alcotest.(check string) "x1" x1 y1;
      let y3 = Pack.find t k3 |> get in
      Alcotest.(check string) "x3" x3 y3;
      let y2 = Pack.find t k2 |> get in
      Alcotest.(check string) "x2" x2 y2;
      let y4 = Pack.find t k4 |> get in
      Alcotest.(check string) "x4" x4 y4
    in
    test t.pack;
    let t' = Context.get_ro_pack t.name in
    test t'.pack;
    Context.close_pack t;
    Context.close_pack t'

  let test_readonly_pack () =
    let t = Context.get_rw_pack () in
    let t' = Context.get_ro_pack t.name in
    let () =
      let adds l =
        List.map
          (fun (k, v) ->
            Pack.unsafe_append ~ensure_unique:true ~overcommit:false t.pack k v)
          l
      in
      let x1 = "foo" in
      let x2 = "bar" in
      let h1 = sha1_contents x1 in
      let h2 = sha1_contents x2 in
      let[@warning "-8"] [ _k1; k2 ] = adds [ (h1, x1); (h2, x2) ] in
      let y2 = Pack.find t'.pack k2 in
      Alcotest.(check (option string)) "before reload" None y2;
      flush t.fm;
      reload t'.fm;
      let y2 = Pack.find t'.pack k2 in
      Alcotest.(check (option string)) "after reload" (Some x2) y2;
      let x3 = "otoo" in
      let x4 = "sdadsadas" in
      let h3 = sha1_contents x3 in
      let h4 = sha1_contents x4 in
      let[@warning "-8"] [ k3; _k4 ] = adds [ (h3, x3); (h4, x4) ] in
      flush t.fm;
      reload t'.fm;
      let y2 = Pack.find t'.pack k2 in
      Alcotest.(check (option string)) "y2" (Some x2) y2;
      let y3 = Pack.find t'.pack k3 in
      Alcotest.(check (option string)) "y3" (Some x3) y3
    in
    Context.close_pack t;
    Context.close_pack t'

  let test_close_pack_more () =
    (*open and close in rw*)
    let t = Context.get_rw_pack () in
    let x1 = "foo" in
    let h1 = sha1_contents x1 in
    let k1 =
      Pack.unsafe_append ~ensure_unique:true ~overcommit:false t.pack h1 x1
    in
    flush t.fm;
    Context.close_pack t;
    (*open and close in ro*)
    let t1 = Context.get_ro_pack t.name in
    let y1 = Pack.find t1.pack k1 |> get in
    Alcotest.(check string) "x1.1" x1 y1;
    Context.close_pack t1;
    (* reopen in rw *)
    let t2 = Context.reopen_rw t.name in
    let y1 = Pack.find t2.pack k1 |> get in
    Alcotest.(check string) "x1.2" x1 y1;
    (*reopen in ro *)
    let t3 = Context.get_ro_pack t.name in
    let y1 = Pack.find t3.pack k1 |> get in
    Alcotest.(check string) "x1.3" x1 y1;
    Context.close_pack t2;
    Context.close_pack t3

  let test_close_pack () =
    let t = Context.get_rw_pack () in
    let w = t.pack in
    let x1 = "foo" in
    let x2 = "bar" in
    let h1 = sha1_contents x1 in
    let h2 = sha1_contents x2 in
    let k1, k2 =
      Pack.batch w (fun w ->
          List.map (fun (k, v) -> Pack.unsafe_add w k v) [ (h1, x1); (h2, x2) ])
      |> function
      | [ k1; k2 ] -> (k1, k2)
      | _ -> assert false
    in
    Context.close_pack t;
    (*reopen in rw *)
    let t' = Context.reopen_rw t.name in
    let y2 = Pack.find t'.pack k2 |> get in
    Alcotest.(check string) "x2.1" x2 y2;
    let y1 = Pack.find t'.pack k1 |> get in
    Alcotest.(check string) "x1.1" x1 y1;
    let x3 = "toto" in
    let h3 = sha1_contents x3 in
    let k3 =
      Pack.unsafe_append ~ensure_unique:true ~overcommit:false t'.pack h3 x3
    in
    Context.close_pack t';
    (*reopen in rw *)
    let t2 = Context.reopen_rw t.name in
    let y2 = Pack.find t2.pack k2 |> get in
    Alcotest.(check string) "x2.2" x2 y2;
    let y3 = Pack.find t2.pack k3 |> get in
    Alcotest.(check string) "x3.2" x3 y3;
    let y1 = Pack.find t2.pack k1 |> get in
    Alcotest.(check string) "x1.2" x1 y1;
    Context.close_pack t2;
    (*reopen in ro *)
    let t' = Context.get_ro_pack t.name in
    let y1 = Pack.find t'.pack k1 |> get in
    Alcotest.(check string) "x1.3" x1 y1;
    let y2 = Pack.find t'.pack k2 |> get in
    Alcotest.(check string) "x2.3" x2 y2;
    Context.close_pack t'

  (** Index can be flushed to disk independently of pack, we simulate this in
      the tests using [Index.filter] and [Index.flush]. Regression test for PR
      1008 in which values were indexed before being reachable in pack. *)
  let readonly_reload_index_flush () =
    let t = Context.get_rw_pack () in
    let t' = Context.get_ro_pack t.name in
    let test w =
      let x1 = "foo" in
      let h1 = sha1_contents x1 in
      let k1 =
        Pack.unsafe_append ~ensure_unique:true ~overcommit:false w h1 x1
      in
      reload t'.fm;
      let y1 = Pack.find t'.pack k1 in
      Alcotest.(check (option string)) "reload before filter" None y1;
      Index.filter t.index (fun _ -> true);
      reload t'.fm;
      let y1 = Pack.find t'.pack k1 in
      Alcotest.(check (option string)) "reload after filter" (Some x1) y1;
      let x2 = "foo" in
      let h2 = sha1_contents x2 in
      let k2 =
        Pack.unsafe_append ~ensure_unique:true ~overcommit:false w h2 x2
      in
      Index.flush t.index ~with_fsync:false |> Errs.raise_if_error;
      let y2 = Pack.find t'.pack k2 in
      Alcotest.(check (option string)) "reload after flush" (Some x2) y2
    in
    test t.pack;
    Context.close_pack t;
    Context.close_pack t'

  let readonly_find_index_flush () =
    let t = Context.get_rw_pack () in
    let t' = Context.get_ro_pack t.name in
    let check h x msg =
      let y = Pack.find t'.pack h in
      Alcotest.(check (option string)) msg (Some x) y
    in
    let test w =
      let x1 = "foo" in
      let h1 = sha1_contents x1 in
      let k1 =
        Pack.unsafe_append ~ensure_unique:true ~overcommit:false w h1 x1
      in
      flush t.fm;
      reload t'.fm;
      check k1 x1 "find before filter";
      Index.filter t.index (fun _ -> true);
      check k1 x1 "find after filter";
      let x2 = "bar" in
      let h2 = sha1_contents x2 in
      let k2 =
        Pack.unsafe_append ~ensure_unique:true ~overcommit:false w h2 x2
      in
      flush t.fm;
      reload t'.fm;
      check k2 x2 "find before flush";
      let x3 = "toto" in
      let h3 = sha1_contents x3 in
      let k3 =
        Pack.unsafe_append ~ensure_unique:true ~overcommit:false w h3 x3
      in
      Index.flush t.index ~with_fsync:false |> Errs.raise_if_error;
      check k2 x2 "find after flush";
      flush t.fm;
      reload t'.fm;
      check k3 x3 "find after flush new values"
    in
    test t.pack;
    Context.close_pack t;
    Context.close_pack t'

  let tests =
    [
      Alcotest.test_case "pack" `Quick test_pack;
      Alcotest.test_case "RO pack" `Quick test_readonly_pack;
      Alcotest.test_case "close" `Quick test_close_pack;
      Alcotest.test_case "close readonly" `Quick test_close_pack_more;
      Alcotest.test_case "readonly reload, index flush" `Quick
        readonly_reload_index_flush;
      Alcotest.test_case "readonly find, index flush" `Quick
        readonly_find_index_flush;
    ]
end

module Branch = struct
  module Branch =
    Irmin_pack_unix.Atomic_write.Make_persistent
      (Irmin.Branch.String)
      (Irmin_pack.Atomic_write.Value.Of_hash (Irmin.Hash.SHA1))

  let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

  let test_branch () =
    let branches = [ "foo"; "bar/toto"; "titi" ] in
    let test t =
      List.iter (fun k -> Branch.set t k (sha1 k)) branches;
      let check h () =
        let v = Branch.find t h in
        Alcotest.(check (option hash)) h (Some (sha1 h)) v
      in
      List.map check branches |> Eio.Fiber.all
    in
    let name = Context.fresh_name "branch" in
    Branch.v ~fresh:true name |> test;
    Branch.v ~fresh:true name |> test;
    Branch.v ~fresh:true name |> test;
    let t = Branch.v ~fresh:false name in
    test t;
    let x = sha1 "XXX" in
    Branch.set t "foo" x;
    let t = Branch.v ~fresh:false name in
    let v = Branch.find t "foo" in
    Alcotest.(check (option hash)) "foo" (Some x) v;
    let br = Branch.list t in
    Alcotest.(check (slist string compare)) "branches" branches br;
    Branch.remove t "foo";
    let t = Branch.v ~fresh:false name in
    let v = Branch.find t "foo" in
    Alcotest.(check (option hash)) "foo none" None v;
    let br = Branch.list t in
    Alcotest.(check (slist string compare))
      "branches"
      (List.filter (( <> ) "foo") branches)
      br

  let test_close_branch () =
    let branches = [ "foo"; "bar/toto"; "titi" ] in
    let add t =
      List.iter
        (fun k ->
          [%logs.debug "k = %s, v= %a" k pp_hash (sha1 k)];
          Branch.set t k (sha1 k))
        branches
    in
    let test t =
      let check h () =
        let v = Branch.find t h in
        Alcotest.(check (option hash)) h (Some (sha1 h)) v
      in
      List.map check branches |> Eio.Fiber.all
    in
    let name = Context.fresh_name "branch" in
    let t = Branch.v ~fresh:true name in
    add t;
    test t;
    Branch.close t;
    let t = Branch.v ~fresh:false ~readonly:true name in
    test t;
    Branch.close t;
    let name = Context.fresh_name "branch" in
    let t1 = Branch.v ~fresh:true ~readonly:false name in
    let t2 = Branch.v ~fresh:false ~readonly:true name in
    add t1;
    Branch.close t1;
    test t2

  let tests =
    [
      Alcotest.test_case "branch" `Quick test_branch;
      Alcotest.test_case "branch close" `Quick test_close_branch;
    ]
end

module Layout = struct
  let test_classify_upper_filename () =
    let module V1_and_v2 = Irmin_pack.Layout.V1_and_v2 in
    let module V4 = Irmin_pack.Layout.V4 in
    let module Classification = Irmin_pack.Layout.Classification.Upper in
    let c = Alcotest.(check (testable_repr Classification.t)) "" in
    let classif = Classification.v in
    c `V1_or_v2_pack (V1_and_v2.pack ~root:"" |> classif);
    c `Branch (V4.branch ~root:"" |> classif);
    c `Control (V4.control ~root:"" |> classif);
    c `Control_tmp (V4.control_tmp ~root:"" |> classif);
    c `Dict (V4.dict ~root:"" |> classif);
    c (`Gc_result 0) (V4.gc_result ~generation:0 ~root:"" |> classif);
    c (`Reachable 1) (V4.reachable ~generation:1 ~root:"" |> classif);
    c (`Sorted 10) (V4.sorted ~generation:10 ~root:"" |> classif);
    c (`Mapping 100) (V4.mapping ~generation:100 ~root:"" |> classif);
    c (`Prefix 1000) (V4.prefix ~generation:1000 ~root:"" |> classif);
    c (`Suffix 42) (V4.suffix_chunk ~chunk_idx:42 ~root:"" |> classif);
    c `Unknown (V4.prefix ~generation:(-1) ~root:"" |> classif);
    c `Unknown (classif "store.toto");
    c `Unknown (classif "store.");
    c `Unknown (classif "store");
    c `Unknown (classif "store.00.prefix");
    c `Unknown (classif "store.01.prefix");
    c `Unknown (classif "./store.0.prefix");
    ()

  let test_classify_volume_filename () =
    let module V1_and_v2 = Irmin_pack.Layout.V1_and_v2 in
    let module V5 = Irmin_pack.Layout.V5.Volume in
    let module Classification = Irmin_pack.Layout.Classification.Volume in
    let c = Alcotest.(check (testable_repr Classification.t)) "" in
    let classif = Classification.v in
    c `Control (V5.control ~root:"" |> classif);
    c `Mapping (V5.mapping ~root:"" |> classif);
    c `Data (V5.data ~root:"" |> classif);
    c `Unknown (classif "store.toto");
    c `Unknown (classif "store.");
    c `Unknown (classif "store");
    c `Unknown (classif "store.00.prefix");
    c `Unknown (classif "store.01.prefix");
    c `Unknown (classif "./store.0.prefix");
    ()

  let tests =
    [
      Alcotest.test_case "classify upper files" `Quick
        test_classify_upper_filename;
      Alcotest.test_case "classify volume files" `Quick
        test_classify_volume_filename;
    ]
end

let misc d_mgr =
  [
    ("hashes", Test_hashes.tests);
    ("dict-files", Dict.tests);
    ("pack-files", Pack.tests);
    ("branch-files", Branch.tests);
    ("read-only", Test_readonly.tests);
    ("existing stores", Test_existing_stores.tests);
    ("inodes", Test_inode.tests);
    ("trees", Test_tree.tests);
    ("version-bump", Test_pack_version_bump.tests);
    ("snapshot", Test_snapshot.tests);
    ("upgrade", Test_upgrade.tests);
    ("gc", Test_gc.Gc.tests);
    ("concurrent gc", Test_gc.Concurrent_gc.tests);
    ("gc archival", Test_gc.Gc_archival.tests);
    ("split", Test_gc.Split.tests);
    ("flush", Test_flush_reload.tests);
    ("ranges", Test_ranges.tests);
    ("mapping", Test_mapping.tests);
    ("test_nearest_geq", Test_nearest_geq.tests);
    ("layout", Layout.tests);
    ("dispatcher", Test_dispatcher.tests);
    ("corrupted", Test_corrupted.tests);
    ("snapshot_gc", Test_gc.Snapshot.tests);
    ("async tasks", Test_async.tests);
    ("indexing strategy", Test_indexing_strategy.tests);
    ("lower: direct", Test_lower.Direct.tests);
    ("lower: store", Test_lower.Store.tests);
    ("multicore", Test_multicore.tests d_mgr);
  ]
