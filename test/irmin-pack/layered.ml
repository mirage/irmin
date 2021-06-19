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

let src = Logs.Src.create "test" ~doc:"Irmin-pack layered tests"

module Log = (val Logs.src_log src : Logs.LOG)

let root = "_build"

let fresh_name =
  let c = ref 0 in
  fun () ->
    incr c;
    let name = Filename.concat root ("layered_" ^ string_of_int !c) in
    Logs.info (fun m -> m "Constructing irmin store %s" name);
    name

let index_log_size = Some 4

module Conf = struct
  let entries = 32
  let stable_hash = 256
  let lower_root = "_lower"
  let upper0_root = "0"
  let with_lower = true
end

module Store = struct
  open Irmin_pack_layered.Maker (Conf)
  include Make (Schema)
end

module V2 = Irmin_pack.V2 (Conf)
module StoreSimple = V2.Make (Schema)

let config ?(readonly = false) ?(fresh = true) ?(lower_root = Conf.lower_root)
    ?(upper_root0 = Conf.upper0_root) ?(with_lower = Conf.with_lower) root =
  let conf = Irmin_pack.config ~readonly ?index_log_size ~fresh root in
  Irmin_pack_layered.config ~lower_root ~upper_root0 ~with_lower conf

module Test = struct
  type index = { root : string; repo : Store.Repo.t }

  and context = {
    index : index;
    parents : Store.Commit.t list;
    tree : Store.tree;
  }

  let info = Store.Info.empty

  let commit ctxt =
    let parents = List.map Store.Commit.hash ctxt.parents in
    let+ h = Store.Commit.v ctxt.index.repo ~info ~parents ctxt.tree in
    Store.Tree.clear ctxt.tree;
    h

  let set ctxt key data =
    let* tree = Store.Tree.add ctxt.tree key data in
    Lwt.return { ctxt with tree }

  let del ctxt key =
    let* tree = Store.Tree.remove ctxt.tree key in
    Lwt.return { ctxt with tree }

  let checkout index key =
    Store.Commit.of_hash index.repo (Store.Commit.hash key) >>= function
    | None -> Lwt.return_none
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let ctxt = { index; tree; parents = [ commit ] } in
        Lwt.return_some ctxt

  let init ?readonly ?with_lower () =
    let root = fresh_name () in
    Common.rm_dir root;
    let+ repo = Store.Repo.v (config ?readonly ?with_lower root) in
    let index = { root; repo } in
    let tree = Store.Tree.empty in
    { index; tree; parents = [] }

  let clone ?readonly ?with_lower root =
    let+ repo = Store.Repo.v (config ~fresh:false ?readonly ?with_lower root) in
    let index = { root; repo } in
    let tree = Store.Tree.empty in
    { index; tree; parents = [] }

  let freeze ?copy_in_upper ctxt commit =
    let max_upper =
      match copy_in_upper with
      | Some false -> Some []
      | None | Some true -> None
    in
    Store.freeze ?max_upper ctxt.index.repo ~max_lower:[ commit ] >|= fun () ->
    { index = ctxt.index; tree = Store.Tree.empty; parents = [] }

  let commit_block1 ctxt =
    let* ctxt = set ctxt [ "a"; "b" ] "Novembre" in
    let* ctxt = set ctxt [ "a"; "c" ] "Juin" in
    let* ctxt = set ctxt [ "version" ] "0.0" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block1 repo block1 =
    Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* novembre = Store.Tree.find tree [ "a"; "b" ] in
        Alcotest.(check (option string)) "nov" (Some "Novembre") novembre;
        let* juin = Store.Tree.find tree [ "a"; "c" ] in
        Alcotest.(check (option string)) "juin" (Some "Juin") juin;
        let+ version = Store.Tree.find tree [ "version" ] in
        Alcotest.(check (option string)) "version" (Some "0.0") version

  let commit_block1a ctxt =
    let* ctxt = del ctxt [ "a"; "b" ] in
    let* ctxt = set ctxt [ "a"; "d" ] "Mars" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block1a repo block1a =
    Store.Commit.of_hash repo (Store.Commit.hash block1a) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* mars = Store.Tree.find tree [ "a"; "d" ] in
        Alcotest.(check (option string)) "mars" (Some "Mars") mars;
        let* juin = Store.Tree.find tree [ "a"; "c" ] in
        Alcotest.(check (option string)) "juin" (Some "Juin") juin;
        let+ version = Store.Tree.find tree [ "version" ] in
        Alcotest.(check (option string)) "version" (Some "0.0") version

  let commit_block1b ctxt =
    let* ctxt = del ctxt [ "a"; "c" ] in
    let* ctxt = set ctxt [ "a"; "d" ] "F\195\169vrier" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block1b repo block1 =
    Store.Commit.of_hash repo (Store.Commit.hash block1) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let+ fev = Store.Tree.find tree [ "a"; "d" ] in
        Alcotest.(check (option string)) "fev" (Some "F\195\169vrier") fev

  let commit_block1c ctxt =
    let* ctxt = del ctxt [ "a"; "c" ] in
    let* ctxt = set ctxt [ "a"; "d" ] "Janv" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block1c repo block =
    Store.Commit.of_hash repo (Store.Commit.hash block) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let+ janv = Store.Tree.find tree [ "a"; "d" ] in
        Alcotest.(check (option string)) "janv" (Some "Janv") janv

  let commit_block2a ctxt =
    let* ctxt = set ctxt [ "a"; "e" ] "Mars" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block2a repo block1a =
    Store.Commit.of_hash repo (Store.Commit.hash block1a) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let+ mars = Store.Tree.find tree [ "a"; "e" ] in
        Alcotest.(check (option string)) "mars" (Some "Mars") mars

  let commit_block3a ctxt =
    let* ctxt = set ctxt [ "e"; "a" ] "Avril" in
    let+ h = commit ctxt in
    (ctxt, h)

  let check_block3a repo block =
    Store.Commit.of_hash repo (Store.Commit.hash block) >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* mars = Store.Tree.find tree [ "a"; "e" ] in
        Alcotest.(check (option string)) "mars" (Some "Mars") mars;
        let* avril = Store.Tree.find tree [ "e"; "a" ] in
        Alcotest.(check (option string)) "avril" (Some "Avril") avril;
        let* mars = Store.Tree.find tree [ "a"; "d" ] in
        Alcotest.(check (option string)) "mars" (Some "Mars") mars;
        let* juin = Store.Tree.find tree [ "a"; "c" ] in
        Alcotest.(check (option string)) "juin" (Some "Juin") juin;
        let+ version = Store.Tree.find tree [ "version" ] in
        Alcotest.(check (option string)) "version" (Some "0.0") version

  let checkout_and_commit ctxt block create =
    checkout ctxt.index block >>= function
    | None -> failwith "checkout block"
    | Some ctxt -> create ctxt

  let check_removed ctxt key msg =
    Store.Commit.of_hash ctxt.index.repo (Store.Commit.hash key) >>= function
    | None -> Lwt.return_unit
    | Some _ -> Alcotest.failf "should not find %s" msg

  let commit_block1_simple_store repo =
    let* tree =
      StoreSimple.Tree.add StoreSimple.Tree.empty [ "a"; "b" ] "Novembre"
    in
    let* tree = StoreSimple.Tree.add tree [ "a"; "c" ] "Juin" in
    let* tree = StoreSimple.Tree.add tree [ "version" ] "0.0" in
    let+ block1 =
      StoreSimple.Commit.v repo ~info:StoreSimple.Info.empty ~parents:[] tree
    in
    StoreSimple.Commit.hash block1

  let check_commit_hash ctxt check_block hash =
    Store.Commit.of_hash ctxt.index.repo hash >>= function
    | None -> Alcotest.fail "no hash found in repo"
    | Some commit -> check_block ctxt.index.repo commit

  (** Check that freeze preserves and deletes commits accordingly. *)
  let test_simple_freeze () =
    let* ctxt = init () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt, block1b = checkout_and_commit ctxt block1 commit_block1b in
    let* ctxt = freeze ctxt block1a in
    check_block1 ctxt.index.repo block1 >>= fun () ->
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_removed ctxt block1b "block1b" >>= fun () ->
    check_block1a ctxt.index.repo block1a >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (** Check that freeze and close work together; check that the right upper is
      used after freeze and after closing and reopening the store. *)
  let test_close_freeze () =
    let check_upper msg expected repo =
      let x = Store.Private_layer.upper_in_use repo in
      if expected <> x then Alcotest.fail msg
    in
    let* ctxt = init () in
    let store_name = ctxt.index.root in
    check_upper "upper1.1" `Upper1 ctxt.index.repo;
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt = freeze ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_upper "upper0.1" `Upper0 ctxt.index.repo;
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* ctxt = clone ~readonly:false store_name in
    check_upper "upper0.2" `Upper0 ctxt.index.repo;
    check_block1 ctxt.index.repo block1 >>= fun () ->
    check_block1a ctxt.index.repo block1a >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* ctxt = clone ~readonly:false store_name in
    check_upper "upper0.3" `Upper0 ctxt.index.repo;
    let* ctxt = freeze ctxt block1a in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* ctxt = clone ~readonly:false store_name in
    check_upper "upper1.2" `Upper1 ctxt.index.repo;
    Store.Repo.close ctxt.index.repo

  (** Check that freeze preserves and deletes commits from RO. *)
  let test_ro_simple () =
    let* ctxt = init () in
    let* ro_ctxt = clone ~readonly:true ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt, block1b = checkout_and_commit ctxt block1 commit_block1b in
    Store.sync ro_ctxt.index.repo;
    Log.debug (fun l -> l "Freeze removes block1b but keeps block1, block1a");
    let* ctxt = freeze ctxt block1a in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    check_block1a ro_ctxt.index.repo block1a >>= fun () ->
    check_block1b ro_ctxt.index.repo block1b >>= fun () ->
    Store.sync ro_ctxt.index.repo;
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    check_block1a ro_ctxt.index.repo block1a >>= fun () ->
    check_removed ro_ctxt block1b "block1b" >>= fun () ->
    let* ctxt, block2a = checkout_and_commit ctxt block1a commit_block2a in
    let* ctxt, block1c = checkout_and_commit ctxt block1a commit_block1c in
    Store.sync ro_ctxt.index.repo;
    Log.debug (fun l -> l "Freeze removes block1c but keeps block2a");
    let* ctxt = freeze ctxt block2a in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_block2a ro_ctxt.index.repo block2a >>= fun () ->
    check_block1c ro_ctxt.index.repo block1c >>= fun () ->
    Store.sync ro_ctxt.index.repo;
    check_block2a ro_ctxt.index.repo block2a >>= fun () ->
    check_removed ro_ctxt block1c "block1c" >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  (** When RO calls sync the value is in upper1, but when RO calls find the
      value moved to lower. *)
  let test_ro_sync_after_two_freezes () =
    let* ctxt = init () in
    let* ro_ctxt = clone ~readonly:true ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    Store.sync ro_ctxt.index.repo;
    let* ctxt = freeze ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt = freeze ctxt block1a in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  (** Check opening RO store and calling sync right after. *)
  let test_ro_sync_after_v () =
    let* ctxt = init () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ro_ctxt = clone ~readonly:true ctxt.index.root in
    Store.sync ro_ctxt.index.repo;
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  (** Delete lower or the upper not in use and add, find and freeze. *)
  let test_delete_stores () =
    let rm_store_from_disk store_name root =
      let name = Filename.concat store_name root in
      let cmd = Printf.sprintf "rm -rf %s" name in
      Fmt.epr "exec: %s\n%!" cmd;
      let _ = Sys.command cmd in
      ()
    in
    let* ctxt = init () in
    let store_name = ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    rm_store_from_disk store_name Conf.lower_root;
    rm_store_from_disk store_name Conf.upper0_root;
    let* ctxt = clone ~readonly:false store_name in
    check_block1 ctxt.index.repo block1 >>= fun () ->
    let* ctxt = freeze ctxt block1 in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    rm_store_from_disk store_name Conf.lower_root;
    rm_store_from_disk store_name "upper1";
    let* ctxt = clone ~readonly:false store_name in
    check_block1 ctxt.index.repo block1 >>= fun () ->
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt = freeze ctxt block1a in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    rm_store_from_disk store_name Conf.lower_root;
    rm_store_from_disk store_name Conf.upper0_root;
    let* ctxt = clone ~readonly:false store_name in
    check_block1a ctxt.index.repo block1a >>= fun () ->
    check_removed ctxt block1 "block1" >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (** Open layered store, close it and open only the lower layer. *)
  let test_open_lower () =
    let* ctxt = init () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt = freeze ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    let hash1 = Store.Commit.hash block1 in
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* repo =
      StoreSimple.Repo.v
        (config ~fresh:false ~readonly:true
           (Filename.concat ctxt.index.root Conf.lower_root))
    in
    let* () =
      StoreSimple.Commit.of_hash repo hash1 >>= function
      | None -> Alcotest.fail "checkout block1"
      | Some commit ->
          let tree = StoreSimple.Commit.tree commit in
          let* version = StoreSimple.Tree.find tree [ "version" ] in
          Alcotest.(check (option string)) "version" version (Some "0.0");
          Lwt.return_unit
    in
    StoreSimple.Repo.close repo

  (** Open upper1 as simple store, close it and then open it as layered store. *)
  let test_upper1_reopen () =
    let store_name = fresh_name () in
    Common.rm_dir store_name;
    let* repo =
      StoreSimple.Repo.v
        (config ~fresh:true ~readonly:false
           (Filename.concat store_name "upper1"))
    in
    let* hash1 = commit_block1_simple_store repo in
    StoreSimple.Repo.close repo >>= fun () ->
    let* ctxt = clone ~readonly:false store_name in
    check_commit_hash ctxt check_block1 hash1 >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (** Open lower as simple store, close it and then open it as layered store. *)
  let test_lower_reopen () =
    let store_name = fresh_name () in
    Common.rm_dir store_name;
    let* repo =
      StoreSimple.Repo.v
        (config ~fresh:true ~readonly:false
           (Filename.concat store_name Conf.lower_root))
    in
    let* hash1 = commit_block1_simple_store repo in
    StoreSimple.Repo.close repo >>= fun () ->
    let* ctxt = clone ~readonly:false store_name in
    check_commit_hash ctxt check_block1 hash1 >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (** Test the without lower option for both RW and RO instances. *)
  let test_without_lower () =
    let* ctxt = init ~with_lower:false () in
    let* ro_ctxt = clone ~readonly:true ~with_lower:false ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    Log.debug (fun l -> l "Freeze removes block1 from upper");
    let* ctxt = freeze ~copy_in_upper:false ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_removed ctxt block1 "block1" >>= fun () ->
    Store.sync ro_ctxt.index.repo;
    check_removed ro_ctxt block1 "block1" >>= fun () ->
    let* ctxt, block1 = commit_block1 ctxt in
    Log.debug (fun l -> l "Freeze keeps block1 in upper");
    let* ctxt = freeze ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_block1 ctxt.index.repo block1 >>= fun () ->
    Store.sync ro_ctxt.index.repo;
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    Log.debug (fun l -> l "Freeze removes block1, block1a from upper");
    let* ctxt = freeze ~copy_in_upper:false ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_removed ctxt block1 "block1" >>= fun () ->
    check_removed ctxt block1a "block1a" >>= fun () ->
    Store.sync ro_ctxt.index.repo;
    check_removed ro_ctxt block1 "block1" >>= fun () ->
    check_removed ro_ctxt block1a "block1a" >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  (* 1 - 1a - 2a - 3a
        \- 1b
        \- 1c
      and keep in upper 1a - 2a - 3a
                          \- 1c *)
  let test_without_lower_min_upper () =
    let* ctxt = init ~with_lower:false () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt, block1b = checkout_and_commit ctxt block1 commit_block1b in
    let* ctxt, block1c = checkout_and_commit ctxt block1 commit_block1c in
    let* ctxt, block2a = checkout_and_commit ctxt block1a commit_block2a in
    let* ctxt, block3a = checkout_and_commit ctxt block2a commit_block3a in
    let* () =
      Store.freeze ctxt.index.repo ~min_upper:[ block1a; block1c ]
        ~max_lower:[ block2a; block1c; block3a ]
    in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_removed ctxt block1 "block1" >>= fun () ->
    check_removed ctxt block1b "block1b" >>= fun () ->
    check_block1c ctxt.index.repo block1c >>= fun () ->
    check_block3a ctxt.index.repo block3a >>= fun () ->
    check_block1a ctxt.index.repo block1a >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (** Open layered store without lower, close it and open it with the lower
      layer. *)
  let test_reopen_with_lower () =
    let* ctxt = init ~with_lower:false () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt = freeze ctxt block1 in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* ctxt = clone ~with_lower:true ctxt.index.root in
    check_block1 ctxt.index.repo block1 >>= fun () ->
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    Store.freeze ctxt.index.repo ~max_lower:[ block1a ] >>= fun () ->
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    let check_layer block msg exp =
      Store.layer_id ctxt.index.repo (Store.Commit_t (Store.Commit.hash block))
      >|= Irmin_test.check Irmin_layers.Layer_id.t msg exp
    in
    check_layer block1 "check layer of block1" `Lower >>= fun () ->
    check_layer block1a "check layer of block1a" `Upper1 >>= fun () ->
    Store.Repo.close ctxt.index.repo

  (* 1 - 1a - 2a - 3a
        \- 1b
        \- 1c *)

  (** the upper is self contained for 3a and 1c; the store is reopened without
      lower and the commits are checked. *)
  let test_self_contained () =
    let* ctxt = init () in
    let* ctxt, block1 = commit_block1 ctxt in
    let* ctxt, block1a = checkout_and_commit ctxt block1 commit_block1a in
    let* ctxt, block1b = checkout_and_commit ctxt block1 commit_block1b in
    let* ctxt, block1c = checkout_and_commit ctxt block1 commit_block1c in
    let* ctxt, block2a = checkout_and_commit ctxt block1a commit_block2a in
    let* () =
      Store.freeze ctxt.index.repo
        ~max_lower:[ block2a; block1b; block1c ]
        ~max_upper:[]
    in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    let* ctxt, block3a = checkout_and_commit ctxt block2a commit_block3a in
    Store.self_contained ~max:[ block3a; block1c ] ctxt.index.repo >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    let* ctxt = clone ~readonly:false ~with_lower:false ctxt.index.root in
    check_block3a ctxt.index.repo block3a >>= fun () ->
    check_removed ctxt block1b "block1b" >>= fun () ->
    check_block1c ctxt.index.repo block1c >>= fun () ->
    Store.Repo.close ctxt.index.repo

  module Hook = Store.Private_layer.Hook

  let hook before after =
    Hook.v (function
      | `Before_Copy -> before ()
      | `After_Clear -> after ()
      | _ -> Lwt.return_unit)

  let test_ro_find_during_freeze () =
    let* ctxt = init () in
    let* ro_ctxt = clone ~readonly:true ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    let before () =
      Store.sync ro_ctxt.index.repo;
      Lwt.return_unit
    in
    let after () =
      Store.sync ro_ctxt.index.repo;
      check_block1 ro_ctxt.index.repo block1
    in
    let* () =
      Store.Private_layer.freeze' ctxt.index.repo ~max_lower:[ block1 ]
        ~hook:(hook before after)
    in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    check_block1 ro_ctxt.index.repo block1 >>= fun () ->
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  let test_ro_checks_async_freeze () =
    let* ctxt = init () in
    let* ro_ctxt = clone ~readonly:true ctxt.index.root in
    let* ctxt, block1 = commit_block1 ctxt in
    let before () =
      if not (Store.async_freeze ro_ctxt.index.repo) then
        Alcotest.fail "Readonly checks for an ongoing freeze";
      Lwt.return_unit
    in
    let after () = Lwt.return_unit in
    let* () =
      Store.Private_layer.freeze' ctxt.index.repo ~max_lower:[ block1 ]
        ~hook:(hook before after)
    in
    Store.Private_layer.wait_for_freeze ctxt.index.repo >>= fun () ->
    if Store.async_freeze ro_ctxt.index.repo then
      Alcotest.fail "Readonly sees a freeze that finished";
    Store.Repo.close ctxt.index.repo >>= fun () ->
    Store.Repo.close ro_ctxt.index.repo

  let tests =
    [
      Alcotest.test_case "Test simple freeze" `Quick (fun () ->
          Lwt_main.run (test_simple_freeze ()));
      Alcotest.test_case "Test close and reopen a store" `Quick (fun () ->
          Lwt_main.run (test_close_freeze ()));
      Alcotest.test_case "Test ro instances" `Quick (fun () ->
          Lwt_main.run (test_ro_simple ()));
      Alcotest.test_case "Test ro after two freezes" `Quick (fun () ->
          Lwt_main.run (test_ro_sync_after_two_freezes ()));
      Alcotest.test_case "Test ro sync after v" `Quick (fun () ->
          Lwt_main.run (test_ro_sync_after_v ()));
      Alcotest.test_case "Delete lower and previous upper" `Quick (fun () ->
          Lwt_main.run (test_delete_stores ()));
      Alcotest.test_case "Open upper as simple store, then as layered" `Quick
        (fun () -> Lwt_main.run (test_upper1_reopen ()));
      Alcotest.test_case "Test open lower" `Quick (fun () ->
          Lwt_main.run (test_open_lower ()));
      Alcotest.test_case "Open lower as simple store, then as layered" `Quick
        (fun () -> Lwt_main.run (test_lower_reopen ()));
      Alcotest.test_case "Test without lower" `Quick (fun () ->
          Lwt_main.run (test_without_lower ()));
      Alcotest.test_case "Test without lower, min_upper" `Quick (fun () ->
          Lwt_main.run (test_without_lower_min_upper ()));
      Alcotest.test_case "Test reopen with lower" `Quick (fun () ->
          Lwt_main.run (test_reopen_with_lower ()));
      Alcotest.test_case "Test ro find during freeze" `Quick (fun () ->
          Lwt_main.run (test_ro_find_during_freeze ()));
      Alcotest.test_case "Test self contained upper" `Quick (fun () ->
          Lwt_main.run (test_self_contained ()));
      Alcotest.test_case "Test ro async freeze" `Quick (fun () ->
          Lwt_main.run (test_ro_checks_async_freeze ()));
    ]
end

let tests = Test.tests
