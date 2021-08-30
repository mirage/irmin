(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Extracted from https://gitlab.com/tezos/tezos. *)

open Irmin.Export_for_backends
open Bench_common

module Make
    (Store : Irmin.S
               with type Schema.Path.t = string list
                and type Schema.Branch.t = string) =
struct
  let current_data_key = [ "data" ]
  let data_key key = current_data_key @ key

  type index = { repo : Store.Repo.t }
  type context = { index : index; tree : Store.tree }

  let tree_fold ?depth t k ~init ~f =
    Store.Tree.find_tree t k >>= function
    | None -> Lwt.return init
    | Some t ->
        Store.Tree.fold ?depth ~force:`And_clear ~uniq:`False
          ~node:(fun k v acc -> f k (Store.Tree.of_node v) acc)
          ~contents:(fun k v acc ->
            if k = [] then Lwt.return acc
            else f k (Store.Tree.of_contents v) acc)
          t init

  let fold ?depth ctxt key ~init ~f =
    tree_fold ?depth ctxt.tree (data_key key) ~init ~f

  let add_tree ctxt key tree =
    Store.Tree.add_tree ctxt.tree (data_key key) tree >|= fun tree ->
    { ctxt with tree }

  let counter = ref 0

  module Flatten_storage_for_H = struct
    (* /tree_abs_key/key/*/*/*/*/*
       => /tree_abs_key/key/rename( */*/*/*/* )
    *)
    let flatten ~tree ~key ~depth ~rename ~init =
      tree_fold tree key ~depth:(`Eq depth) ~init
        ~f:(fun old_key tree dst_tree ->
          let new_key = rename old_key in
          Store.Tree.add_tree dst_tree new_key tree)
      >>= fun dst_tree ->
      (* rm -rf $index_key
         mv $tmp_index_key $index_key *)
      Store.Tree.add_tree tree key dst_tree

    (* /abs_key/*(depth')/mid_key/*(depth)
       => /abs_key/*(depth')/mid_key/rename( *(depth) )
    *)
    let fold_flatten ctxt abs_key depth' mid_key ~depth ~rename =
      fold ~depth:(`Eq depth') ctxt abs_key ~init:ctxt ~f:(fun key tree ctxt ->
          (* tree at /abs_key/*(depth') *)
          flatten ~tree ~key:mid_key ~depth ~rename ~init:Store.Tree.empty
          >>= fun tree ->
          incr counter;
          add_tree ctxt (abs_key @ key) tree)

    let flatten_storage ctxt =
      Logs.info (fun l ->
          l "Flattening the context storage.  It takes several minutes.");
      let rec drop n xs =
        match (n, xs) with
        | 0, _ -> xs
        | _, [] -> assert false
        | _, _ :: xs -> drop (n - 1) xs
      in
      report_mem_stats ();
      counter := 0;
      (* *)
      (* /contracts/index/xx/xx/xx/xx/xx/xx/yyyyyyyyyy
         => /contracts/index/yyyyyyyyyy
      *)
      fold_flatten ctxt [ "contracts"; "index" ] 0 [] ~depth:7 ~rename:(drop 6)
      >>= fun ctxt ->
      Logs.info (fun l -> l "flattened /contracts/index/ ");
      report_mem_stats ();
      (* *)
      (* /contracts/index/yyyyyyyyyy/delegated/xx/xx/xx/xx/xx/xx/zzzzzzzzzz
         => /contracts/index/yyyyyyyyyy/delegated/zzzzzzzzzz
      *)
      fold_flatten ctxt [ "contracts"; "index" ] 1 [ "delegated" ] ~depth:7
        ~rename:(drop 6)
      >>= fun ctxt ->
      Logs.info (fun l -> l "flattened /contracts/index/delegated/");
      report_mem_stats ();
      (* *)
      (* /rolls/owner/snapshot/n1/n2/x/y/n3
         => /rolls/owner/snapshot/n1/n2/n3
      *)
      fold_flatten ctxt
        [ "rolls"; "owner"; "snapshot" ]
        2 [] ~depth:3 ~rename:(drop 2)
      >|= fun ctxt ->
      Logs.info (fun l -> l "flatten /rolls/owner/snapshot");
      report_mem_stats ();
      Logs.info (fun l -> l "Flattened the context storage.");
      ctxt
  end

  let close { index; _ } = Store.Repo.close index.repo

  let run_migration root =
    let* repo =
      Store.Repo.v (Irmin_pack.config ~readonly:false ~fresh:false root)
    in
    let index = { repo } in
    let* commit =
      Store.Branch.find repo "migrate" >>= function
      | None -> Lwt.fail_with "branch migrate not found"
      | Some commit -> Lwt.return commit
    in
    let tree = Store.Commit.tree commit in
    let context = { index; tree } in
    let* ctxt = Flatten_storage_for_H.flatten_storage context in
    close ctxt
end
