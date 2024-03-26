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

open Irmin.Export_for_backends
open Bench_common
include Trace_replay_intf
module Def = Trace_definitions.Replayable_trace
module Seq = Trace_common.Seq

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_2char_hex s =
  if String.length s <> 2 then false
  else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

let all_6_2char_hex a b c d e f =
  is_2char_hex a
  && is_2char_hex b
  && is_2char_hex c
  && is_2char_hex d
  && is_2char_hex e
  && is_2char_hex f

let is_30char_hex s =
  if String.length s <> 30 then false
  else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

(** This function flattens all the 6 step-long chunks forming 40 byte-long
    hashes to a single step.

    Those flattenings are performed during the trace replay, i.e. they count in
    the total time.

    If a path contains 2 or more of those patterns, only the leftmost one is
    converted.

    A chopped hash has this form

    {v ([0-9a-f]{2}/){5}[0-9a-f]{30} v}

    and is flattened to that form

    {v [0-9a-f]{40} v} *)
let flatten_v0 key =
  let rec aux rev_prefix suffix =
    match suffix with
    | a :: b :: c :: d :: e :: f :: tl
      when is_2char_hex a
           && is_2char_hex b
           && is_2char_hex c
           && is_2char_hex d
           && is_2char_hex e
           && is_30char_hex f ->
        let mid = a ^ b ^ c ^ d ^ e ^ f in
        aux (mid :: rev_prefix) tl
    | hd :: tl -> aux (hd :: rev_prefix) tl
    | [] -> List.rev rev_prefix
  in
  aux [] key

(** This function removes from the paths all the 6 step-long hashes of this form

    {v ([0-9a-f]{2}/){6} v}

    Those flattenings are performed during the trace replay, i.e. they count in
    the total time.

    The paths in tezos:
    https://www.dailambda.jp/blog/2020-05-11-plebeia/#tezos-path

    Tezos' PR introducing this flattening:
    https://gitlab.com/tezos/tezos/-/merge_requests/2771 *)
let flatten_v1 = function
  | "data" :: "contracts" :: "index" :: a :: b :: c :: d :: e :: f :: tl
    when all_6_2char_hex a b c d e f -> (
      match tl with
      | hd :: "delegated" :: a :: b :: c :: d :: e :: f :: tl
        when all_6_2char_hex a b c d e f ->
          "data" :: "contracts" :: "index" :: hd :: "delegated" :: tl
      | _ -> "data" :: "contracts" :: "index" :: tl)
  | "data" :: "big_maps" :: "index" :: a :: b :: c :: d :: e :: f :: tl
    when all_6_2char_hex a b c d e f ->
      "data" :: "big_maps" :: "index" :: tl
  | "data" :: "rolls" :: "index" :: _ :: _ :: tl ->
      "data" :: "rolls" :: "index" :: tl
  | "data" :: "rolls" :: "owner" :: "current" :: _ :: _ :: tl ->
      "data" :: "rolls" :: "owner" :: "current" :: tl
  | "data" :: "rolls" :: "owner" :: "snapshot" :: a :: b :: _ :: _ :: tl ->
      "data" :: "rolls" :: "owner" :: "snapshot" :: a :: b :: tl
  | l -> l

let flatten_op ~flatten_path = function
  | Def.Checkout _ as op -> op
  | Add op -> Add { op with key = flatten_path op.key }
  | Remove (keys, in_ctx_id, out_ctx_id) ->
      Remove (flatten_path keys, in_ctx_id, out_ctx_id)
  | Copy op ->
      Copy
        {
          op with
          key_src = flatten_path op.key_src;
          key_dst = flatten_path op.key_dst;
        }
  | Find (keys, b, ctx) -> Find (flatten_path keys, b, ctx)
  | Mem (keys, b, ctx) -> Mem (flatten_path keys, b, ctx)
  | Mem_tree (keys, b, ctx) -> Mem_tree (flatten_path keys, b, ctx)
  | Commit _ as op -> op

let open_commit_sequence max_ncommits path_conversion path : Def.row list Seq.t
    =
  let flatten_path =
    match path_conversion with
    | `None -> Fun.id
    | `V1 -> flatten_v1
    | `V0 -> flatten_v0
    | `V0_and_v1 -> fun p -> flatten_v1 p |> flatten_v0
  in

  let rec aux (ops_seq, commits_sent, ops) =
    if commits_sent >= max_ncommits then None
    else
      match ops_seq () with
      | Seq.Nil -> None
      | Cons ((Def.Commit _ as op), ops_seq) ->
          let ops = op :: ops |> List.rev in
          Some (ops, (ops_seq, commits_sent + 1, []))
      | Cons (op, ops_seq) ->
          let op = flatten_op ~flatten_path op in
          aux (ops_seq, commits_sent, op :: ops)
  in
  let _header, ops_seq = Def.open_reader path in
  Seq.unfold aux (ops_seq, 0, [])

module Make (Store : Store) = struct
  include Config
  module Stat_collector = Trace_collection.Make_stat (Store)

  type key = Store.contents_key [@@deriving irmin ~pp]
  type context = { tree : Store.tree }

  type t = {
    contexts : (int64, context) Hashtbl.t;
    hash_corresps : (Def.hash, Store.commit_key) Hashtbl.t;
    mutable commits_since_start_or_gc : int;
    mutable latest_commit_idx : int;
        (** the most recent commit idx to be replayed. initial value is -1 *)
    mutable gc_count : int;
    key_per_commit_idx : (int, Store.commit_key) Hashtbl.t;
  }

  let error_find op k b n_op n_c in_ctx_id =
    Fmt.failwith
      "Cannot reproduce operation %d on ctx %Ld of commit %d %s @[k = %a@] \
       expected %b"
      n_op in_ctx_id n_c op
      Fmt.(list ~sep:comma string)
      k b

  let unscope = function Def.Forget v -> v | Keep v -> v

  let maybe_forget_hash t = function
    | Def.Forget h -> Hashtbl.remove t.hash_corresps h
    | Keep _ -> ()

  let maybe_forget_ctx t = function
    | Def.Forget ctx -> Hashtbl.remove t.contexts ctx
    | Keep _ -> ()

  let exec_checkout t stats repo h_trace out_ctx_id =
    let h_store = Hashtbl.find t.hash_corresps (unscope h_trace) in
    maybe_forget_hash t h_trace;
    Stat_collector.short_op_begin stats;
    match Store.Commit.of_key repo h_store with
    | None -> failwith "prev commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        Stat_collector.short_op_end stats `Checkout;
        Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
        maybe_forget_ctx t out_ctx_id

  let exec_add t stats key v in_ctx_id out_ctx_id empty_blobs =
    let v = if empty_blobs then Bytes.empty else Bytes.of_string v in
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let tree = Store.Tree.add tree key v in
    Stat_collector.short_op_end stats `Add;
    Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
    maybe_forget_ctx t out_ctx_id

  let exec_remove t stats keys in_ctx_id out_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let tree = Store.Tree.remove tree keys in
    Stat_collector.short_op_end stats `Remove;
    Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
    maybe_forget_ctx t out_ctx_id

  let exec_copy t stats from to_ in_ctx_id out_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    match Store.Tree.find_tree tree from with
    | None -> failwith "Couldn't find tree in exec_copy"
    | Some sub_tree ->
        let tree = Store.Tree.add_tree tree to_ sub_tree in
        Stat_collector.short_op_end stats `Copy;
        Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
        maybe_forget_ctx t out_ctx_id

  let exec_find t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let query = Store.Tree.find tree keys in
    Stat_collector.short_op_end stats `Find;
    if Option.is_some query <> b then
      error_find "find" keys b i n (unscope in_ctx_id)

  let exec_mem t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let b' = Store.Tree.mem tree keys in
    Stat_collector.short_op_end stats `Mem;
    if b <> b' then error_find "mem" keys b i n (unscope in_ctx_id)

  let exec_mem_tree t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let b' = Store.Tree.mem_tree tree keys in
    Stat_collector.short_op_end stats `Mem_tree;
    if b <> b' then error_find "mem_tree" keys b i n (unscope in_ctx_id)

  let check_hash_trace h_trace h_store =
    let h_store = Irmin.Type.(to_string Store.Hash.t) h_store in
    if h_trace <> h_store then
      Fmt.failwith "hash replay %s, hash trace %s" h_store h_trace

  let exec_commit t stats repo h_trace date message parents_trace in_ctx_id
      check_hash =
    let parents_store =
      parents_trace
      |> List.map unscope
      |> List.map (Hashtbl.find t.hash_corresps)
    in
    List.iter (maybe_forget_hash t) parents_trace;
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    let () = Stat_collector.commit_begin stats tree in
    let _ =
      (* in tezos commits call Tree.list first for the unshallow operation *)
      Store.Tree.list tree []
    in
    let info = Store.Info.v ~author:"Tezos" ~message date in
    let commit = Store.Commit.v repo ~info ~parents:parents_store tree in
    let () = Stat_collector.commit_end stats tree in
    Store.Tree.clear tree;
    let k_store, h_store = Store.Commit.(key commit, hash commit) in
    if check_hash then check_hash_trace (unscope h_trace) h_store;
    (* It's okey to have [h_trace] already in history. It corresponds to
     * re-commiting the same thing, hence the [.replace] below. *)
    Hashtbl.replace t.hash_corresps (unscope h_trace) k_store;
    maybe_forget_hash t h_trace;
    let () =
      let tbl = t.key_per_commit_idx in
      Hashtbl.add tbl (Hashtbl.length tbl) k_store
    in
    ()

  let add_operations t repo operations n stats check_hash empty_blobs =
    let rec aux l i =
      match l with
      | Def.Checkout (h, out_ctx_id) :: tl ->
          let () = exec_checkout t stats repo h out_ctx_id in
          aux tl (i + 1)
      | Add op :: tl ->
          let () =
            exec_add t stats op.key op.value op.in_ctx_id op.out_ctx_id
              empty_blobs
          in
          aux tl (i + 1)
      | Remove (keys, in_ctx_id, out_ctx_id) :: tl ->
          let () = exec_remove t stats keys in_ctx_id out_ctx_id in
          aux tl (i + 1)
      | Copy op :: tl ->
          let () =
            exec_copy t stats op.key_src op.key_dst op.in_ctx_id op.out_ctx_id
          in
          aux tl (i + 1)
      | Find (keys, b, in_ctx_id) :: tl ->
          let () = exec_find t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | Mem (keys, b, in_ctx_id) :: tl ->
          let () = exec_mem t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | Mem_tree (keys, b, in_ctx_id) :: tl ->
          let () = exec_mem_tree t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | [ Commit op ] ->
          exec_commit t stats repo op.hash op.date op.message op.parents
            op.in_ctx_id check_hash
      | Commit _ :: _ | [] ->
          failwith "A batch of operation should end with a commit"
    in
    aux operations 0

  let gc_actions config i commits_since_start_or_gc gc_count =
    let gc_enabled =
      (* Is GC enabled at all? *)
      config.gc_every > 0
    in
    let gc_wait_enabled =
      (* Will GC wait be called at all? *)
      gc_enabled && config.gc_wait_after > 0
    in

    let first_gc_occured = i <> commits_since_start_or_gc in

    let time_to_split = commits_since_start_or_gc = config.gc_every in

    let time_to_start =
      (* Is it time to start GC? *)
      if first_gc_occured then time_to_split
      else
        let gc_commit_idx =
          (* [i] is the replay step and also the commit index of the next
             commit and also the number of commits replayed so far.

             [i - t.gc_distance_in_the_past - 1] is the index of the commit we
             want to GC. *)
          i - config.gc_distance_in_the_past - 1
        in
        gc_commit_idx = 1
    in

    let time_to_wait =
      (* Is it time to wait GC? *)
      if first_gc_occured then commits_since_start_or_gc = config.gc_wait_after
      else false
    in

    let time_to_add_volume =
      config.add_volume_every > 0
      && time_to_split
      && gc_count > 0
      && gc_count mod config.add_volume_every = 0
    in

    let really_split = gc_enabled && time_to_split in
    let really_start_gc = gc_enabled && time_to_start in
    let really_wait_gc = gc_wait_enabled && time_to_wait in
    let really_add_volume = time_to_add_volume in
    (really_wait_gc, really_start_gc, really_split, really_add_volume)

  let add_commits ~fs ~domain_mgr config repo commit_seq on_commit on_end stats
      check_hash empty_blobs =
    let max_ncommits = config.number_of_commits_to_replay in
    with_progress_bar ~message:"Replaying trace" ~n:max_ncommits ~unit:"commit"
    @@ fun prog ->
    let t =
      {
        contexts = Hashtbl.create 3;
        hash_corresps = Hashtbl.create 3;
        commits_since_start_or_gc = 0;
        gc_count = 0;
        latest_commit_idx = -1;
        key_per_commit_idx = Hashtbl.create 3;
      }
    in

    (* Manually add genesis context *)
    Hashtbl.add t.contexts 0L { tree = Store.Tree.empty () };

    let rec aux commit_seq i =
      match commit_seq () with
      | Seq.Nil ->
          on_end ();
          i
      | Cons (ops, commit_seq) ->
          let really_wait_gc, really_start_gc, really_split, really_add_volume =
            gc_actions config i t.commits_since_start_or_gc t.gc_count
          in
          (* Split before GC to simulate how it is inteded to be used. *)
          let () = if really_split then Store.split repo in
          let () = if really_add_volume then Store.add_volume repo in
          let () =
            if really_wait_gc then (
              [%logs.app
                "Waiting gc while latest commit has idx %d" t.latest_commit_idx];
              Store.gc_wait repo)
          in
          let () =
            if really_start_gc then (
              (* Starting GC.

                 TODO: If the GC-commit is an orphan commit we will have
                 problems. *)
              let gc_commit_idx =
                t.latest_commit_idx - config.gc_distance_in_the_past
              in
              let gc_commit_key =
                Hashtbl.find t.key_per_commit_idx gc_commit_idx
              in
              let gc_start_commit_idx = t.latest_commit_idx in
              (* used in closure below to know start commit of gc process *)
              t.commits_since_start_or_gc <- 0;
              [%logs.app
                "Starting gc on commit idx %d with key %a while latest commit \
                 has idx %d with key %a"
                gc_commit_idx pp_key gc_commit_key gc_start_commit_idx pp_key
                  (Hashtbl.find t.key_per_commit_idx t.latest_commit_idx)];
              let finished = function
                | Ok stats ->
                    t.gc_count <- t.gc_count + 1;
                    let commit_idx = t.latest_commit_idx in
                    let commit_duration = commit_idx - gc_start_commit_idx in
                    let duration =
                      Irmin_pack_unix.Stats.Latest_gc.total_duration stats
                    in
                    let finalise_duration =
                      Irmin_pack_unix.Stats.Latest_gc.finalise_duration stats
                    in
                    [%logs.app
                      "Gc ended after %d commits; duration: %fs; \
                       finalise_duration: %fs"
                      commit_duration duration finalise_duration]
                | Error s -> failwith s
              in
              Store.gc_run ~fs ~domain_mgr ~finished repo gc_commit_key)
          in
          let () = add_operations t repo ops i stats check_hash empty_blobs in
          t.latest_commit_idx <- i;
          let len0 = Hashtbl.length t.contexts in
          let len1 = Hashtbl.length t.hash_corresps in
          if (len0, len1) <> (0, 1) then
            [%logs.app
              "\nAfter commit %6d we have %d/%d history sizes"
                t.latest_commit_idx len0 len1];
          let () =
            on_commit t.latest_commit_idx
              (Hashtbl.find t.key_per_commit_idx t.latest_commit_idx
              |> Store.Backend.Commit.Key.to_hash)
          in
          t.commits_since_start_or_gc <- t.commits_since_start_or_gc + 1;
          prog 1;
          aux commit_seq (t.latest_commit_idx + 1)
    in
    aux commit_seq 0

  let run :
      type a.
      fs:Eio.Fs.dir_ty Eio.Path.t ->
      domain_mgr:_ Eio.Domain_manager.t ->
      _ ->
      a config ->
      a =
   fun ~fs ~domain_mgr ext_config config ->
    let check_hash =
      config.path_conversion = `None
      && config.inode_config = (32, 256)
      && config.empty_blobs = false
    in
    [%logs.app
      "Will %scheck commit hashes against reference."
        (if check_hash then "" else "NOT ")];
    Eio.Switch.run @@ fun sw ->
    let commit_seq =
      open_commit_sequence config.number_of_commits_to_replay
        config.path_conversion config.replay_trace_path
    in
    let root = Eio.Path.(config.artefacts_path / "root") in
    let repo, on_commit, on_end = Store.create_repo ~sw ~fs ~root ext_config in
    prepare_artefacts_dir config.artefacts_path;
    let stat_path = Eio.Path.(config.artefacts_path / "stat_trace.repr") in
    let c =
      let entries, stable_hash = config.inode_config in
      Trace_definitions.Stat_trace.
        {
          setup =
            `Replay
              {
                path_conversion = config.path_conversion;
                artefacts_dir = Eio.Path.native_exn config.artefacts_path;
              };
          inode_config = (entries, entries, stable_hash);
          store_type = config.store_type;
        }
    in
    let stats =
      Stat_collector.create_file stat_path c (Eio.Path.native_exn root)
    in
    Irmin_pack.Stats.reset_stats ();
    Fun.protect
      (fun () ->
        let block_count =
          add_commits ~fs ~domain_mgr config repo commit_seq on_commit on_end
            stats check_hash config.empty_blobs
        in
        [%logs.app "Closing repo..."];
        let () = Store.Repo.close repo in
        Stat_collector.close stats;
        match config.return_type with
        | Unit -> (() : a)
        | Summary ->
            [%logs.app "Computing summary..."];
            Trace_stat_summary.summarise ~block_count stat_path)
      ~finally:(fun () ->
        if config.keep_stat_trace then (
          [%logs.app "Stat trace kept at %s" (Eio.Path.native_exn stat_path)];
          Unix.chmod (Eio.Path.native_exn stat_path) 0o444)
        else Stat_collector.remove stats)
end
