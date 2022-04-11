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

open Irmin.Export_for_backends
open Bench_common
include Trace_replay_intf
module Def = Trace_definitions.Replayable_trace

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

  type context = { tree : Store.tree }

  type t = {
    contexts : (int64, context) Hashtbl.t;
    hash_corresps : (Def.hash, Store.commit_key) Hashtbl.t;
    mutable latest_commit : Store.Hash.t option;
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
    Store.Commit.of_key repo h_store >|= function
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
    let+ tree = Store.Tree.add tree key v in
    Stat_collector.short_op_end stats `Add;
    Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
    maybe_forget_ctx t out_ctx_id

  let exec_remove t stats keys in_ctx_id out_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let+ tree = Store.Tree.remove tree keys in
    Stat_collector.short_op_end stats `Remove;
    Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
    maybe_forget_ctx t out_ctx_id

  let exec_copy t stats from to_ in_ctx_id out_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    Store.Tree.find_tree tree from >>= function
    | None -> failwith "Couldn't find tree in exec_copy"
    | Some sub_tree ->
        let* tree = Store.Tree.add_tree tree to_ sub_tree in
        Stat_collector.short_op_end stats `Copy;
        Hashtbl.add t.contexts (unscope out_ctx_id) { tree };
        maybe_forget_ctx t out_ctx_id;
        Lwt.return_unit

  let exec_find t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let+ query = Store.Tree.find tree keys in
    Stat_collector.short_op_end stats `Find;
    if Option.is_some query <> b then
      error_find "find" keys b i n (unscope in_ctx_id)

  let exec_mem t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let+ b' = Store.Tree.mem tree keys in
    Stat_collector.short_op_end stats `Mem;
    if b <> b' then error_find "mem" keys b i n (unscope in_ctx_id)

  let exec_mem_tree t stats n i keys b in_ctx_id =
    let { tree } = Hashtbl.find t.contexts (unscope in_ctx_id) in
    maybe_forget_ctx t in_ctx_id;
    Stat_collector.short_op_begin stats;
    let+ b' = Store.Tree.mem_tree tree keys in
    Stat_collector.short_op_end stats `Mem_tree;
    if b <> b' then error_find "mem_tree" keys b i n (unscope in_ctx_id)

  let check_hash_trace h_trace h_store =
    let h_store = Irmin.Type.(to_string Store.Hash.t) h_store in
    if h_trace <> h_store then
      Fmt.failwith "hash replay %s, hash trace %s" h_store h_trace

  (* for layers, we want to initiate a GC after every [layers_n] commits *)
  let layers_counter = ref 0
  let layers_n = 400

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
    let* () = Stat_collector.commit_begin stats tree in
    let* _ =
      (* in tezos commits call Tree.list first for the unshallow operation *)
      Store.Tree.list tree []
    in
    let info = Store.Info.v ~author:"Tezos" ~message date in
    let* commit = Store.Commit.v repo ~info ~parents:parents_store tree in
    let+ () = Stat_collector.commit_end stats tree in
    Store.Tree.clear tree;
    let k_store, h_store = Store.Commit.(key commit, hash commit) in
    if check_hash then check_hash_trace (unscope h_trace) h_store;
    (* It's okey to have [h_trace] already in history. It corresponds to
     * re-commiting the same thing, hence the [.replace] below. *)
    Hashtbl.replace t.hash_corresps (unscope h_trace) k_store;
    maybe_forget_hash t h_trace;
    t.latest_commit <- Some h_store;
    let _for_layers = 
      (* print out the hash of each commit *)
      let hash_as_string = (h_store : Store.hash) |> Irmin.Type.to_string Store.hash_t in
      Printf.printf "%s: processing commit %s\n%!" __FILE__ hash_as_string;
      (* for layers, we want to initiate a gc every so often *)
      incr layers_counter;
      match !layers_counter mod layers_n = 0 with
      | false -> ()
      | true -> 
        let hash_as_string = (h_store : Store.hash) |> Irmin.Type.to_string Store.hash_t in
        Store.trigger_gc repo hash_as_string;
        Printf.printf "Called GC for commit %s\n%!" hash_as_string;
        ()
    in
    (* NOTE this is absolutely crucial to prevent any carry over of objects indexed by
       hash (which may not be recorded by [create_reach.exe]) *)
    Irmin_pack_unix.Pack_store.clear_all_caches ();
    ()


  let add_operations t repo operations n stats check_hash empty_blobs =
    let rec aux l i =
      match l with
      | Def.Checkout (h, out_ctx_id) :: tl ->
          let* () = exec_checkout t stats repo h out_ctx_id in
          aux tl (i + 1)
      | Add op :: tl ->
          let* () =
            exec_add t stats op.key op.value op.in_ctx_id op.out_ctx_id
              empty_blobs
          in
          aux tl (i + 1)
      | Remove (keys, in_ctx_id, out_ctx_id) :: tl ->
          let* () = exec_remove t stats keys in_ctx_id out_ctx_id in
          aux tl (i + 1)
      | Copy op :: tl ->
          let* () =
            exec_copy t stats op.key_src op.key_dst op.in_ctx_id op.out_ctx_id
          in
          aux tl (i + 1)
      | Find (keys, b, in_ctx_id) :: tl ->
          let* () = exec_find t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | Mem (keys, b, in_ctx_id) :: tl ->
          let* () = exec_mem t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | Mem_tree (keys, b, in_ctx_id) :: tl ->
          let* () = exec_mem_tree t stats n i keys b in_ctx_id in
          aux tl (i + 1)
      | [ Commit op ] ->
          exec_commit t stats repo op.hash op.date op.message op.parents
            op.in_ctx_id check_hash
      | Commit _ :: _ | [] ->
          failwith "A batch of operation should end with a commit"
    in
    aux operations 0

  let add_commits repo max_ncommits commit_seq on_commit on_end stats check_hash
      empty_blobs =
    with_progress_bar ~message:"Replaying trace" ~n:max_ncommits ~unit:"commit"
    @@ fun prog ->
    let t =
      {
        contexts = Hashtbl.create 3;
        hash_corresps = Hashtbl.create 3;
        latest_commit = None;
      }
    in

    (* Manually add genesis context *)
    Hashtbl.add t.contexts 0L { tree = Store.Tree.empty () };

    let rec aux commit_seq i =
      match commit_seq () with
      | Seq.Nil -> on_end () >|= fun () -> i
      | Cons (ops, commit_seq) ->
          let* () = add_operations t repo ops i stats check_hash empty_blobs in
          let len0 = Hashtbl.length t.contexts in
          let len1 = Hashtbl.length t.hash_corresps in
          if (len0, len1) <> (0, 1) then
            [%logs.app
              "\nAfter commit %6d we have %d/%d history sizes" i len0 len1];
          let* () = on_commit i (Option.get t.latest_commit) in
          prog 1;
          aux commit_seq (i + 1)
    in
    aux commit_seq 0

  let run : type a. _ -> a config -> a Lwt.t =
   fun ext_config config ->
    let check_hash =
      config.path_conversion = `None
      && config.inode_config = (32, 256)
      && config.empty_blobs = false
    in
    [%logs.app
      "Will %scheck commit hashes against reference."
        (if check_hash then "" else "NOT ")];
    let commit_seq =
      open_commit_sequence config.number_of_commits_to_replay
        config.path_conversion config.replay_trace_path
    in
    let root = Filename.concat config.artefacts_path "root" in
    let* repo, on_commit, on_end = Store.create_repo ~root ext_config in
    prepare_artefacts_dir config.artefacts_path;
    let stat_path = Filename.concat config.artefacts_path "stat_trace.repr" in
    let c =
      let entries, stable_hash = config.inode_config in
      Trace_definitions.Stat_trace.
        {
          setup =
            `Replay
              {
                path_conversion = config.path_conversion;
                artefacts_dir = config.artefacts_path;
              };
          inode_config = (entries, entries, stable_hash);
          store_type = config.store_type;
        }
    in
    let stats = Stat_collector.create_file stat_path c root in
    Irmin_pack.Stats.reset_stats ();
    Lwt.finalize
      (fun () ->
        let* block_count =
          add_commits repo config.number_of_commits_to_replay commit_seq
            on_commit on_end stats check_hash config.empty_blobs
        in
        [%logs.app "Closing repo..."];
        let+ () = Store.Repo.close repo in
        Stat_collector.close stats;
        match config.return_type with
        | Unit -> (() : a)
        | Summary ->
            [%logs.app "Computing summary..."];
            Trace_stat_summary.summarise ~block_count stat_path)
      (fun () ->
        if config.keep_stat_trace then (
          [%logs.app "Stat trace kept at %s" stat_path];
          Unix.chmod stat_path 0o444;
          Lwt.return_unit)
        else Lwt.return (Stat_collector.remove stats))
end
