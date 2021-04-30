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

open Bench_common
open Irmin.Export_for_backends
open Irmin_traces

type config = {
  ncommits : int;
  ncommits_trace : int;
  depth : int;
  nchain_trees : int;
  width : int;
  nlarge_trees : int;
  store_dir : string;
  path_conversion : [ `None | `V1 | `V0_and_v1 | `V0 ];
  inode_config : int * int;
  store_type : [ `Pack | `Pack_layered ];
  freeze_commit : int;
  commit_data_file : string;
  artefacts_dir : string;
  keep_store : bool;
  keep_stat_trace : bool;
  no_summary : bool;
  empty_blobs : bool;
}

module type Store = sig
  include Irmin.KV with type contents = bytes

  type on_commit := int -> Hash.t -> unit Lwt.t
  type on_end := unit -> unit Lwt.t
  type pp := Format.formatter -> unit

  val create_repo : config -> (Repo.t * on_commit * on_end * pp) Lwt.t
end

let pp_inode_config ppf (entries, stable_hash) =
  Format.fprintf ppf "[%d, %d]" entries stable_hash

let pp_store_type ppf = function
  | `Pack -> Format.fprintf ppf "[pack store]"
  | `Pack_layered -> Format.fprintf ppf "[pack-layered store]"

module Bootstrap_trace = struct
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

      Those flattenings are performed during the trace replay, i.e. they count
      in the total time.

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

  (** This function removes from the paths all the 6 step-long hashes of this
      form

      {v ([0-9a-f]{2}/){6} v}

      Those flattenings are performed during the trace replay, i.e. they count
      in the total time.

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

  let open_commit_sequence max_ncommits path_conversion path :
      Def.row list Seq.t =
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
end

module Trace_replay (Store : Store) = struct
  module Stat_collector = Trace_collection.Make_stat (Store)

  type context = { tree : Store.tree }

  type t = {
    contexts : (int64, context) Hashtbl.t;
    hash_corresps : (Bootstrap_trace.Def.hash, Store.Hash.t) Hashtbl.t;
    mutable latest_commit : Store.Hash.t option;
  }

  let error_find op k b n_op n_c in_ctx_id =
    Fmt.failwith
      "Cannot reproduce operation %d on ctx %Ld of commit %d %s @[k = %a@] \
       expected %b"
      n_op in_ctx_id n_c op
      Fmt.(list ~sep:comma string)
      k b

  let unscope = function Bootstrap_trace.Def.Forget v -> v | Keep v -> v

  let maybe_forget_hash t = function
    | Bootstrap_trace.Def.Forget h -> Hashtbl.remove t.hash_corresps h
    | Keep _ -> ()

  let maybe_forget_ctx t = function
    | Bootstrap_trace.Def.Forget ctx -> Hashtbl.remove t.contexts ctx
    | Keep _ -> ()

  let exec_checkout t stats repo h_trace out_ctx_id =
    let h_store = Hashtbl.find t.hash_corresps (unscope h_trace) in
    maybe_forget_hash t h_trace;
    Stat_collector.short_op_begin stats;
    Store.Commit.of_hash repo h_store >|= function
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
    let info = Irmin.Info.v ~date ~author:"Tezos" message in
    let* commit = Store.Commit.v repo ~info ~parents:parents_store tree in
    let+ () = Stat_collector.commit_end stats tree in
    Store.Tree.clear tree;
    let h_store = Store.Commit.hash commit in
    if check_hash then check_hash_trace (unscope h_trace) h_store;
    (* It's okey to have [h_trace] already in history. It corresponds to
     * re-commiting the same thing, hence the [.replace] below. *)
    Hashtbl.replace t.hash_corresps (unscope h_trace) h_store;
    maybe_forget_hash t h_trace;
    t.latest_commit <- Some h_store

  let add_operations t repo operations n stats check_hash empty_blobs =
    let rec aux l i =
      match l with
      | Bootstrap_trace.Def.Checkout (h, out_ctx_id) :: tl ->
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
    with_progress_bar ~message:"Replaying trace" ~n:max_ncommits ~unit:"commits"
    @@ fun prog ->
    let t =
      {
        contexts = Hashtbl.create 3;
        hash_corresps = Hashtbl.create 3;
        latest_commit = None;
      }
    in

    (* Manually add genesis context *)
    Hashtbl.add t.contexts 0L { tree = Store.Tree.empty };

    let rec aux commit_seq i =
      match commit_seq () with
      | Seq.Nil -> on_end () >|= fun () -> i
      | Cons (ops, commit_seq) ->
          let* () = add_operations t repo ops i stats check_hash empty_blobs in
          let len0 = Hashtbl.length t.contexts in
          let len1 = Hashtbl.length t.hash_corresps in
          if (len0, len1) <> (0, 1) then
            Logs.app (fun l ->
                l "\nAfter commit %6d we have %d/%d history sizes" i len0 len1);
          let* () = on_commit i (Option.get t.latest_commit) in
          prog Int64.one;
          aux commit_seq (i + 1)
    in
    aux commit_seq 0

  let run config =
    let check_hash =
      config.path_conversion = `None
      && config.inode_config = (32, 256)
      && config.empty_blobs = false
    in
    Logs.app (fun l ->
        l "Will %scheck commit hashes against reference."
          (if check_hash then "" else "NOT "));
    let commit_seq =
      Bootstrap_trace.open_commit_sequence config.ncommits_trace
        config.path_conversion config.commit_data_file
    in
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in
    prepare_artefacts_dir config.artefacts_dir;
    let summary_path =
      Filename.concat config.artefacts_dir "boostrap_summary.json"
    in
    let stat_path = Filename.concat config.artefacts_dir "stat_trace.repr" in
    let c =
      let entries, stable_hash = config.inode_config in
      Trace_definitions.Stat_trace.
        {
          setup =
            `Replay
              {
                path_conversion = config.path_conversion;
                artefacts_dir = config.artefacts_dir;
              };
          inode_config = (entries, entries, stable_hash);
          store_type = config.store_type;
        }
    in
    let stats = Stat_collector.create_file stat_path c config.store_dir in
    let+ summary_opt =
      Lwt.finalize
        (fun () ->
          let* block_count =
            add_commits repo config.ncommits_trace commit_seq on_commit on_end
              stats check_hash config.empty_blobs
          in
          Logs.app (fun l -> l "Closing repo...");
          let+ () = Store.Repo.close repo in
          Stat_collector.close stats;
          if not config.no_summary then (
            Logs.app (fun l -> l "Computing summary...");
            Some (Trace_stat_summary.summarise ~block_count stat_path))
          else None)
        (fun () ->
          if config.keep_stat_trace then (
            Logs.app (fun l -> l "Stat trace kept at %s" stat_path);
            Unix.chmod stat_path 0o444;
            Lwt.return_unit)
          else Lwt.return (Stat_collector.remove stats))
    in
    match summary_opt with
    | Some summary ->
        Trace_stat_summary.save_to_json summary summary_path;
        fun ppf ->
          Format.fprintf ppf "\n%t\n%a" repo_pp
            (Trace_stat_summary_pp.pp 5)
            ([ "" ], [ summary ])
    | None -> fun ppf -> Format.fprintf ppf "\n%t\n" repo_pp
end

module Benchmark = struct
  type result = { time : float; size : int }

  let run config f =
    let+ time, res = with_timer f in
    let size = FSHelper.get_size config.store_dir in
    ({ time; size }, res)

  let pp_results ppf result =
    Format.fprintf ppf "Total time: %f@\nSize on disk: %d M" result.time
      result.size
end

module Hash = Irmin.Hash.SHA1

module Bench_suite (Store : Store) = struct
  let init_commit repo =
    Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

  module Trees = Generate_trees (Store)
  module Trace_replay = Trace_replay (Store)

  let checkout_and_commit repo prev_commit f =
    Store.Commit.of_hash repo prev_commit >>= function
    | None -> Lwt.fail_with "commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* tree = f tree in
        Store.Commit.v repo ~info:(info ()) ~parents:[ prev_commit ] tree

  let add_commits ~message repo ncommits on_commit on_end f () =
    with_progress_bar ~message ~n:ncommits ~unit:"commits" @@ fun prog ->
    let* c = init_commit repo in
    let rec aux c i =
      if i >= ncommits then on_end ()
      else
        let* c' = checkout_and_commit repo (Store.Commit.hash c) f in
        let* () = on_commit i (Store.Commit.hash c') in
        prog Int64.one;
        aux c' (i + 1)
    in
    aux c 0

  let run_large config =
    reset_stats ();
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in
    let* result, () =
      Trees.add_large_trees config.width config.nlarge_trees
      |> add_commits ~message:"Playing large mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Large trees mode on inode config %a, %a: %d commits, each consisting \
         of %d large trees of %d entries@\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nlarge_trees config.width repo_pp
        Benchmark.pp_results result

  let run_chains config =
    reset_stats ();
    let* repo, on_commit, on_end, repo_pp = Store.create_repo config in
    let* result, () =
      Trees.add_chain_trees config.depth config.nchain_trees
      |> add_commits ~message:"Playing chain mode" repo config.ncommits
           on_commit on_end
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Chain trees mode on inode config %a, %a: %d commits, each consisting \
         of %d chains of depth %d@\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nchain_trees config.depth repo_pp
        Benchmark.pp_results result

  let run_read_trace = Trace_replay.run
end


module Make_store_pack (Conf : sig
  val entries : int
  val stable_hash : int
end) =
struct
  open Tezos_context_hash.Encoding

  module Store =
    Irmin_pack.Make_ext
      (struct
        let io_version = `V1
      end)
      (Conf)
      (Metadata)
      (Contents)
      (Path)
      (Branch)
      (Hash)
      (Node)
      (Commit)

  let create_repo config =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true config.store_dir in
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let on_end () = Lwt.return_unit in
    let pp _ = () in
    Lwt.return (repo, on_commit, on_end, pp)

  include Store
end

module Make_store_layered = Make_store_pack

module type B = sig
  val run_large : config -> (Format.formatter -> unit) Lwt.t
  val run_chains : config -> (Format.formatter -> unit) Lwt.t
  val run_read_trace : config -> (Format.formatter -> unit) Lwt.t
end

let store_of_config config =
  let entries, stable_hash = config.inode_config in
  let module Conf = struct
    let entries = entries
    let stable_hash = stable_hash
  end in
  match config.store_type with
  | `Pack -> (module Bench_suite (Make_store_pack (Conf)) : B)
  | `Pack_layered -> (module Bench_suite (Make_store_layered (Conf)) : B)

type suite_elt = {
  mode : [ `Read_trace | `Chains | `Large ];
  speed : [ `Quick | `Slow | `Custom ];
  run : config -> (Format.formatter -> unit) Lwt.t;
}

let suite : suite_elt list =
  [
    {
      mode = `Read_trace;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
    {
      mode = `Read_trace;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
    {
      mode = `Chains;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_chains config);
    };
    {
      mode = `Chains;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (2, 5); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_chains config);
    };
    {
      mode = `Large;
      speed = `Quick;
      run =
        (fun config ->
          let config =
            { config with inode_config = (32, 256); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_large config);
    };
    {
      mode = `Large;
      speed = `Slow;
      run =
        (fun config ->
          let config =
            { config with inode_config = (2, 5); store_type = `Pack }
          in
          let (module Store) = store_of_config config in
          Store.run_large config);
    };
    {
      mode = `Read_trace;
      speed = `Custom;
      run =
        (fun config ->
          let (module Store) = store_of_config config in
          Store.run_read_trace config);
    };
  ]

let get_suite suite_filter =
  List.filter
    (fun { mode; speed; _ } ->
      match (suite_filter, speed, mode) with
      | `Slow, `Slow, `Read_trace ->
          (* The suite contains several `Read_trace benchmarks, let's keep the
             slow one only *)
          true
      | `Slow, _, `Read_trace -> false
      | `Slow, (`Slow | `Quick), _ -> true
      | `Quick, `Quick, _ -> true
      | `Custom_trace, `Custom, `Read_trace -> true
      | `Custom_chains, `Custom, `Chains -> true
      | `Custom_large, `Custom, `Large -> true
      | (`Slow | `Quick | `Custom_trace | `Custom_chains | `Custom_large), _, _
        ->
          false)
    suite

let main () ncommits ncommits_trace suite_filter inode_config store_type
    freeze_commit path_conversion depth width nchain_trees nlarge_trees
    commit_data_file artefacts_dir keep_store keep_stat_trace no_summary
    empty_blobs =
  let default = match suite_filter with `Quick -> 10000 | _ -> 13315 in
  let ncommits_trace = Option.value ~default ncommits_trace in
  let config =
    {
      ncommits;
      ncommits_trace;
      store_dir = Filename.concat artefacts_dir "store";
      path_conversion;
      depth;
      width;
      nchain_trees;
      nlarge_trees;
      commit_data_file;
      inode_config;
      store_type;
      freeze_commit;
      artefacts_dir;
      keep_store;
      keep_stat_trace;
      no_summary;
      empty_blobs;
    }
  in
  Printexc.record_backtrace true;
  Random.self_init ();
  FSHelper.rm_dir config.store_dir;
  let suite = get_suite suite_filter in
  let run_benchmarks () = Lwt_list.map_s (fun b -> b.run config) suite in
  let results =
    Lwt_main.run
      (Lwt.finalize run_benchmarks (fun () ->
           if keep_store then (
             Logs.app (fun l -> l "Store kept at %s" config.store_dir);
             let ( / ) = Filename.concat in
             let ro p = if Sys.file_exists p then Unix.chmod p 0o444 in
             ro (config.store_dir / "store.branches");
             ro (config.store_dir / "store.dict");
             ro (config.store_dir / "store.pack");
             ro (config.store_dir / "index" / "data");
             ro (config.store_dir / "index" / "log");
             ro (config.store_dir / "index" / "log_async"))
           else FSHelper.rm_dir config.store_dir;
           Lwt.return_unit))
  in
  Logs.app (fun l ->
      l "%a@." Fmt.(list ~sep:(any "@\n@\n") (fun ppf f -> f ppf)) results)

open Cmdliner

let mode =
  let mode =
    [
      ("slow", `Slow);
      ("quick", `Quick);
      ("trace", `Custom_trace);
      ("chains", `Custom_chains);
      ("large", `Custom_large);
    ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "mode" ] in
  Arg.(value @@ opt (Arg.enum mode) `Slow doc)

let inode_config =
  let doc = Arg.info ~doc:"Inode config" [ "inode-config" ] in
  Arg.(value @@ opt (pair int int) (32, 256) doc)

let store_type =
  let mode = [ ("pack", `Pack); ("pack-layered", `Pack_layered) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "store-type" ] in
  Arg.(value @@ opt (Arg.enum mode) `Pack doc)

let freeze_commit =
  let doc =
    Arg.info
      ~doc:"Index of the commit after which to start the layered store freeze."
      [ "freeze-commit" ]
  in
  Arg.(value @@ opt int 1664 doc)

let path_conversion =
  let mode =
    [ ("none", `None); ("v0", `V0); ("v1", `V1); ("v0+v1", `V0_and_v1) ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "p"; "path-conversion" ] in
  Arg.(value @@ opt (Arg.enum mode) `None doc)

let ncommits =
  let doc =
    Arg.info ~doc:"Number of commits for the large and chain modes."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt int 2 doc)

let ncommits_trace =
  let doc =
    Arg.info ~doc:"Number of commits to read from trace." [ "ncommits-trace" ]
  in
  Arg.(value @@ opt (some int) None doc)

let keep_store =
  let doc =
    Arg.info ~doc:"Whether or not the irmin store on disk should be kept."
      [ "keep-store" ]
  in
  Arg.(value @@ flag doc)

let no_summary =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stat trace should be converted to a summary at the \
         end of a replay."
      [ "no-summary" ]
  in
  Arg.(value @@ flag doc)

let keep_stat_trace =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stat trace should be discarded are the end, after \
         the summary has been saved the disk."
      [ "keep-stat-trace" ]
  in
  Arg.(value @@ flag doc)

let empty_blobs =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the blobs added to the store should be the empty \
         string, during trace replay. This greatly increases the replay speed."
      [ "empty-blobs" ]
  in
  Arg.(value @@ flag doc)

let depth =
  let doc =
    Arg.info ~doc:"Depth of a commit's tree in chains-mode." [ "d"; "depth" ]
  in
  Arg.(value @@ opt int 1000 doc)

let nchain_trees =
  let doc =
    Arg.info ~doc:"Number of chain trees per commit in chains-mode."
      [ "c"; "nchain" ]
  in
  Arg.(value @@ opt int 1 doc)

let width =
  let doc =
    Arg.info ~doc:"Width of a commit's tree in large-mode." [ "w"; "width" ]
  in
  Arg.(value @@ opt int 1000000 doc)

let nlarge_trees =
  let doc =
    Arg.info ~doc:"Number of large trees per commit in large-mode."
      [ "l"; "nlarge" ]
  in
  Arg.(value @@ opt int 1 doc)

let commit_data_file =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Trace of Tezos operations to be replayed." []
  in
  Arg.(required @@ pos 0 (some string) None doc)

let artefacts_dir =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Destination of the bench artefacts."
      [ "artefacts" ]
  in
  Arg.(value @@ opt string default_artefacts_dir doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main_term =
  Term.(
    const main
    $ setup_log
    $ ncommits
    $ ncommits_trace
    $ mode
    $ inode_config
    $ store_type
    $ freeze_commit
    $ path_conversion
    $ depth
    $ width
    $ nchain_trees
    $ nlarge_trees
    $ commit_data_file
    $ artefacts_dir
    $ keep_store
    $ keep_stat_trace
    $ no_summary
    $ empty_blobs)

let () =
  let man =
    [
      `S "DESCRIPTION";
      `P
        "Benchmarks for tree operations. Requires traces of operations, \
         download them (`wget trace.repr`) from: ";
      `P
        "Trace with $(b,10310) commits \
         http://data.tarides.com/irmin/data4_10310commits.repr";
      `P
        "Trace with $(b,100066) commits \
         http://data.tarides.com/irmin/data4_100066commits.repr";
      `P
        "Trace with $(b,654941) commits \
         http://data.tarides.com/irmin/data4_654941commits.repr";
    ]
  in
  let info = Term.info ~man ~doc:"Benchmarks for tree operations" "tree" in
  Term.exit @@ Term.eval (main_term, info)
