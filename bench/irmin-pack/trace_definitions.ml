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

(** Traces file format definitions.

    This file is meant to be used from Tezos. OCaml version 4.09 and the 32bit
    architecture should be supported.

    {3 Traces Workflow}

    A Tezos node (may) output a [Raw_replayable_trace] file. Such a trace should
    be postprocessed to create a [Replayable_trace].

    A Tezos node (may) output a [Stat_trace] file.

    {e trace_stats.exe summarise} takes a [Stat_trace] file and summarises it to
    a {e stat_summary.json} file.

    A series of python script take a {e stat_summary.json} file and produce
    plots (e.g. png files).

    {e tree.exe} takes a [Replayable_trace] file, internally produces a
    [Stat_trace] file and yields it to [Trace_stat_summary] to produce a
    {e stat_summary.json} file. *)

(** [Replayable_trace], a trace of Tezos's interactions with Irmin.

    {3 Interleaved Contexts and Commits}

    All the recorded operations in Tezos operate on (and create new) immutable
    records of type [context]. Most of the time, everything is linear (i.e. the
    input context to an operation is the latest output context), but there
    sometimes are several parallel chains of contexts, where all but one will
    end up being discarded.

    Similarly to contexts, commits are not always linear, i.e. a checkout may
    choose a parent that is not the latest commit.

    To solve this conundrum when replaying the trace, we need to remember all
    the [context_id -> tree] and [trace commit hash -> real commit hash] pairs
    to make sure an operation is operating on the right parent.

    In the trace, the context indices and the commit hashes are 'scoped',
    meaning that they are tagged with a boolean information indicating if this
    is the very last occurence of that value in the trace. This way we can
    discard a recorded pair as soon as possible.

    In practice, there is only 1 context and 1 commit in history, and sometimes
    0 or 2, but the code is ready for more. *)
module Replayable_trace = struct
  module V0 = struct
    let version = 0

    type header = unit [@@deriving repr]
    type 'a scope = Forget of 'a | Keep of 'a [@@deriving repr]
    type key = string list [@@deriving repr]
    type hash = string [@@deriving repr]
    type message = string [@@deriving repr]
    type context_id = int64 [@@deriving repr]

    type add = {
      key : key;
      value : string;
      in_ctx_id : context_id scope;
      out_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type copy = {
      key_src : key;
      key_dst : key;
      in_ctx_id : context_id scope;
      out_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type commit = {
      hash : hash scope;
      date : int64;
      message : message;
      parents : hash scope list;
      in_ctx_id : context_id scope;
    }
    [@@deriving repr]

    type row =
      (* Operation(s) that create a context from none *)
      | Checkout of hash scope * context_id scope
      (* Operations that create a context from one *)
      | Add of add
      | Remove of key * context_id scope * context_id scope
      | Copy of copy
      (* Operations that just read a context *)
      | Find of key * bool * context_id scope
      | Mem of key * bool * context_id scope
      | Mem_tree of key * bool * context_id scope
      | Commit of commit
    [@@deriving repr]
  end

  module Latest = V0
  include Latest

  include Trace_common.Io (struct
    module Latest = Latest

    (** Irmin's Replayable Bootstrap Trace *)
    let magic = Trace_common.Magic.of_string "IrmRepBT"

    let get_version_converter = function
      | 0 ->
          Trace_common.Version_converter
            {
              header_t = V0.header_t;
              row_t = V0.row_t;
              upgrade_header = Fun.id;
              upgrade_row = Fun.id;
            }
      | i -> Fmt.invalid_arg "Unknown Replayable_trace version %d" i
  end)
end

(** Trace of a tezos node run, or a replay run.

    May be summarised to a JSON file.

    {3 Implicitly Auto-Upgradable File Format}

    The stat trace has these two properties:

    - It supports extensions, in order to change or add new stats in the future.
    - Old trace files from old versions are still readable.

    There are multiple reasons for wanting compatibility with old versions:

    - Because one of the goal of the benchmarks is to assess the evolution of
      performances across distant versions of irmin, we need stability in order
      to avoid recomputing everything every time.
    - When those traces will be produced by Tezos nodes, we have no control over
      the version of those traces.

    For this system to work, the "decoding shape" of a version of the stat trace
    shouldn't ever change (once fixed). The way the trace is built for a version
    should be stable too.

    To modify something in the definition or the collection: append a new
    version. *)
module Stat_trace = struct
  module V0 = struct
    let version = 0

    type float32 = int32 [@@deriving repr]

    type pack = {
      finds : int;
      cache_misses : int;
      appended_hashes : int;
      appended_offsets : int;
    }
    [@@deriving repr]
    (** Stats extracted from [Irmin_pack.Stats.get ()]. *)

    type tree = {
      contents_hash : int;
      contents_find : int;
      contents_add : int;
      node_hash : int;
      node_mem : int;
      node_add : int;
      node_find : int;
      node_val_v : int;
      node_val_find : int;
      node_val_list : int;
    }
    [@@deriving repr]
    (** Stats extracted from [Irmin.Tree.counters ()]. *)

    type index = {
      bytes_read : int;
      nb_reads : int;
      bytes_written : int;
      nb_writes : int;
      nb_merge : int;
      new_merge_durations : float list;
    }
    [@@deriving repr]
    (** Stats extracted from [Index.Stats.get ()].

        [new_merge_durations] is not just a mirror of
        [Index.Stats.merge_durations], it only contains the new entries since
        the last time it was recorded. This list is always empty when in the
        header. *)

    type gc = {
      minor_words : float;
      promoted_words : float;
      major_words : float;
      minor_collections : int;
      major_collections : int;
      heap_words : int;
      compactions : int;
      top_heap_words : int;
      stack_size : int;
    }
    [@@deriving repr]
    (** Stats extracted from [Gc.quick_stat ()]. *)

    type disk = {
      index_data : int64;
      index_log : int64;
      index_log_async : int64;
      store_dict : int64;
      store_pack : int64;
    }
    [@@deriving repr]
    (** Stats extracted from filesystem. Requires the path to the irmin store. *)

    type 'pack_stats bag_of_stats_base = {
      pack : 'pack_stats;
      tree : tree;
      index : index;
      gc : gc;
      disk : disk;
      timestamp_wall : float;
      timestamp_cpu : float;
    }
    [@@deriving repr]
    (** Melting pot of stats, recorded before and after every commits.

        They are necessary in order to compute any throughput analytics. *)

    type store_before = {
      nodes : int;
      leafs : int;
      skips : int;
      depth : int;
      width : int;
    }
    [@@deriving repr]
    (** Stats computed from the [tree] value passed to the commit operation,
        before the commit, when the tree still carries the modifications brought
        by the previous operations. *)

    type watched_node =
      [ `Contracts_index
      | `Big_maps_index
      | `Rolls_index
      | `Rolls_owner_current
      | `Commitments
      | `Contracts_index_ed25519
      | `Contracts_index_originated ]
    [@@deriving repr, enum]

    type store_after = { watched_nodes_length : int list } [@@deriving repr]
    (** Stats computed on the [tree] value passed to the commit operation, after
        the commit, when the inode has been reconstructed and that [Tree.length]
        is now innexpensive to perform. *)

    type 'pack_stats commit_base = {
      duration : float32;
      before : 'pack_stats bag_of_stats_base;
      after : 'pack_stats bag_of_stats_base;
      store_before : store_before;
      store_after : store_after;
    }
    [@@deriving repr]

    type 'pack_stats row_base =
      [ `Add of float32
      | `Remove of float32
      | `Find of float32
      | `Mem of float32
      | `Mem_tree of float32
      | `Checkout of float32
      | `Copy of float32
      | `Commit of 'pack_stats commit_base ]
    [@@deriving repr]

    type row = pack row_base [@@deriving repr]
    (** Stats gathered while running an operation.

        {3 Operation durations}

        For each operation we record its wall time length using a [float32], a
        [float16] would be suitable too (it has >3 digits of precision).

        {3 Time and disk performance considerations}

        On commit we record a lot of things, thankfuly the frequency is low:
        ~1/600. 599 small operations weigh ~3600 bytes, 1 commit weighs ~300
        bytes. The trace reaches 1GB after ~250k commits. *)

    type setup_play = unit [@@deriving repr]
    (** Informations gathered from the tezos node.

        Noting so far. Any ideas? *)

    type setup_replay = {
      path_conversion : [ `None | `V1 | `V0_and_v1 | `V0 ];
      artefacts_dir : string;
    }
    [@@deriving repr]
    (** Informations gathered from the tree.exe parameters. *)

    type config = {
      inode_config : int * int * int;
      store_type : [ `Pack | `Pack_layered | `Pack_mem ];
      setup : [ `Play of setup_play | `Replay of setup_replay ];
    }
    [@@deriving repr]

    type 'pack_stats header_base = {
      config : config;
      hostname : string;
      timeofday : float;
      word_size : int;
      initial_stats : 'pack_stats bag_of_stats_base;
    }
    [@@deriving repr]

    type header = pack header_base [@@deriving repr]
    (** File header.

        {3 Timestamps}

        [stats.timestamp_wall] and [stats.timestamp_cpu] are the starting points
        of the trace, they are to be substracted from their counterpart in
        [commit] to compute time spans.

        [timeofday] is the date and time at which the stats started to be
        accumulated.

        [stats.timestamp_wall] may originate from [Mtime_clock.now].

        [stats.timestamp_cpu] may originate from [Sys.time].

        It would be great to be able to record the library/sources versions. *)

    type commit = pack commit_base [@@deriving repr]
    type bag_of_stats = pack bag_of_stats_base [@@deriving repr]
  end

  module V1 = struct
    include V0

    let version = 1

    type finds = {
      total : int;
      from_staging : int;
      from_lru : int;
      from_pack_direct : int;
      from_pack_indexed : int;
    }
    [@@deriving repr]
    (** Stats extracted from [Irmin_pack.Stats.get ()]. *)

    type pack = {
      finds : finds;
      appended_hashes : int;
      appended_offsets : int;
      inode_add : int;
      inode_remove : int;
      inode_of_seq : int;
      inode_of_raw : int;
      inode_rec_add : int;
      inode_rec_remove : int;
      inode_to_binv : int;
      inode_decode_bin : int;
      inode_encode_bin : int;
    }
    [@@deriving repr]
    (** Stats extracted from [Irmin_pack.Stats.get ()]. *)

    type commit = pack commit_base [@@deriving repr]
    type bag_of_stats = pack bag_of_stats_base [@@deriving repr]
    type row = pack row_base [@@deriving repr]
    type header = pack header_base [@@deriving repr]

    (* [v0.cache_misses] is lost *)
    let v1pack_of_v0pack (v0 : V0.pack) : pack =
      {
        finds =
          {
            total = v0.finds;
            from_staging = 0;
            from_lru = 0;
            from_pack_direct = 0;
            from_pack_indexed = 0;
          };
        appended_hashes = v0.appended_hashes;
        appended_offsets = v0.appended_offsets;
        inode_add = 0;
        inode_remove = 0;
        inode_of_seq = 0;
        inode_of_raw = 0;
        inode_rec_add = 0;
        inode_rec_remove = 0;
        inode_to_binv = 0;
        inode_decode_bin = 0;
        inode_encode_bin = 0;
      }

    let v1bos_of_v0bos (v0 : V0.bag_of_stats) : bag_of_stats =
      {
        pack = v1pack_of_v0pack v0.pack;
        tree = v0.tree;
        index = v0.index;
        gc = v0.gc;
        disk = v0.disk;
        timestamp_wall = v0.timestamp_wall;
        timestamp_cpu = v0.timestamp_cpu;
      }

    let v1commit_of_v0commit (v0 : V0.commit) : commit =
      {
        duration = v0.duration;
        before = v1bos_of_v0bos v0.before;
        after = v1bos_of_v0bos v0.after;
        store_before = v0.store_before;
        store_after = v0.store_after;
      }

    let v1row_of_v0row (v0 : V0.row) : row =
      match v0 with
      | `Commit payload -> `Commit (v1commit_of_v0commit payload)
      | ( `Add _ | `Remove _ | `Find _ | `Mem _ | `Mem_tree _ | `Checkout _
        | `Copy _ ) as v0 ->
          v0

    let v1header_of_v0header (v0 : V0.header) : header =
      {
        config = v0.config;
        hostname = v0.hostname;
        timeofday = v0.timeofday;
        word_size = v0.word_size;
        initial_stats = v1bos_of_v0bos v0.initial_stats;
      }
  end

  module Latest = V1
  include Latest

  let watched_nodes : watched_node list =
    List.init (max_watched_node + 1) (fun i ->
        watched_node_of_enum i |> Option.get)

  let step_list_per_watched_node =
    let aux = function
      | `Contracts_index -> [ "data"; "contracts"; "index" ]
      | `Big_maps_index -> [ "data"; "big_maps"; "index" ]
      | `Rolls_index -> [ "data"; "rolls"; "index" ]
      | `Rolls_owner_current -> [ "data"; "rolls"; "owner"; "current" ]
      | `Commitments -> [ "data"; "commitments" ]
      | `Lol -> []
      | `Contracts_index_ed25519 -> [ "data"; "contracts"; "index"; "ed25519" ]
      | `Contracts_index_originated ->
          [ "data"; "contracts"; "index"; "originated" ]
    in
    List.combine watched_nodes (List.map aux watched_nodes)

  let path_per_watched_node =
    List.map
      (fun (k, l) -> (k, "/" ^ String.concat "/" l))
      step_list_per_watched_node

  include Trace_common.Io (struct
    module Latest = Latest

    (** Irmin's Stats Bootstrap Trace *)
    let magic = Trace_common.Magic.of_string "IrmStaBT"

    let get_version_converter = function
      | 0 ->
          Trace_common.Version_converter
            {
              header_t = V0.header_t;
              row_t = V0.row_t;
              upgrade_header = v1header_of_v0header;
              upgrade_row = v1row_of_v0row;
            }
      | 1 ->
          Trace_common.Version_converter
            {
              header_t = V1.header_t;
              row_t = V1.row_t;
              upgrade_header = Fun.id;
              upgrade_row = Fun.id;
            }
      | i -> Fmt.invalid_arg "Unknown Stat_trace version %d" i
  end)
end
