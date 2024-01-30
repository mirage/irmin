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

module Config = struct
  type _ return_type =
    | Unit : unit return_type
    | Summary : Trace_stat_summary.t return_type

  type 'a config = {
    number_of_commits_to_replay : int;
    path_conversion : [ `None | `V1 | `V0_and_v1 | `V0 ];
    inode_config : int * int;
    store_type : [ `Pack | `Pack_layered | `Pack_mem ];
    replay_trace_path : string;
    artefacts_path : string;
    keep_store : bool;
    keep_stat_trace : bool;
    empty_blobs : bool;
    return_type : 'a return_type;
    gc_every : int;
    gc_distance_in_the_past : int;
    gc_wait_after : int;
    add_volume_every : int;
  }
  (** Replay configuration

      [replay_trace_path] points to a specific file that describes the sequence
      of operations to replay. You may download one of the following URLs. The
      smaller ones are prefix of the larger ones.

      - http://data.tarides.com/irmin/data4_10310commits.repr (0.3GB)
      - http://data.tarides.com/irmin/data4_100066commits.repr (2.9GB)
      - http://data.tarides.com/irmin/data_1343496commits.repr (102GB)

      [number_of_commits_to_replay] is the wished number of commits to replay.
      If the value is too high, the replay will stop when reaching the end of
      [replay_trace_path]. Pick a number of commits depending on the wished
      runtime. Here are some reference runtimes that were true for irmin 3.0:

      - [60_457] commits take 3 minutes
      - [500_000] commits take 1 hour
      - [1_343_496] commits take 5 hours

      [artefacts_path] is the destination for the stats trace and the store. If
      both [keep_store] and [keep_stat_trace] are false, the destination will be
      emptied at the end of the replay.

      [path_conversion] is the strategy for shortening the paths while
      replaying. Was useful when benchmarking irmin on flattened Tezos paths.

      [empty_blobs] make the replay to push the empty string as in all the
      blobs, instead of their actual value read in the trace.

      [inode_config] is a pair of ints that will be stored in the results of the
      replay.

      A GC is triggered every [gc_every] commits. When GC is triggered, we
      select a previous commit that is [gc_distance_in_the_past] commits away
      from the current head commit.

      The first GC will be started after [gc_distance_in_the_past + 1] commits
      were replayed. [gc_distance_in_the_past] only makes sense if [gc_every] is
      not [0].

      [gc_wait_after] defines how many commits separate the start of a GC and
      the moment we block to wait for it to finish. [0] means that we will only
      block when the next gc starts or at the end of the replay. This parameter
      only makes sense if [gc_every] is not [0]. *)
end

module type Config = module type of Config

include Config

module type Store = sig
  type store_config
  type key

  include
    Irmin.Generic_key.KV
      with type Schema.Contents.t = bytes
       and type commit_key = key
       and type node_key = key
       and type contents_key = key

  type on_commit := int -> Hash.t -> unit
  type on_end := unit -> unit

  val create_repo :
    sw:Eio.Switch.t ->
    root:string ->
    store_config ->
    Repo.t * on_commit * on_end

  val split : repo -> unit
  val add_volume : repo -> unit
  val gc_wait : repo -> unit

  type stats := Irmin_pack_unix.Stats.Latest_gc.stats

  val gc_run :
    ?finished:((stats, string) result -> unit) -> repo -> commit_key -> unit
end

module type Sigs = sig
  include
    Config
      with type 'a return_type = 'a return_type
       and type 'a config = 'a config

  module type Store = Store

  module Make (Store : Store) : sig
    include
      Config
        with type 'a return_type = 'a return_type
         and type 'a config = 'a config

    val run : Store.store_config -> 'a config -> 'a
  end
end
