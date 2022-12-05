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

open Import

module Pack_store = struct
  type field =
    | Appended_hashes
    | Appended_offsets
    | Staging  (** Found in the store's write buffer. *)
    | Lru  (** Found in the store's LRU of recent [find] results. *)
    | Pack_direct
        (** Decoded directly from the pack file (via a direct key). *)
    | Pack_indexed
        (** Binding recovered from the pack file after first checking the index
            for its offset and length (via an indexed key). *)
    | Not_found  (** Find returned [None]. *)
  [@@deriving irmin]

  type t = {
    mutable appended_hashes : int;
    mutable appended_offsets : int;
    mutable total : int;
    mutable from_staging : int;
    mutable from_lru : int;
    mutable from_pack_direct : int;
    mutable from_pack_indexed : int;
  }
  [@@deriving irmin]
end

module Index = struct
  module S = Index.Stats

  type t = S.t = {
    mutable bytes_read : int;
    mutable nb_reads : int;
    mutable bytes_written : int;
    mutable nb_writes : int;
    mutable nb_merge : int;
    mutable merge_durations : float list;
    mutable nb_replace : int;
    mutable replace_durations : float list;
    mutable nb_sync : int;
    mutable time_sync : float;
    mutable lru_hits : int;
    mutable lru_misses : int;
  }
  [@@deriving irmin]
end

module File_manager = struct
  type field =
    | Dict_flushes
    | Suffix_flushes
    | Index_flushes
    | Auto_dict
    | Auto_suffix
    | Auto_index
    | Flush

  type t = {
    mutable dict_flushes : int;
    mutable suffix_flushes : int;
    mutable index_flushes : int;
    mutable auto_dict : int;
    mutable auto_suffix : int;
    mutable auto_index : int;
    mutable flush : int;
  }
  [@@deriving irmin]
end

module Latest_gc = struct
  type rusage = {
    maxrss : int64;
    minflt : int64;
    majflt : int64;
    inblock : int64;
    oublock : int64;
    nvcsw : int64;
    nivcsw : int64;
  }
  [@@deriving irmin]

  type ocaml_gc = {
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
  [@@deriving irmin]

  type duration = { wall : float; sys : float; user : float } [@@deriving irmin]
  (** Timings in seconds *)

  type step = {
    duration : duration;
    rusage : rusage;
    ocaml_gc : ocaml_gc;
    index : Index.t;
    pack_store : Pack_store.t;
    inode : Irmin_pack.Stats.Inode.t;
  }
  [@@deriving irmin]
  (** Stats gathered for one worker step *)

  type worker = {
    initial_maxrss : int64;
    initial_heap_words : int;
    initial_top_heap_words : int;
    initial_stack_size : int;
    steps : (string * step) list;
    files : (string * int63) list;
    objects_traversed : int63;
    suffix_transfers : int63 list;
  }
  [@@deriving irmin]
  (** Stats produced by the worker. They are meant to be transmited to the
      parent process through the gc result JSON file.

      [steps] is the list of all step names associated with the timing of these
      steps plus stats recorded at the end of the step. An association lists is
      used here instead of types because in the future the exact list of tasks
      may change and we don't want the rigidity of types, especially because
      these informations will end up appearing in a file format.

      Since the worker lives in a fork, [rusage] and [ocaml_gc] contain stats
      that are impacted by what happened in the main process, prior to the fork.
      The [init_*] fields reflect this.

      The total wall time of the worker is the sum of all [wall] fields in
      [steps].

      [files] contains the size of files created by the GC. And association list
      is used instead of plain types for the same reason as [steps]/

      [suffix_transfers] contains an int for each transfer loop to the new
      suffix. That integer corresponds to the number of bytes copied during that
      loop. The sum of these integers is equal to the "suffix" step in [files]. *)

  type stats = {
    generation : int;
    commit_offset : int63;
    before_suffix_start_offset : int63;
    before_suffix_end_offset : int63;
    after_suffix_start_offset : int63;
    after_suffix_end_offset : int63;
    steps : (string * duration) list;
    worker : worker;
  }
  [@@deriving irmin]
  (** All the stats for a single successful GC run.

      [commit_offset] is the offset of the commit provided by the irmin user.

      The [before_*] (and [after_*]) offsets track the state of the suffix
      before (and after) the GC.

      [steps] has a similar meaning as [steps] in [worker] but for the main
      process. *)

  type t = stats option [@@deriving irmin]
  (** [Latest_gc.t] is an [option] type because before the first gc end, there
      are no gc stats to expose. *)
end

module type Sigs = sig
  module Pack_store : sig
    include module type of Pack_store with type t = Pack_store.t

    type stat

    val cache_misses : t -> int
    val export : stat -> t
  end

  module Index : sig
    include module type of Index with type t = Index.t

    type stat

    val export : stat -> t
  end

  module File_manager : sig
    include module type of File_manager with type t = File_manager.t

    type stat

    val export : stat -> t
  end

  module Latest_gc : sig
    include
      module type of Latest_gc
        with type duration = Latest_gc.duration
        with type ocaml_gc = Latest_gc.ocaml_gc
        with type rusage = Latest_gc.rusage
        with type step = Latest_gc.step
        with type worker = Latest_gc.worker
        with type stats = Latest_gc.stats
        with type t = Latest_gc.t

    type stat

    val export : stat -> t
    val new_suffix_end_offset_before_finalise : worker -> int63

    val finalise_duration : stats -> float
    (** Time taken by the GC finalisation in seconds. It includes the time it
        took to wait for the worker to finish in case of [~wait:true]. *)

    val total_duration : stats -> float
    (** Total wall duration of the GC in seconds, from the call to GC and to the
        end of finalise. This duration contains the time it takes for the user
        to trigger a finalise while the worker is over. *)

    val finalise_suffix_transfer : stats -> int63
    (** [finalise_suffix_transfer stats] is the number of bytes appended to the
        new suffix by the finalise step of the GC. Before this, the worker
        already appended bytes to the new suffix, this is reported in
        [worker.suffix_transfers]. *)
  end

  type t = {
    pack_store : Pack_store.stat;
    index : Index.stat;
    file_manager : File_manager.stat;
    latest_gc : Latest_gc.stat;
  }
  (** Record type for all statistics that will be collected. There is a single
      instance (which we refer to as "the instance" below) which is returned by
      {!get}. *)

  val reset_stats : unit -> unit
  (** [reset_stats ()] will call the relevant [clear] function on each field of
      the instance. This typically resets the fields (e.g. to 0 for an int
      field). *)

  val get : unit -> t
  (** [get ()] returns the instance of {!t} that stores the satistics. If
      {!report_pack_store} or {!report_index} is not called before, the content
      will be filled with default value, decided at create time (most the time,
      [0]). *)

  val report_pack_store : field:Pack_store.field -> unit
  (** [report_pack_store ~field] increments the [field] value in the
      [pack_store] stats. It also increments the [total] field in
      {!Pack_store.t} when the field is related to [finds]. *)

  val report_index : unit -> unit
  (** [report_index ()] fills the [stats] with value from the {!Index.Stats}
      module. This essentially copies the "current" values from {!Index.Stats}
      to the [get()] instance [index] field. *)

  val incr_appended_hashes : unit -> unit
  (** [incr_appended_hashes ()] increments the field [appended_hashes] for
      [pack_store] in the instance. *)

  val incr_appended_offsets : unit -> unit
  (** [incr_appended_offsets] increments the field [appended_offsets] for
      [pack_store] in the instance. *)

  type cache_stats = { cache_misses : float }

  type offset_stats = { offset_ratio : float; offset_significance : int }
  (** [offset_ratio]: [appended_offsets / (appended_offsets + appended_hashes)];
      [offset_significance]: [appended_offsets + appended_hashes] *)

  val get_cache_stats : unit -> cache_stats
  (** [get_cache_stats()] uses the instance [pack_store] field to compute cache
      misses. *)

  val get_offset_stats : unit -> offset_stats
  (** [get_offset_stats()] uses the instance [pack_store] field to compute
      offset stats. *)

  val incr_fm_field : File_manager.field -> unit
  (** [incr_fm_field field] increments the chosen stats field for the
      {!File_manager} *)

  val report_latest_gc : Latest_gc.stats -> unit
  (** [report_latest_gc gc_stats] sets [(get ()).latest_gc] to the stats of the
      latest successful GC. *)
end
