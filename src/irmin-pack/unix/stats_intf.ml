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

module type Sigs = sig
  module Pack_store : sig
    include module type of Pack_store with type t = Pack_store.t

    type stat

    val cache_misses : t -> int
    val export : stat -> t
  end

  module Index : sig
    include module type of Index

    type stat

    val export : stat -> t
  end

  module File_manager : sig
    include module type of File_manager

    type stat

    val export : stat -> t
  end

  type t = {
    pack_store : Pack_store.stat;
    index : Index.stat;
    file_manager : File_manager.stat;
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
end
