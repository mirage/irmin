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
module Io = Io.Unix

module Stats : sig
  type t

  val empty : unit -> t
  val add : t -> Pack_value.Kind.t -> unit
  val duplicate_entry : t -> unit
  val missing_hash : t -> unit
  val pp : t Fmt.t
end = struct
  module Kind = Pack_value.Kind

  type t = {
    pack_values : int array;
    mutable duplicates : int;
    mutable missing_hashes : int;
  }

  let empty () =
    let pack_values = Array.make (List.length Kind.all) 0 in
    { pack_values; duplicates = 0; missing_hashes = 0 }

  let incr t n = t.pack_values.(n) <- t.pack_values.(n) + 1
  let add t k = incr t (Kind.to_enum k)
  let duplicate_entry t = t.duplicates <- t.duplicates + 1
  let missing_hash t = t.missing_hashes <- t.missing_hashes + 1

  let pp =
    let open Fmt.Dump in
    let pack_values =
      ListLabels.map Kind.all ~f:(fun k ->
          let name = Fmt.str "%a" Kind.pp k in
          let index = Kind.to_enum k in
          field name (fun t -> t.pack_values.(index)) Fmt.int)
    in
    record
      (pack_values
      @ [
          field "Duplicated entries" (fun t -> t.duplicates) Fmt.int;
          field "Missing entries" (fun t -> t.missing_hashes) Fmt.int;
        ])
end

module type Args = sig
  module File_manager : File_manager.S
  module Dispatcher : Dispatcher.S with module Fm = File_manager
  module Hash : Irmin.Hash.S
  module Index : Pack_index.S with type key := Hash.t
  module Inode : Inode.S with type hash := Hash.t
  module Contents : Pack_value.S
  module Commit : Pack_value.S
end

module Make (Args : Args) : sig
  val run :
    [ `Reconstruct_index of [ `In_place | `Output of string ]
    | `Check_index
    | `Check_and_fix_index ] ->
    Irmin.config ->
    unit

  val test :
    [ `Reconstruct_index of [ `In_place | `Output of string ]
    | `Check_index
    | `Check_and_fix_index ] ->
    Irmin.config ->
    unit
end = struct
  open Args
  module Errs = Io_errors.Make (File_manager.Io)

  let pp_key = Irmin.Type.pp Hash.t
  let decode_key = Irmin.Type.(unstage (decode_bin Hash.t))
  let decode_kind = Irmin.Type.(unstage (decode_bin Pack_value.Kind.t))

  (* [Repr] doesn't yet support buffered binary decoders, so we hack one
     together by re-interpreting [Invalid_argument _] exceptions from [Repr]
     as requests for more data. *)
  exception Not_enough_buffer

  type index_value = int63 * int * Pack_value.Kind.t
  [@@deriving irmin ~equal ~pp]

  type index_binding = { key : Hash.t; data : index_value }
  type missing_hash = { idx_pack : int; binding : index_binding }

  let pp_binding ppf x =
    let off, len, kind = x.data in
    Fmt.pf ppf "@[<v 0>%a with hash %a@,pack offset = %a, length = %d@]"
      Pack_value.Kind.pp kind pp_key x.key Int63.pp off len

  let to_index = function
    | Pack_value.Kind.Commit_v2 | Commit_v1 | Dangling_parent_commit -> true
    | Contents | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root
    | Inode_v2_nonroot ->
        false

  module Index_reconstructor = struct
    let create ~dest config =
      let dest =
        match dest with
        | `Output path ->
            if Sys.file_exists path then
              Fmt.invalid_arg "Can't reconstruct index. File already exits.";
            path
        | `In_place ->
            if Conf.readonly config then raise Irmin_pack.RO_not_allowed;
            Conf.root config
      in
      let log_size = Conf.index_log_size config in
      [%log.app
        "Beginning index reconstruction with parameters: { log_size = %d }"
          log_size];
      let index = Index.v_exn ~fresh:true ~readonly:false ~log_size dest in
      index

    let iter_pack_entry ~always index key data =
      let _, _, kind = data in
      if always || to_index kind then Index.add index key data;
      Ok ()

    let finalise index () =
      (* Ensure that the log file is empty, so that subsequent opens with a
         smaller [log_size] don't immediately trigger a merge operation. *)
      [%log.app "Completed indexing of pack entries. Running a final merge ..."];
      Index.try_merge index;
      Index.close_exn index
  end

  module Index_checker = struct
    let create config =
      let log_size = Conf.index_log_size config in
      [%log.app
        "Beginning index checking with parameters: { log_size = %d }" log_size];
      let index =
        Index.v_exn ~fresh:false ~readonly:true ~log_size (Conf.root config)
      in
      (index, ref 0)

    let iter_pack_entry ~always (index, idx_ref) key data =
      let _, _, kind = data in
      if always || to_index kind then (
        match Index.find index key with
        | None ->
            Error
              (`Missing_hash { idx_pack = !idx_ref; binding = { key; data } })
        | Some data' when not @@ equal_index_value data data' ->
            Error `Inconsistent_entry
        | Some _ ->
            incr idx_ref;
            Ok ())
      else Ok ()

    let finalise (index, _) () = Index.close_exn index
  end

  module Index_check_and_fix = struct
    let create config =
      let log_size = Conf.index_log_size config in
      [%log.app
        "Beginning index checking with parameters: { log_size = %d }" log_size];
      let root = Conf.root config in
      let index = Index.v_exn ~fresh:false ~readonly:false ~log_size root in
      (index, ref 0)

    let iter_pack_entry ~always (index, idx_ref) key data =
      let _, _, kind = data in
      if always || to_index kind then (
        match Index.find index key with
        | None ->
            Index.add index key data;
            Error
              (`Missing_hash { idx_pack = !idx_ref; binding = { key; data } })
        | Some data' when not @@ equal_index_value data data' ->
            Error `Inconsistent_entry
        | Some _ ->
            incr idx_ref;
            Ok ())
      else Ok ()

    let finalise (index, _) () =
      [%log.app "Completed indexing of pack entries. Running a final merge ..."];
      Index.try_merge index;
      Index.close_exn index
  end

  let decode_entry_length = function
    | Pack_value.Kind.Contents -> Contents.decode_bin_length
    | Commit_v1 | Commit_v2 | Dangling_parent_commit -> Commit.decode_bin_length
    | Inode_v1_stable | Inode_v1_unstable | Inode_v2_root | Inode_v2_nonroot ->
        Inode.decode_bin_length

  let decode_entry_exn ~off ~buffer ~buffer_off =
    try
      let buffer_pos = ref buffer_off in
      (* Decode the key and kind by hand *)
      let key = decode_key buffer buffer_pos in
      assert (!buffer_pos = buffer_off + Hash.hash_size);
      let kind = decode_kind buffer buffer_pos in
      assert (!buffer_pos = buffer_off + Hash.hash_size + 1);
      (* Get the length of the entire entry *)
      let entry_len = decode_entry_length kind buffer buffer_off in
      { key; data = (off, entry_len, kind) }
    with
    | Invalid_argument msg when msg = "index out of bounds" ->
        raise Not_enough_buffer
    | Invalid_argument msg when msg = "String.blit / Bytes.blit_string" ->
        raise Not_enough_buffer

  (* duplicated code with irmin-tezos-utils *)
  let decode_entry_len buffer =
    let buffer = Bytes.unsafe_to_string buffer in
    let i0 = 0 in
    let ilength = i0 + Hash.hash_size + 1 in
    let pos_ref = ref ilength in
    let suffix_length = Varint.decode_bin buffer pos_ref in
    let length_length = !pos_ref - ilength in
    Hash.hash_size + 1 + length_length + suffix_length

  let guess_entry_len dispatcher ~off =
    let min_bytes_needed_to_discover_length = Hash.hash_size + 1 in
    let max_bytes_needed_to_discover_length =
      min_bytes_needed_to_discover_length + Varint.max_encoded_size
    in
    let buffer = Bytes.create max_bytes_needed_to_discover_length in
    let _len, _volume =
      Dispatcher.read_range_exn dispatcher ~off
        ~min_len:min_bytes_needed_to_discover_length
        ~max_len:max_bytes_needed_to_discover_length buffer
    in
    decode_entry_len buffer

  (* Read at most [len], by checking that [(off, len)] don't go out of bounds of
     the suffix file. *)
  let io_read_at_most ~off ~len buffer dispatcher =
    let len =
      let open Int63.Syntax in
      let bytes_after_off = Dispatcher.end_offset dispatcher - off in
      if bytes_after_off < Int63.of_int len then Int63.to_int bytes_after_off
      else len
    in
    Dispatcher.read_exn dispatcher ~off ~len buffer

  let on_entry { data; key } stats iter_pack_entry missing_hash =
    [%log.debug "k = %a (off, len, kind) = %a" pp_key key pp_index_value data];
    match iter_pack_entry key data with
    | Ok () -> Option.map Fun.id missing_hash
    | Error `Inconsistent_entry ->
        Stats.duplicate_entry stats;
        Option.map Fun.id missing_hash
    | Error (`Missing_hash x) ->
        Stats.missing_hash stats;
        Some x

  let ingest_data_file_after_v3 ~initial_buffer_size ~progress dispatcher
      iter_pack_entry =
    let stats = Stats.empty () in
    let buffer = Bytes.create initial_buffer_size in
    let on_entry missing_hash off =
      let len = guess_entry_len dispatcher ~off in
      let _ = Dispatcher.read_exn dispatcher ~off ~len buffer in
      let { key; data } =
        decode_entry_exn ~off
          ~buffer:(Bytes.unsafe_to_string buffer)
          ~buffer_off:0
      in
      let off', entry_len, kind = data in
      let entry_lenL = Int63.of_int entry_len in
      assert (off = off');
      Stats.add stats kind;
      progress entry_lenL;
      (entry_len, on_entry { key; data } stats iter_pack_entry missing_hash)
    in
    let rec traverse off missing_hash =
      match Dispatcher.next_valid_offset dispatcher ~off with
      | None -> missing_hash
      | Some off ->
          let len, missing_hash = on_entry missing_hash off in
          let after_off =
            let open Int63.Syntax in
            off + Int63.of_int len
          in
          traverse after_off missing_hash
    in
    let missing_hash = traverse Int63.zero None in
    (stats, missing_hash)

  let ingest_data_file_before_v3 ~initial_buffer_size ~progress ~total
      dispatcher iter_pack_entry =
    let buffer = ref (Bytes.create initial_buffer_size) in
    let refill_buffer ~from =
      let buffer_len = Bytes.length !buffer in
      let _ = io_read_at_most ~off:from ~len:buffer_len !buffer dispatcher in
      ()
    in
    let expand_and_refill_buffer ~from =
      let length = Bytes.length !buffer in
      if length > 1_000_000_000 (* 1 GB *) then
        Fmt.failwith
          "Couldn't decode the value at offset %a in %d of buffer space. \
           Corrupted data file?"
          Int63.pp from length
      else (
        buffer := Bytes.create (2 * length);
        refill_buffer ~from)
    in
    let stats = Stats.empty () in
    let rec loop_entries ~buffer_off off missing_hash =
      if off >= total then (stats, missing_hash)
      else
        let buffer_off, off, missing_hash =
          match
            (* Bytes.unsafe_to_string usage: possibly safe, depending on details of
               implementation of decode_entry_exn TODO either justify clearly that this is
               safe, or change to use safe Bytes.to_string *)
            decode_entry_exn ~off
              ~buffer:(Bytes.unsafe_to_string !buffer)
              ~buffer_off
          with
          | { key; data } ->
              let off', entry_len, kind = data in
              let entry_lenL = Int63.of_int entry_len in
              assert (off = off');
              Stats.add stats kind;
              progress entry_lenL;
              let off = Int63.Syntax.(off + entry_lenL) in
              let missing_hash =
                on_entry { data; key } stats iter_pack_entry missing_hash
              in
              (buffer_off + entry_len, off, missing_hash)
          | exception Not_enough_buffer ->
              let () =
                if buffer_off > 0 then
                  (* Try again with the value at the start of the buffer. *)
                  refill_buffer ~from:off
                else
                  (* The entire buffer isn't enough to hold this value: expand it. *)
                  expand_and_refill_buffer ~from:off
              in
              (0, off, missing_hash)
        in
        loop_entries ~buffer_off off missing_hash
    in
    refill_buffer ~from:Int63.zero;
    loop_entries ~buffer_off:0 Int63.zero None

  let run_or_test ~initial_buffer_size mode config =
    let always =
      Conf.indexing_strategy config
      |> Irmin_pack.Indexing_strategy.is_minimal
      |> not
    in
    let iter_pack_entry, finalise, message =
      match mode with
      | `Reconstruct_index dest ->
          let open Index_reconstructor in
          let v = create ~dest config in
          (iter_pack_entry ~always v, finalise v, "Reconstructing index")
      | `Check_index ->
          let open Index_checker in
          let v = create config in
          (iter_pack_entry ~always v, finalise v, "Checking index")
      | `Check_and_fix_index ->
          let open Index_check_and_fix in
          let v = create config in
          (iter_pack_entry ~always v, finalise v, "Checking and fixing index")
    in
    let run_duration = Mtime_clock.counter () in
    let fm = File_manager.open_ro config |> Errs.raise_if_error in
    let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
    let total = Dispatcher.end_offset dispatcher in
    let ingest_data progress =
      if File_manager.gc_allowed fm then
        ingest_data_file_after_v3 ~initial_buffer_size dispatcher
          iter_pack_entry ~progress
      else
        ingest_data_file_before_v3 ~initial_buffer_size ~total dispatcher
          iter_pack_entry ~progress
    in
    let stats, missing_hash =
      let bar =
        let open Progress.Line.Using_int63 in
        list
          [ const message; bytes; elapsed (); bar total; percentage_of total ]
      in
      Progress.(with_reporter bar) ingest_data
    in
    finalise ();
    File_manager.close fm |> Errs.raise_if_error;
    let run_duration = Mtime_clock.count run_duration in
    let store_stats fmt =
      Fmt.pf fmt "Store statistics:@,  @[<v 0>%a@]" Stats.pp stats
    in
    match missing_hash with
    | None ->
        [%log.app
          "%a in %a. %t"
            Fmt.(styled `Green string)
            "Success" Mtime.Span.pp run_duration store_stats]
    | Some x ->
        let msg =
          match mode with
          | `Check_index -> "Detected missing entries"
          | `Check_and_fix_index ->
              "Detected missing entries and added them to index"
          | _ -> assert false
        in
        [%log.err
          "%a in %a.@,\
           First pack entry missing from index is the %d entry of the pack:@,\
          \  %a@,\
           %t"
            Fmt.(styled `Red string)
            msg Mtime.Span.pp run_duration x.idx_pack pp_binding x.binding
            store_stats]

  let run = run_or_test ~initial_buffer_size:1024
  let test = run_or_test ~initial_buffer_size:100
end
