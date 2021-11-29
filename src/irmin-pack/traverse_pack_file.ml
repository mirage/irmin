open! Import
module IO = IO.Unix

module Stats : sig
  type t

  val empty : unit -> t
  val add : t -> Pack_value.Kind.t -> unit
  val duplicate_entry : t -> unit
  val missing_hash : t -> unit
  val pp : t Fmt.t
end = struct
  open Pack_value.Kind

  type t = {
    pack_values : int array;
    mutable duplicates : int;
    mutable missing_hashes : int;
  }

  let empty () =
    let total_kinds = 7 (* TODO: expose this from [Pack_value.Kind] *) in
    let pack_values = Array.make total_kinds 0 in
    { pack_values; duplicates = 0; missing_hashes = 0 }

  let incr t n = t.pack_values.(n) <- t.pack_values.(n) + 1

  let add t = function
    | Contents -> incr t 0
    | Commit_v0 -> incr t 1
    | Commit_v1 -> incr t 2
    | Inode_v0_stable -> incr t 3
    | Inode_v0_unstable -> incr t 4
    | Inode_v1_root -> incr t 5
    | Inode_v1_nonroot -> incr t 6

  let duplicate_entry t = t.duplicates <- t.duplicates + 1
  let missing_hash t = t.missing_hashes <- t.missing_hashes + 1

  let pp =
    let open Fmt.Dump in
    record
      [
        field "Contents" (fun t -> t.pack_values.(0)) Fmt.int;
        field "Commit_v0" (fun t -> t.pack_values.(1)) Fmt.int;
        field "Commit_v1" (fun t -> t.pack_values.(2)) Fmt.int;
        field "Inode_v0_stable" (fun t -> t.pack_values.(3)) Fmt.int;
        field "Inode_v0_unstable" (fun t -> t.pack_values.(4)) Fmt.int;
        field "Inode_v1_root" (fun t -> t.pack_values.(5)) Fmt.int;
        field "Inode_v1_nonroot" (fun t -> t.pack_values.(6)) Fmt.int;
        field "Duplicated entries" (fun t -> t.duplicates) Fmt.int;
        field "Missing entries" (fun t -> t.missing_hashes) Fmt.int;
      ]
end

module type Args = sig
  module Version : Version.S
  module Hash : Irmin.Hash.S
  module Index : Pack_index.S with type key := Hash.t
  module Inode : Inode.S with type hash := Hash.t
  module Dict : Pack_dict.S
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
end = struct
  open Args

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
    Fmt.pf ppf "@[<v 0>%s with hash %a@,pack offset = %a, length = %d@]"
      (match kind with
      | Pack_value.Kind.Contents -> "Contents"
      | Commit_v0 -> "Commit_v0"
      | Commit_v1 -> "Commit_v1"
      | Inode_v0_stable -> "Inode_v0_stable"
      | Inode_v0_unstable -> "Inode_v0_unstable"
      | Inode_v1_root -> "Inode_v1_root"
      | Inode_v1_nonroot -> "Inode_v1_nonroot")
      pp_key x.key Int63.pp off len

  module Index_reconstructor = struct
    type t = {
      index : Index.t;
      indexing_strategy : Pack_store.Indexing_strategy.t;
    }

    let create ~dest config =
      let dest =
        match dest with
        | `Output path ->
            if IO.exists path then
              Fmt.invalid_arg "Can't reconstruct index. File already exits.";
            path
        | `In_place ->
            if Conf.readonly config then raise S.RO_not_allowed;
            Conf.root config
      in
      let log_size = Conf.index_log_size config
      and indexing_strategy = Conf.indexing_strategy config in
      [%log.app
        "Beginning index reconstruction with parameters: { log_size = %d }"
          log_size];
      let index = Index.v ~fresh:true ~readonly:false ~log_size dest in
      { index; indexing_strategy }

    let iter_pack_entry t key ((_off, len, kind) as data) =
      if t.indexing_strategy ~value_length:len kind then
        Index.add t.index key data;
      Ok ()

    let finalise t () =
      (* Ensure that the log file is empty, so that subsequent opens with a
         smaller [log_size] don't immediately trigger a merge operation. *)
      [%log.app "Completed indexing of pack entries. Running a final merge ..."];
      Index.try_merge t.index;
      Index.close t.index
  end

  module Index_checker = struct
    let create config =
      let log_size = Conf.index_log_size config in
      [%log.app
        "Beginning index checking with parameters: { log_size = %d }" log_size];
      let index =
        Index.v ~fresh:false ~readonly:true ~log_size (Conf.root config)
      in
      (index, ref 0)

    let iter_pack_entry (index, idx_ref) key data =
      match Index.find index key with
      | None ->
          Error (`Missing_hash { idx_pack = !idx_ref; binding = { key; data } })
      | Some data' when not @@ equal_index_value data data' ->
          Error `Inconsistent_entry
      | Some _ ->
          incr idx_ref;
          Ok ()

    let finalise (index, _) () = Index.close index
  end

  module Index_check_and_fix = struct
    let create config =
      let log_size = Conf.index_log_size config in
      [%log.app
        "Beginning index checking with parameters: { log_size = %d }" log_size];
      let root = Conf.root config in
      let index = Index.v ~fresh:false ~readonly:false ~log_size root in
      (index, ref 0)

    let iter_pack_entry (index, idx_ref) key data =
      match Index.find index key with
      | None ->
          Index.add index key data;
          Error (`Missing_hash { idx_pack = !idx_ref; binding = { key; data } })
      | Some data' when not @@ equal_index_value data data' ->
          Error `Inconsistent_entry
      | Some _ ->
          incr idx_ref;
          Ok ()

    let finalise (index, _) () =
      [%log.app "Completed indexing of pack entries. Running a final merge ..."];
      Index.try_merge index;
      Index.close index
  end

  let decode_entry_length = function
    | Pack_value.Kind.Contents -> Contents.decode_bin_length
    | Commit_v0 | Commit_v1 -> Commit.decode_bin_length
    | Inode_v0_stable | Inode_v0_unstable | Inode_v1_root | Inode_v1_nonroot ->
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

  let ingest_data_file ~progress ~total pack iter_pack_entry =
    let buffer = ref (Bytes.create 1024) in
    let refill_buffer ~from =
      let read = IO.read pack ~off:from !buffer in
      let filled = read = Bytes.length !buffer in
      let eof = Int63.equal total (Int63.add from (Int63.of_int read)) in
      if (not filled) && not eof then
        Fmt.failwith
          "When refilling from offset %#Ld (total %#Ld), read %#d but expected \
           %#d"
          (Int63.to_int64 from) (Int63.to_int64 total) read
          (Bytes.length !buffer)
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
            decode_entry_exn ~off
              ~buffer:(Bytes.unsafe_to_string !buffer)
              ~buffer_off
          with
          | { key; data } ->
              let off', entry_len, kind = data in
              let entry_lenL = Int63.of_int entry_len in
              assert (off = off');
              [%log.debug
                "k = %a (off, len, kind) = (%a, %d, %a)" pp_key key Int63.pp off
                  entry_len Pack_value.Kind.pp kind];
              Stats.add stats kind;
              let missing_hash =
                match iter_pack_entry key data with
                | Ok () -> Option.map Fun.id missing_hash
                | Error `Inconsistent_entry ->
                    Stats.duplicate_entry stats;
                    Option.map Fun.id missing_hash
                | Error (`Missing_hash x) ->
                    Stats.missing_hash stats;
                    Some x
              in
              progress entry_lenL;
              (buffer_off + entry_len, off ++ entry_lenL, missing_hash)
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

  let run mode config =
    let iter_pack_entry, finalise, message =
      match mode with
      | `Reconstruct_index dest ->
          let open Index_reconstructor in
          let v = create ~dest config in
          (iter_pack_entry v, finalise v, "Reconstructing index")
      | `Check_index ->
          let open Index_checker in
          let v = create config in
          (iter_pack_entry v, finalise v, "Checking index")
      | `Check_and_fix_index ->
          let open Index_check_and_fix in
          let v = create config in
          (iter_pack_entry v, finalise v, "Checking and fixing index")
    in
    let run_duration = Mtime_clock.counter () in
    let root = Conf.root config in
    let pack_file = Filename.concat root "store.pack" in
    let pack =
      IO.v ~fresh:false ~readonly:true ~version:(Some Version.version) pack_file
    in
    let total = IO.offset pack in
    let bar, progress =
      Utils.Progress.counter ~total ~sampling_interval:100 ~message
        ~pp_count:Utils.pp_bytes ()
    in
    let stats, missing_hash =
      ingest_data_file ~progress ~total pack iter_pack_entry
    in
    Utils.Progress.finalise bar;
    finalise ();
    IO.close pack;
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
end
