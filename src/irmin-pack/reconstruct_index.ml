open! Import
module IO = IO.Unix

module Stats : sig
  type t

  val empty : unit -> t
  val add : t -> Pack_value.Kind.t -> unit
  val pp : t Fmt.t
end = struct
  open Pack_value.Kind

  type t = int array

  let empty () = Array.make 4 0
  let incr t n = t.(n) <- t.(n) + 1

  let add t = function
    | Contents -> incr t 0
    | Commit -> incr t 1
    | Node -> incr t 2
    | Inode -> incr t 3

  let pp =
    let open Fmt.Dump in
    record
      [
        field "Contents" (fun t -> t.(0)) Fmt.int;
        field "Commit" (fun t -> t.(1)) Fmt.int;
        field "Node" (fun t -> t.(2)) Fmt.int;
        field "Inode" (fun t -> t.(3)) Fmt.int;
      ]
end

module type Args = sig
  module Version : Version.S
  module Hash : Irmin.Hash.S
  module Index : Pack_index.S with type key := Hash.t
  module Inode : Inode.S with type key := Hash.t
  module Dict : Pack_dict.S
  module Contents : Pack_value.S
  module Commit : Pack_value.S
end

module Make (Args : Args) : sig
  val run : ?output:string -> Irmin.config -> unit
end = struct
  open Args

  let pp_key = Irmin.Type.pp Hash.t
  let decode_key = Irmin.Type.(unstage (decode_bin Hash.t))
  let decode_kind = Irmin.Type.(unstage (decode_bin Pack_value.Kind.t))

  (* [Repr] doesn't yet support buffered binary decoders, so we hack one
     together by re-interpreting [Invalid_argument _] exceptions from [Repr]
     as requests for more data. *)
  exception Not_enough_buffer

  type index_binding = { key : Hash.t; data : Index.value }

  let decode_entry_length = function
    | Pack_value.Kind.Contents -> Contents.decode_bin_length
    | Commit -> Commit.decode_bin_length
    | Node | Inode -> Inode.decode_bin_length

  let decode_entry_exn ~off ~buffer ~buffer_off =
    try
      (* Decode the key and kind by hand *)
      let off_after_key, key = decode_key buffer buffer_off in
      assert (off_after_key = buffer_off + Hash.hash_size);
      let off_after_kind, kind = decode_kind buffer off_after_key in
      assert (off_after_kind = buffer_off + Hash.hash_size + 1);
      (* Get the length of the entire entry *)
      let entry_len = decode_entry_length kind buffer buffer_off in
      { key; data = (off, entry_len, kind) }
    with
    | Invalid_argument msg when msg = "index out of bounds" ->
        raise Not_enough_buffer
    | Invalid_argument msg when msg = "String.blit / Bytes.blit_string" ->
        raise Not_enough_buffer

  let ingest_data_file ~progress ~total pack index =
    let buffer = ref (Bytes.create 1024) in
    let refill_buffer ~from =
      let read = IO.read pack ~off:from !buffer in
      assert (read = Bytes.length !buffer)
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
    let rec loop_entries ~buffer_off off =
      if off >= total then stats
      else
        let buffer_off, off =
          match
            decode_entry_exn ~off
              ~buffer:(Bytes.unsafe_to_string !buffer)
              ~buffer_off
          with
          | { key; data } ->
              let off', entry_len, kind = data in
              let entry_lenL = Int63.of_int entry_len in
              assert (off = off');
              Log.debug (fun l ->
                  l "k = %a (off, len, kind) = (%a, %d, %a)" pp_key key Int63.pp
                    off entry_len Pack_value.Kind.pp kind);
              Stats.add stats kind;
              Index.add index key data;
              progress entry_lenL;
              (buffer_off + entry_len, off ++ entry_lenL)
          | exception Not_enough_buffer ->
              let () =
                if buffer_off > 0 then
                  (* Try again with the value at the start of the buffer. *)
                  refill_buffer ~from:off
                else
                  (* The entire buffer isn't enough to hold this value: expand it. *)
                  expand_and_refill_buffer ~from:off
              in
              (0, off)
        in
        loop_entries ~buffer_off off
    in
    refill_buffer ~from:Int63.zero;
    loop_entries ~buffer_off:0 Int63.zero

  let run ?output config =
    if Conf.readonly config then raise S.RO_not_allowed;
    let run_duration = Mtime_clock.counter () in
    let root = Conf.root config in
    let dest = match output with Some path -> path | None -> root in
    let log_size = Conf.index_log_size config in
    Log.app (fun f ->
        f "Beginning index reconstruction with parameters: { log_size = %d }"
          log_size);
    let index = Index.v ~fresh:true ~readonly:false ~log_size dest in
    let pack_file = Filename.concat root "store.pack" in
    let pack =
      IO.v ~fresh:false ~readonly:true ~version:(Some Version.version) pack_file
    in
    let total = IO.offset pack in
    let bar, progress =
      Utils.Progress.counter ~total ~sampling_interval:100
        ~message:"Reconstructing index" ~pp_count:Utils.pp_bytes ()
    in
    let stats = ingest_data_file ~progress ~total pack index in
    Utils.Progress.finalise bar;
    (* Ensure that the log file is empty, so that subsequent opens with a
       smaller [log_size] don't immediately trigger a merge operation. *)
    Log.app (fun f ->
        f "Completed indexing of pack entries. Running a final merge ...");
    Index.try_merge index;
    Index.close index;
    IO.close pack;
    let run_duration = Mtime_clock.count run_duration in
    Log.app (fun f ->
        f "%a in %a. Store statistics:@,  @[<v 0>%a@]"
          Fmt.(styled `Green string)
          "Success" Mtime.Span.pp run_duration Stats.pp stats)
end
