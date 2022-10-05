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
include Dispatcher_intf
module Payload = Control_file.Latest_payload

(* The following [with module Io = Io.Unix] forces unix *)
module Make (Fm : File_manager.S with module Io = Io.Unix) :
  S with module Fm = Fm = struct
  module Fm = Fm
  module Io = Fm.Io
  module Suffix = Fm.Suffix
  module Mapping_file = Fm.Mapping_file
  module Errs = Fm.Errs
  module Control = Fm.Control

  type t = { fm : Fm.t }
  type location = Prefix | Suffix [@@deriving irmin]

  type accessor = { poff : int63; len : int; location : location }
  [@@deriving irmin]
  (** [poff] is a physical offset in a file. It is meant to be passed to [Io] or
      [Append_only]

      [len] is a number of bytes following [poff].

      [location] is a file identifier. *)

  let v fm =
    let t = { fm } in
    Ok t

  let get_prefix t =
    match Fm.prefix t.fm with
    | Some prefix -> prefix
    | None -> raise (Errors.Pack_error (`Invalid_prefix_read "no prefix found"))

  let get_mapping t =
    match Fm.mapping t.fm with
    | Some mapping -> mapping
    | None ->
        raise (Errors.Pack_error (`Invalid_mapping_read "no mapping found"))

  let suffix_start_offset t =
    let pl = Control.payload (Fm.control t.fm) in
    match pl.status with
    | Payload.From_v1_v2_post_upgrade _
    | From_v3_used_non_minimal_indexing_strategy | From_v3_no_gc_yet ->
        Int63.zero
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        assert false
    | From_v3_gced { suffix_start_offset; _ } -> suffix_start_offset

  (* The suffix only know the real offsets, it is in the dispatcher that global
     offsets are translated into real ones (i.e. in prefix or suffix offsets). *)
  let end_offset t =
    let open Int63.Syntax in
    Suffix.end_poff (Fm.suffix t.fm) + suffix_start_offset t

  module Suffix_arithmetic = struct
    (* Adjust the read in suffix, as the global offset [off] is
       [off] = [suffix_start_offset] + [suffix_offset]. *)
    let poff_of_off t off =
      let open Int63.Syntax in
      let suffix_start_offset = suffix_start_offset t in
      off - suffix_start_offset

    let off_of_poff t suffix_off =
      let open Int63.Syntax in
      let suffix_start_offset = suffix_start_offset t in
      suffix_off + suffix_start_offset
  end

  let offset_of_suffix_poff = Suffix_arithmetic.off_of_poff

  module Prefix_arithmetic = struct
    (* Find the last chunk which is before [off_start] (or at [off_start]). If no
       chunk found, then the entry was possibly gced (case 1). If [off_start] is
       after the entry's chunk then the entry was possibly gced (case 2). Note
       that for these two cases we cannot distinguished between trying to read a
       gced entry, or doing an invalid read. We expose two [read_exn] functions
       and we handled this upstream. *)
    let chunk_of_off_exn mapping off_start =
      (* NOTE off_start is a virtual offset *)
      let open Int63 in
      let open Int63.Syntax in
      let res = Mapping_file.find_nearest_leq mapping off_start in
      match res with
      | None ->
          (* Case 1: The entry if before the very first chunk (or there are no
             chunks). Possibly the entry was gced. *)
          let s =
            Fmt.str
              "offset %a is before the first chunk, or the prefix is empty"
              Int63.pp off_start
          in
          raise (Errors.Pack_error (`Invalid_read_of_gced_object s))
      | Some entry ->
          let chunk_off_start = entry.off in
          assert (chunk_off_start <= off_start);
          let chunk_len = entry.len in
          let chunk_off_end = chunk_off_start + of_int chunk_len in

          (* Case 2: The entry starts after the chunk. Possibly the entry was
             gced. *)
          (if chunk_off_end <= off_start then
           let s =
             Fmt.str
               "offset %a is supposed to be contained in chunk \
                (off=%a,poff=%a,len=%d) but starts after chunk"
               Int63.pp off_start Int63.pp chunk_off_start Int63.pp entry.poff
               entry.len
           in
           raise (Errors.Pack_error (`Invalid_read_of_gced_object s)));

          let shift_in_chunk = off_start - chunk_off_start in
          let max_entry_len = of_int chunk_len - shift_in_chunk in
          assert (max_entry_len >= Int63.zero);

          (entry, shift_in_chunk, max_entry_len)

    (* After we find the chunk of an entry, we check that a read is possible in the
       chunk. If it's not, this is always an invalid read. *)
    let poff_of_entry_exn mapping ~off ~len =
      let chunk, shift_in_chunk, max_entry_len = chunk_of_off_exn mapping off in

      (* Case 3: The entry ends after the chunk *)
      let open Int63 in
      let open Int63.Syntax in
      (if of_int len > max_entry_len then
       let s =
         Fmt.str
           "entry (off=%a, len=%d) is supposed to be contained in chunk \
            (poff=%a,len=%d) and starting at %a but is larger than it can be\n\
           \ contained in chunk" Int63.pp off len Int63.pp chunk.poff chunk.len
           Int63.pp shift_in_chunk
       in
       raise (Errors.Pack_error (`Invalid_prefix_read s)));

      (* Case 4: Success *)
      chunk.poff + shift_in_chunk
  end

  module Accessor = struct
    let v_in_suffix_exn t ~off ~len =
      let open Int63.Syntax in
      let entry_end_offset = off + Int63.of_int len in
      if entry_end_offset > end_offset t then
        raise (Errors.Pack_error `Read_out_of_bounds)
      else
        let poff = Suffix_arithmetic.poff_of_off t off in
        { poff; len; location = Suffix }

    let v_in_prefix_exn t ~off ~len =
      let poff =
        Prefix_arithmetic.poff_of_entry_exn (get_mapping t) ~off ~len
      in
      { poff; len; location = Prefix }

    let v_exn t ~off ~len =
      let open Int63.Syntax in
      let suffix_start_offset = suffix_start_offset t in
      if off >= suffix_start_offset then v_in_suffix_exn t ~off ~len
      else v_in_prefix_exn t ~off ~len

    let v_range_in_suffix_exn t ~off ~min_len ~max_len =
      let min_len = Int63.of_int min_len in
      let max_len = Int63.of_int max_len in
      let len =
        let open Int63.Syntax in
        let bytes_after_off = end_offset t - off in
        if bytes_after_off < min_len then
          raise (Errors.Pack_error `Read_out_of_bounds)
        else if bytes_after_off > max_len then max_len
        else bytes_after_off
      in
      let poff = Suffix_arithmetic.poff_of_off t off in
      { poff; len = Int63.to_int len; location = Suffix }

    let v_range_in_prefix_exn t ~off ~min_len ~max_len =
      let mapping = get_mapping t in
      let chunk, shift_in_chunk, max_entry_len =
        Prefix_arithmetic.chunk_of_off_exn mapping off
      in
      let open Int63 in
      let open Int63.Syntax in
      let len =
        let min_len = of_int min_len in
        let max_len = of_int max_len in
        if max_entry_len < min_len then
          raise (Errors.Pack_error `Read_out_of_bounds)
        else if max_entry_len > max_len then max_len
        else max_entry_len
      in
      let poff = chunk.poff + shift_in_chunk in
      let len = Int63.to_int len in
      { poff; len; location = Prefix }

    let v_range_exn t ~off ~min_len ~max_len =
      let open Int63.Syntax in
      let suffix_start_offset = suffix_start_offset t in
      if off >= suffix_start_offset then
        v_range_in_suffix_exn t ~off ~min_len ~max_len
      else v_range_in_prefix_exn t ~off ~min_len ~max_len
  end

  let read_exn t { poff; len; location } buf =
    match location with
    | Prefix -> Io.read_exn (get_prefix t) ~off:poff ~len buf
    | Suffix -> Suffix.read_exn (Fm.suffix t.fm) ~off:poff ~len buf

  let read_in_prefix_and_suffix_exn t ~off ~len buf =
    let ( -- ) a b = a - b in
    let open Int63.Syntax in
    let suffix_start_offset = suffix_start_offset t in
    if off < suffix_start_offset && off + Int63.of_int len > suffix_start_offset
    then (
      let read_in_prefix = suffix_start_offset - off |> Int63.to_int in
      let accessor = Accessor.v_exn t ~off ~len:read_in_prefix in
      read_exn t accessor buf;
      let read_in_suffix = len -- read_in_prefix in
      let buf_suffix = Bytes.create read_in_suffix in
      let accessor =
        Accessor.v_exn t ~off:suffix_start_offset ~len:read_in_suffix
      in
      read_exn t accessor buf_suffix;
      Bytes.blit buf_suffix 0 buf read_in_prefix read_in_suffix)
    else read_exn t (Accessor.v_exn t ~off ~len) buf

  let create_accessor_exn = Accessor.v_exn
  let create_accessor_from_range_exn = Accessor.v_range_exn

  let shrink_accessor_exn a ~new_len =
    if new_len > a.len then failwith "shrink_accessor_exn to larger accessor";
    { a with len = new_len }

  let create_sequential_accessor_exn location rem_len ~poff ~len =
    if len > rem_len then raise (Errors.Pack_error `Read_out_of_bounds)
    else { poff; len; location }

  let create_sequential_accessor_from_range_exn location rem_len ~poff ~min_len
      ~max_len =
    let len =
      if rem_len < min_len then raise (Errors.Pack_error `Read_out_of_bounds)
      else if rem_len > max_len then max_len
      else rem_len
    in
    { poff; len; location }

  let create_sequential_accessor_seq t ~min_header_len ~max_header_len ~read_len
      =
    let preffix_chunks =
      match Fm.mapping t.fm with
      | Some mapping ->
          let preffix_chunks = ref [] in
          Mapping_file.iter mapping (fun ~off ~len ->
              preffix_chunks := (off, len) :: !preffix_chunks)
          |> Errs.raise_if_error;
          List.rev !preffix_chunks
      | None -> []
    in
    let suffix_end_poff = Fm.Suffix.end_poff (Fm.suffix t.fm) in
    let suffix_start_offset = suffix_start_offset t in
    let get_entry_accessor rem_len location poff =
      let accessor =
        create_sequential_accessor_from_range_exn location rem_len ~poff
          ~min_len:min_header_len ~max_len:max_header_len
      in
      let buf = Bytes.create max_header_len in
      read_exn t accessor buf;
      let entry_len = read_len buf in
      ( entry_len,
        create_sequential_accessor_exn location rem_len ~poff ~len:entry_len )
    in
    let rec suffix_accessors poff () =
      let open Seq in
      let open Int63.Syntax in
      if poff >= suffix_end_poff then Nil
      else
        let rem_len = Int63.to_int (suffix_end_poff - poff) in
        let entry_len, accessor = get_entry_accessor rem_len Suffix poff in
        let r = (suffix_start_offset + poff, accessor) in
        let poff = poff + Int63.of_int entry_len in
        let f = suffix_accessors poff in
        Cons (r, f)
    in
    let rec prefix_accessors poff acc () =
      let open Seq in
      match acc with
      | [] -> suffix_accessors Int63.zero ()
      | (off, rem_len) :: acc ->
          if rem_len <= 0 then prefix_accessors poff acc ()
          else
            let entry_len, accessor = get_entry_accessor rem_len Prefix poff in
            let r = (off, accessor) in
            let rem_len = rem_len - entry_len in
            let open Int63.Syntax in
            let poff = poff + Int63.of_int entry_len in
            let off = off + Int63.of_int entry_len in
            let f = prefix_accessors poff ((off, rem_len) :: acc) in
            Cons (r, f)
    in
    prefix_accessors Int63.zero preffix_chunks
end
