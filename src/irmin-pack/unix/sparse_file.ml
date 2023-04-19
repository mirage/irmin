(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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
include Sparse_file_intf
module BigArr1 = Bigarray.Array1

type int64_bigarray = (int64, Bigarray.int64_elt, Bigarray.c_layout) BigArr1.t

module Int64_mmap (Io : Io.S) : sig
  type t

  val open_ro : fn:string -> sz:int -> (t, [> Io.open_error ]) result
  val length : t -> int
  val get : t -> int -> Int64.t
  val close : t -> (unit, [> Io.close_error ]) result
end = struct
  type t = {
    fn : string;
    fd : Io.t;
    loaded : bool array;
    mutable arr : int64_bigarray;
  }

  let sector_size = 512
  let length t = BigArr1.dim t.arr

  let open_ro ~fn ~sz =
    let open Result_syntax in
    assert (Sys.file_exists fn);
    let+ fd = Io.open_ ~path:fn ~readonly:true in
    let size = sz / 8 in
    let arr = BigArr1.create Bigarray.Int64 Bigarray.c_layout size in
    let loaded = Array.make (1 + (sz / sector_size)) false in
    { fn; fd; arr; loaded }

  let close t = Io.close t.fd

  let load t sector_id =
    if not t.loaded.(sector_id) then (
      let sector_start = sector_id * sector_size in
      let nb = min sector_size (length t - sector_start) in
      let len = 8 * nb in
      let bytes = Bytes.create len in
      Io.read_exn t.fd ~off:(Int63.of_int (8 * sector_start)) ~len bytes;
      for i = 0 to nb - 1 do
        t.arr.{sector_start + i} <- Bytes.get_int64_le bytes (8 * i)
      done;
      t.loaded.(sector_id) <- true)

  let ensure_loaded t i =
    let sector_id = i / sector_size in
    if not t.loaded.(sector_id) then load t sector_id

  let get t i =
    ensure_loaded t i;
    t.arr.{i}
end

module Make (Io : Io.S) = struct
  module Io = Io
  module Errs = Io_errors.Make (Io)

  module Mapping_file = struct
    module Int64_mmap = Int64_mmap (Io)

    let ( .%{} ) = Int64_mmap.get

    type t = Int64_mmap.t

    let open_map ~path ~size =
      match Io.classify_path path with
      | `File ->
          let open Result_syntax in
          let* mmap = Int64_mmap.open_ro ~fn:path ~sz:size in
          if Int64_mmap.length mmap mod 3 = 0 then Ok mmap
          else
            Error
              (`Corrupted_mapping_file
                (__FILE__ ^ ": mapping mmap size did not meet size requirements"))
      | _ -> Error (`No_such_file_or_directory path)

    let close = Int64_mmap.close
    let entry_count t = Int64_mmap.length t / 3
    let entry_idx i = i * 3
    let entry_off t i = t.%{entry_idx i} |> Int63.of_int64
    let entry_poff t i = t.%{entry_idx i + 1} |> Int63.of_int64
    let entry_len t i = t.%{entry_idx i + 2} |> Int64.to_int

    let iter_exn t f =
      for i = 0 to entry_count t - 1 do
        f ~off:(entry_off t i) ~len:(entry_len t i)
      done

    let iter t f = Errs.catch (fun () -> iter_exn t f)

    type entry = { off : int63; poff : int63; len : int }

    let find_nearest_geq arr off =
      let get arr i =
        let start = arr.%{entry_idx i} |> Int64.to_int in
        let len = entry_len arr i in
        start + len - 1
      in
      match
        Utils.nearest_geq ~arr ~get ~lo:0
          ~hi:(entry_count arr - 1)
          ~key:(Int63.to_int off)
      with
      | None -> `After
      | Some i ->
          let entry =
            {
              off = entry_off arr i;
              poff = entry_poff arr i;
              len = entry_len arr i;
            }
          in
          if i == 0 && entry.off > off then `Before entry else `Inside entry
  end

  type t = { mapping : Mapping_file.t; data : Io.t }

  let open_ ~readonly ~mapping_size ~mapping ~data =
    let open Result_syntax in
    let* mapping = Mapping_file.open_map ~path:mapping ~size:mapping_size in
    let+ data = Io.open_ ~path:data ~readonly in
    { mapping; data }

  let open_ro ~mapping_size ~mapping ~data =
    open_ ~readonly:true ~mapping_size ~mapping ~data

  let close t =
    let open Result_syntax in
    let* () = Mapping_file.close t.mapping in
    Io.close t.data

  let iter t fn = Mapping_file.iter t.mapping fn

  let get_poff { mapping; _ } ~off =
    match Mapping_file.find_nearest_geq mapping off with
    | `After -> raise (Errors.Pack_error (`Invalid_sparse_read (`After, off)))
    | `Before _ ->
        raise (Errors.Pack_error (`Invalid_sparse_read (`Before, off)))
    | `Inside entry when entry.off > off ->
        raise (Errors.Pack_error (`Invalid_sparse_read (`Hole, off)))
    | `Inside entry ->
        let open Int63.Syntax in
        let shift_in_entry = off - entry.off in
        let max_entry_len = Int63.of_int entry.len - shift_in_entry in
        let poff = entry.poff + off - entry.off in
        (poff, Int63.to_int max_entry_len)

  let read_exn t ~off ~len buf =
    let poff, max_entry_len = get_poff t ~off in
    if max_entry_len < len then raise (Errors.Pack_error `Read_out_of_bounds);
    Io.read_exn t.data ~off:poff ~len buf

  let read_range_exn t ~off ~min_len ~max_len buf =
    [%log.debug
      "read_range_exn ~off:%a ~min_len:%i ~max_len:%i" Int63.pp off min_len
        max_len];
    let poff, max_entry_len = get_poff t ~off in
    if max_entry_len < min_len then
      raise (Errors.Pack_error `Read_out_of_bounds);
    let len = min max_len max_entry_len in
    Io.read_exn t.data ~off:poff ~len buf;
    len

  let next_valid_offset { mapping; _ } ~off =
    match Mapping_file.find_nearest_geq mapping off with
    | `After -> None
    | `Before entry -> Some entry.off
    | `Inside entry ->
        let open Int63.Syntax in
        Some (if entry.off < off then off else entry.off)

  let make_entry ~off ~poff ~len =
    if Int64.(equal zero) len then ""
    else
      let buf = Bytes.create (3 * 8) in
      Bytes.set_int64_le buf 0 off;
      Bytes.set_int64_le buf 8 poff;
      Bytes.set_int64_le buf 16 len;
      Bytes.unsafe_to_string buf

  module Wo = struct
    type nonrec t = t

    let open_wo ~mapping_size ~mapping ~data =
      open_ ~readonly:false ~mapping_size ~mapping ~data

    let write_exn t ~off ~len str =
      let poff, max_entry_len = get_poff t ~off in
      assert (len <= max_entry_len);
      Io.write_exn t.data ~off:poff ~len str

    let fsync t = Io.fsync t.data
    let close = close

    let create_from_data ~mapping ~dead_header_size ~size ~data:_ =
      let open Result_syntax in
      let entry =
        make_entry ~off:Int64.zero
          ~poff:(Int64.of_int dead_header_size)
          ~len:(Int63.to_int64 size)
      in
      let* mapping = Io.create ~path:mapping ~overwrite:false in
      let* () = Io.write_string mapping ~off:Int63.zero entry in
      let+ () = Io.close mapping in
      Int63.of_int (String.length entry)
  end

  module Ao = struct
    module Ao = Append_only_file.Make (Io) (Errs)

    type t = { mapping : Ao.t; data : Ao.t; mutable end_off : Int63.t }

    let end_off t = t.end_off
    let mapping_size t = Ao.end_poff t.mapping

    let create ~mapping ~data =
      let open Result_syntax in
      let ao_create path =
        Ao.create_rw ~path ~overwrite:false ~auto_flush_threshold:1_000_000
          ~auto_flush_procedure:`Internal
      in
      let* mapping = ao_create mapping in
      let+ data = ao_create data in
      { mapping; data; end_off = Int63.zero }

    let open_ao ~mapping_size ~mapping ~data =
      let open Result_syntax in
      let ao_open ~end_poff path =
        Ao.open_rw ~path ~end_poff ~dead_header_size:0
          ~auto_flush_threshold:1_000_000 ~auto_flush_procedure:`Internal
      in
      let* ao_mapping = ao_open ~end_poff:mapping_size mapping in
      let* end_off, end_poff =
        if mapping_size <= Int63.zero then Ok (Int63.zero, Int63.zero)
        else
          let entry_len = 3 * 8 in
          let+ entry =
            Ao.read_to_string ao_mapping
              ~off:Int63.(Syntax.(mapping_size - of_int entry_len))
              ~len:entry_len
          in
          let entry = Bytes.of_string entry in
          let end_off = Bytes.get_int64_le entry 0 |> Int63.of_int64 in
          let end_poff = Bytes.get_int64_le entry 8 |> Int63.of_int64 in
          let len = Bytes.get_int64_le entry 16 |> Int63.of_int64 in
          let open Int63.Syntax in
          (end_off + len, end_poff + len)
      in
      let+ ao_data = ao_open ~end_poff data in
      { mapping = ao_mapping; data = ao_data; end_off }

    let check_offset_exn { end_off; _ } ~off =
      if Int63.Syntax.(end_off > off) then
        Fmt.failwith
          "Sparse.Ao.append_exn at offset %a, smaller than latest offset %a"
          Int63.pp off Int63.pp end_off

    let append_seq_exn t ~off seq =
      check_offset_exn t ~off;
      let poff = Ao.end_poff t.data in
      let len =
        Seq.fold_left
          (fun len str ->
            Ao.append_exn t.data str;
            len + String.length str)
          0 seq
      in
      let entry =
        make_entry ~off:(Int63.to_int64 off) ~poff:(Int63.to_int64 poff)
          ~len:(Int64.of_int len)
      in
      Ao.append_exn t.mapping entry;
      t.end_off <- Int63.(Syntax.(off + of_int len))

    let flush t =
      let open Result_syntax in
      let* () = Ao.flush t.data in
      Ao.flush t.mapping

    let close t =
      let open Result_syntax in
      let* () = Ao.close t.data in
      Ao.close t.mapping
  end
end
