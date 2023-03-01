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

module Int64_mmap : sig
  type t = private {
    fn : string;
    fd : Unix.file_descr;
    mutable arr : int64_bigarray;
  }

  val open_ro : fn:string -> sz:int -> t
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size
      of the underlying file *)

  val close : t -> unit
end = struct
  type t = { fn : string; fd : Unix.file_descr; mutable arr : int64_bigarray }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ro ~fn ~sz =
    let shared = false in
    assert (Sys.file_exists fn);
    let fd = Unix.(openfile fn [ O_RDONLY ] 0o660) in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int64 c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  let close t =
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int64 c_layout 0);
    ()
end

module Make (Io : Io.S) = struct
  module Io = Io
  module Errs = Io_errors.Make (Io)

  module Mapping_file = struct
    type t = { arr : int64_bigarray; path : string }

    let open_map ~path =
      match Io.classify_path path with
      | `File -> (
          let mmap = Int64_mmap.open_ro ~fn:path ~sz:(-1) in
          let arr = mmap.arr in
          let len = BigArr1.dim arr in
          match len mod 3 = 0 with
          | true ->
              Int64_mmap.close mmap;
              Ok { path; arr }
          | false ->
              Error
                (`Corrupted_mapping_file
                  (__FILE__
                  ^ ": mapping mmap size did not meet size requirements")))
      | _ -> Error (`No_such_file_or_directory path)

    let conv_int64 : int64 -> int =
     fun i ->
      (if Sys.big_endian then (
       (* We are currently on a BE platform but the ints are encoded as LE in the
          file. We've just read a LE int using a BE decoding scheme. Let's fix
          this.

          The first step is to set [buf] to contain exactly what is stored on
          disk. Since the current platform is BE, we've interpreted what was
          written on disk using a BE decoding scheme. To do the opposite operation
          we must use a BE encoding scheme, hence [set_int64_be].

          Now that [buf] mimics what was on disk, the second step consist of
          decoding it using a LE function, hence [get_int64_le]. *)
       let buf = Bytes.create 8 in
       Bytes.set_int64_be buf 0 i;
       Bytes.get_int64_le buf 0)
      else i)
      |> Int64.to_int

    let entry_count arr = BigArr1.dim arr / 3
    let entry_idx i = i * 3
    let entry_off arr i = arr.{entry_idx i} |> conv_int64 |> Int63.of_int
    let entry_poff arr i = arr.{entry_idx i + 1} |> conv_int64 |> Int63.of_int
    let entry_len arr i = arr.{entry_idx i + 2} |> conv_int64

    let iter_exn { arr; _ } f =
      for i = 0 to entry_count arr - 1 do
        f ~off:(entry_off arr i) ~len:(entry_len arr i)
      done

    let iter t f = Errs.catch (fun () -> iter_exn t f)

    type entry = { off : int63; poff : int63; len : int }

    let find_nearest_geq { arr; _ } off =
      let get arr i =
        let start = arr.{entry_idx i} |> conv_int64 in
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

  let open_ ~readonly ~mapping ~data =
    let open Result_syntax in
    let* mapping = Mapping_file.open_map ~path:mapping in
    let+ data = Io.open_ ~path:data ~readonly in
    { mapping; data }

  let open_ro ~mapping ~data = open_ ~readonly:true ~mapping ~data
  let close t = Io.close t.data
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
    let poff, max_entry_len = get_poff t ~off in
    if max_entry_len < min_len then
      raise (Errors.Pack_error `Read_out_of_bounds);
    let len = min max_len max_entry_len in
    Io.read_exn t.data ~off:poff ~len buf

  let next_valid_offset { mapping; _ } ~off =
    match Mapping_file.find_nearest_geq mapping off with
    | `After -> None
    | `Before entry -> Some entry.off
    | `Inside entry ->
        let open Int63.Syntax in
        Some (if entry.off < off then off else entry.off)

  module Wo = struct
    type nonrec t = t

    let open_wo ~mapping ~data = open_ ~readonly:false ~mapping ~data

    let write_exn t ~off ~len str =
      let poff, max_entry_len = get_poff t ~off in
      assert (len <= max_entry_len);
      Io.write_exn t.data ~off:poff ~len str

    let fsync t = Io.fsync t.data
    let close = close
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
      let buffer = Bytes.create 24 in
      Bytes.set_int64_le buffer 0 (Int63.to_int64 off);
      Bytes.set_int64_le buffer 8 (Int63.to_int64 poff);
      Bytes.set_int64_le buffer 16 (Int64.of_int len);
      Ao.append_exn t.mapping (Bytes.unsafe_to_string buffer);
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
