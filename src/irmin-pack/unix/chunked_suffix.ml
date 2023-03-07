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
include Chunked_suffix_intf

module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs
  module Ao = Append_only_file.Make (Io) (Errs)

  type chunk = { idx : int; suffix_off : int63; ao : Ao.t }
  type create_error = Io.create_error

  type open_error =
    [ Io.open_error
    | `Closed
    | `Invalid_argument
    | `Inconsistent_store
    | `Read_out_of_bounds ]

  type add_new_error =
    [ open_error
    | Io.close_error
    | `Pending_flush
    | `File_exists of string
    | `Multiple_empty_chunks ]

  (** A simple container for chunks. *)
  module Inventory : sig
    type t

    val v : int -> (int -> chunk) -> t
    val appendable : t -> chunk

    val find : off:int63 -> t -> chunk * int63
    (** [find ~off t] returns the chunk that contains suffix offset [off], along
        with the corresponding [poff] within the chunk.

        Raises `Read_out_of_bounds exception. *)

    val fold :
      (acc:'a -> is_appendable:bool -> chunk:chunk -> 'a) -> 'a -> t -> 'a

    val open_ :
      start_idx:int ->
      chunk_num:int ->
      open_chunk:
        (chunk_idx:int ->
        is_legacy:bool ->
        is_appendable:bool ->
        (Ao.t, open_error) result) ->
      (t, [> open_error ]) result

    val close : t -> (unit, [> Io.close_error | `Pending_flush ]) result

    val add_new_appendable :
      open_chunk:
        (chunk_idx:int ->
        is_legacy:bool ->
        is_appendable:bool ->
        (Ao.t, add_new_error) result) ->
      t ->
      (unit, [> add_new_error ]) result

    val length : t -> int63
    (** [length t] is the length of bytes for all chunks *)

    val start_idx : t -> int
    (** [start_idx t] is the idx of the first chunk *)

    val count : t -> int
    (** [count t] is the number of chunks *)
  end = struct
    type t = { mutable chunks : chunk Array.t }

    exception OpenInventoryError of open_error

    let v num create = { chunks = Array.init num create }
    let appendable t = Array.get t.chunks (Array.length t.chunks - 1)

    let find ~off t =
      let open Int63.Syntax in
      let suffix_off_to_chunk_poff c = off - c.suffix_off in
      let find c =
        let end_poff = Ao.end_poff c.ao in
        let poff = suffix_off_to_chunk_poff c in
        Int63.zero <= poff && poff < end_poff
      in
      match Array.find_opt find t.chunks with
      | None -> raise (Errors.Pack_error `Read_out_of_bounds)
      | Some c -> (c, suffix_off_to_chunk_poff c)

    let end_offset_of_chunk start_offset ao =
      let chunk_len = Ao.end_poff ao in
      Int63.Syntax.(start_offset + chunk_len)

    let is_legacy chunk_idx = chunk_idx = 0

    let fold f acc t =
      let appendable_idx = (appendable t).idx in
      Array.fold_left
        (fun acc chunk ->
          let is_appendable = chunk.idx = appendable_idx in
          f ~acc ~is_appendable ~chunk)
        acc t.chunks

    let open_ ~start_idx ~chunk_num ~open_chunk =
      let off_acc = ref Int63.zero in
      let create_chunk i =
        let suffix_off = !off_acc in
        let is_appendable = i = chunk_num - 1 in
        let chunk_idx = start_idx + i in
        let is_legacy = is_legacy chunk_idx in
        let open_result = open_chunk ~chunk_idx ~is_legacy ~is_appendable in
        match open_result with
        | Error err -> raise (OpenInventoryError err)
        | Ok ao ->
            off_acc := end_offset_of_chunk suffix_off ao;
            { idx = chunk_idx; suffix_off; ao }
      in
      try Ok (v chunk_num create_chunk)
      with OpenInventoryError err ->
        Error (err : open_error :> [> open_error ])

    let close t =
      (* Close immutable chunks, ignoring errors. *)
      let _ =
        Array.sub t.chunks 0 (Array.length t.chunks - 1)
        |> Array.iter @@ fun chunk ->
           let _ = Ao.close chunk.ao in
           ()
      in
      (* Close appendable chunk and keep error since this
         is the one that can have a pending flush. *)
      (appendable t).ao |> Ao.close

    let wrap_error result =
      Result.map_error
        (fun err -> (err : add_new_error :> [> add_new_error ]))
        result

    let reopen_last_chunk ~open_chunk t =
      (* Close the previous appendable chunk and reopen as non-appendable. *)
      let open Result_syntax in
      let ({ idx; ao; suffix_off } as last_chunk) = appendable t in
      let is_legacy = is_legacy idx in
      (* Compute the suffix_off for the following chunk. *)
      let length = end_offset_of_chunk suffix_off ao in
      let* () = Ao.close ao in
      let* ao =
        open_chunk ~chunk_idx:idx ~is_legacy ~is_appendable:false |> wrap_error
      in
      let pos = Array.length t.chunks - 1 in
      t.chunks.(pos) <- { last_chunk with ao };
      Ok length

    let create_appendable_chunk ~open_chunk t suffix_off =
      let open Result_syntax in
      let next_id = succ (appendable t).idx in
      let* ao =
        open_chunk ~chunk_idx:next_id ~is_legacy:false ~is_appendable:true
      in
      Ok { idx = next_id; suffix_off; ao }

    let add_new_appendable ~open_chunk t =
      let open Result_syntax in
      let* next_suffix_off = reopen_last_chunk ~open_chunk t in
      let* chunk =
        create_appendable_chunk ~open_chunk t next_suffix_off |> wrap_error
      in
      t.chunks <- Array.append t.chunks [| chunk |];
      Ok ()

    let length t =
      let open Int63.Syntax in
      Array.fold_left (fun sum c -> sum + Ao.end_poff c.ao) Int63.zero t.chunks

    let count t = Array.length t.chunks
    let start_idx t = t.chunks.(0).idx
  end

  type t = { inventory : Inventory.t; root : string; dead_header_size : int }

  let chunk_path = Layout.V4.suffix_chunk

  let create_rw ~root ~start_idx ~overwrite ~auto_flush_threshold
      ~auto_flush_procedure =
    let open Result_syntax in
    let chunk_idx = start_idx in
    let path = chunk_path ~root ~chunk_idx in
    let+ ao =
      Ao.create_rw ~path ~overwrite ~auto_flush_threshold ~auto_flush_procedure
    in
    let chunk = { idx = chunk_idx; suffix_off = Int63.zero; ao } in
    let inventory = Inventory.v 1 (Fun.const chunk) in
    { inventory; root; dead_header_size = 0 }

  (** A module to adjust values when mapping from chunks to append-only files *)
  module Ao_shim = struct
    type t = { dead_header_size : int; end_poff : int63 }

    let v ~path ~appendable_chunk_poff ~dead_header_size ~is_legacy
        ~is_appendable =
      let open Result_syntax in
      (* Only use the legacy dead_header_size for legacy chunks. *)
      let dead_header_size = if is_legacy then dead_header_size else 0 in
      (* The appendable chunk uses the provided [appendable_chunk_poff]; but the others
         read their size on disk. TODO: this is needed for the Ao module's current
         APIs but could perhaps be removed by future Ao API modifications. *)
      let+ end_poff =
        if is_appendable then Ok appendable_chunk_poff
        else
          match Io.size_of_path path with
          (* Subtract [dead_header_size] because the poff value stored in the
             control file does the same. *)
          | Ok s -> Ok Int63.Syntax.(s - Int63.of_int dead_header_size)
          | Error _ as e -> e
      in
      { dead_header_size; end_poff }
  end

  let open_rw ~root ~appendable_chunk_poff ~start_idx ~chunk_num
      ~dead_header_size ~auto_flush_threshold ~auto_flush_procedure =
    let open Result_syntax in
    let open_chunk ~chunk_idx ~is_legacy ~is_appendable =
      let path = chunk_path ~root ~chunk_idx in
      let* { dead_header_size; end_poff } =
        Ao_shim.v ~path ~appendable_chunk_poff ~dead_header_size ~is_legacy
          ~is_appendable
      in
      match is_appendable with
      | true ->
          Ao.open_rw ~path ~end_poff ~dead_header_size ~auto_flush_threshold
            ~auto_flush_procedure
      | false -> Ao.open_ro ~path ~end_poff ~dead_header_size
    in
    let+ inventory = Inventory.open_ ~start_idx ~chunk_num ~open_chunk in
    { inventory; root; dead_header_size }

  let open_ro ~root ~appendable_chunk_poff ~dead_header_size ~start_idx
      ~chunk_num =
    let open Result_syntax in
    let open_chunk ~chunk_idx ~is_legacy ~is_appendable =
      let path = chunk_path ~root ~chunk_idx in
      let* { dead_header_size; end_poff } =
        Ao_shim.v ~path ~appendable_chunk_poff ~dead_header_size ~is_legacy
          ~is_appendable
      in
      Ao.open_ro ~path ~end_poff ~dead_header_size
    in
    let+ inventory = Inventory.open_ ~start_idx ~chunk_num ~open_chunk in
    { inventory; root; dead_header_size }

  let start_idx t = Inventory.start_idx t.inventory
  let chunk_num t = Inventory.count t.inventory
  let appendable_ao t = (Inventory.appendable t.inventory).ao
  let appendable_chunk_poff t = appendable_ao t |> Ao.end_poff
  let end_soff t = Inventory.length t.inventory

  let read_exn t ~off ~len buf =
    let rec read progress_off suffix_off len_requested =
      let open Int63.Syntax in
      (* Find chunk with [suffix_off] and calculate length we can read. *)
      let chunk, poff = Inventory.find ~off:suffix_off t.inventory in
      let chunk_end_poff = Ao.end_poff chunk.ao in
      let read_end_poff = poff + len_requested in
      let len_read =
        if read_end_poff > chunk_end_poff then chunk_end_poff - poff
        else len_requested
      in

      (* Perform read. If this is the first read, we can use [buf]; otherwise,
         we create a new buffer and transfer after the read. *)
      let len_i = Int63.to_int len_read in
      let is_first_read = progress_off = Int63.zero in
      let ao_buf = if is_first_read then buf else Bytes.create len_i in
      Ao.read_exn chunk.ao ~off:poff ~len:len_i ao_buf;
      if not is_first_read then
        Bytes.blit ao_buf 0 buf (Int63.to_int progress_off) len_i;

      (* Read more if any is [rem]aining. *)
      let rem = len_requested - len_read in
      if rem > Int63.zero then
        read (progress_off + len_read) (suffix_off + len_read) rem
      else ()
    in
    read Int63.zero off (Int63.of_int len)

  let read_range_exn t ~off ~min_len ~max_len buf =
    let len =
      let max_off = end_soff t in
      let bytes_after_off = Int63.(to_int Syntax.(max_off - off)) in
      if bytes_after_off < min_len then
        raise (Errors.Pack_error `Read_out_of_bounds)
      else if bytes_after_off > max_len then max_len
      else bytes_after_off
    in
    read_exn t ~off ~len buf;
    len

  let append_exn t s = Ao.append_exn (appendable_ao t) s

  let add_chunk ~auto_flush_threshold ~auto_flush_procedure t =
    let open Result_syntax in
    let* () =
      let end_poff = appendable_chunk_poff t in
      if Int63.(equal end_poff zero) then Error `Multiple_empty_chunks
      else Ok ()
    in
    let root = t.root in
    let dead_header_size = t.dead_header_size in
    let open_chunk ~chunk_idx ~is_legacy ~is_appendable =
      let path = chunk_path ~root ~chunk_idx in
      let* { dead_header_size; end_poff } =
        Ao_shim.v ~path ~appendable_chunk_poff:Int63.zero ~dead_header_size
          ~is_legacy ~is_appendable
      in
      match is_appendable with
      | true ->
          Ao.create_rw ~path ~overwrite:true ~auto_flush_threshold
            ~auto_flush_procedure
      | false -> Ao.open_ro ~path ~end_poff ~dead_header_size
    in
    Inventory.add_new_appendable ~open_chunk t.inventory

  let close t = Inventory.close t.inventory
  let empty_buffer t = appendable_ao t |> Ao.empty_buffer
  let flush t = appendable_ao t |> Ao.flush
  let fsync t = appendable_ao t |> Ao.fsync

  let refresh_appendable_chunk_poff t new_poff =
    Ao.refresh_end_poff (appendable_ao t) new_poff

  let readonly t = appendable_ao t |> Ao.readonly
  let auto_flush_threshold t = appendable_ao t |> Ao.auto_flush_threshold

  let fold_chunks f acc t =
    Inventory.fold
      (fun ~acc ~is_appendable ~chunk ->
        let len = Ao.end_poff chunk.ao in
        let start_suffix_off = chunk.suffix_off in
        let end_suffix_off = Int63.Syntax.(start_suffix_off + len) in
        f ~acc ~idx:chunk.idx ~start_suffix_off ~end_suffix_off ~is_appendable)
      acc t.inventory
end
