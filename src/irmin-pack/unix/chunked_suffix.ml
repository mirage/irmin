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

  (** A simple container for chunks. *)
  module Inventory : sig
    type t

    val v : int -> (int -> chunk) -> t
    val appendable : t -> chunk

    val find : off:int63 -> t -> chunk * int63
    (** [find ~off t] returns the chunk that contains suffix offset [off], along
        with the corresponding [poff] within the chunk.

        Raises `Read_out_of_bounds exception. *)

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
  end = struct
    type t = chunk Array.t

    exception OpenInventoryError of open_error

    let v = Array.init
    let appendable t = Array.get t (Array.length t - 1)

    let find ~off t =
      let open Int63.Syntax in
      let suffix_off_to_chunk_poff c = off - c.suffix_off in
      let find c =
        let end_poff = Ao.end_poff c.ao in
        let poff = suffix_off_to_chunk_poff c in
        Int63.zero <= poff && poff < end_poff
      in
      match Array.find_opt find t with
      | None -> raise (Errors.Pack_error `Read_out_of_bounds)
      | Some c -> (c, suffix_off_to_chunk_poff c)

    let open_ ~start_idx ~chunk_num ~open_chunk =
      let off_acc = ref Int63.zero in
      let create_chunk i =
        let suffix_off = !off_acc in
        let is_appendable = i = chunk_num - 1 in
        let chunk_idx = start_idx + i in
        let is_legacy = chunk_idx = 0 in
        let open_result = open_chunk ~chunk_idx ~is_legacy ~is_appendable in
        match open_result with
        | Error err -> raise (OpenInventoryError err)
        | Ok ao ->
            let chunk_len = Ao.end_poff ao in
            (off_acc := Int63.Syntax.(suffix_off + chunk_len));
            { idx = chunk_idx; suffix_off; ao }
      in
      try Ok (v chunk_num create_chunk)
      with OpenInventoryError err ->
        Error (err : open_error :> [> open_error ])

    let close t =
      (* Close immutable chunks, ignoring errors. *)
      let _ =
        Array.sub t 0 (Array.length t - 1)
        |> Array.iter @@ fun chunk ->
           let _ = Ao.close chunk.ao in
           ()
      in
      (* Close appendable chunk and keep error since this
         is the one that can have a pending flush. *)
      (appendable t).ao |> Ao.close
  end

  type t = { inventory : Inventory.t }

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
    { inventory }

  (** A module to adjust values when mapping from chunks to append-only files *)
  module Ao_shim = struct
    type t = { dead_header_size : int; end_poff : int63 }

    let v ~path ~end_poff ~dead_header_size ~is_legacy ~is_appendable =
      let open Result_syntax in
      (* Only use the legacy dead_header_size for legacy chunks. *)
      let dead_header_size = if is_legacy then dead_header_size else 0 in
      (* The appendable chunk uses the provided [end_poff]; but the others
         read their size on disk. TODO: this is needed for the Ao module's current
         APIs but could perhaps be removed by future Ao API modifications. *)
      let+ end_poff =
        if is_appendable then Ok end_poff else Io.size_of_path path
      in
      { dead_header_size; end_poff }
  end

  let open_rw ~root ~end_poff ~start_idx ~chunk_num ~dead_header_size
      ~auto_flush_threshold ~auto_flush_procedure =
    let open Result_syntax in
    let open_chunk ~chunk_idx ~is_legacy ~is_appendable =
      let path = chunk_path ~root ~chunk_idx in
      let* { dead_header_size; end_poff } =
        Ao_shim.v ~path ~end_poff ~dead_header_size ~is_legacy ~is_appendable
      in
      match is_appendable with
      | true ->
          Ao.open_rw ~path ~end_poff ~dead_header_size ~auto_flush_threshold
            ~auto_flush_procedure
      | false -> Ao.open_ro ~path ~end_poff ~dead_header_size
    in
    let+ inventory = Inventory.open_ ~start_idx ~chunk_num ~open_chunk in
    { inventory }

  let open_ro ~root ~end_poff ~dead_header_size ~start_idx ~chunk_num =
    let open Result_syntax in
    let open_chunk ~chunk_idx ~is_legacy ~is_appendable =
      let path = chunk_path ~root ~chunk_idx in
      let* { dead_header_size; end_poff } =
        Ao_shim.v ~path ~end_poff ~dead_header_size ~is_legacy ~is_appendable
      in
      Ao.open_ro ~path ~end_poff ~dead_header_size
    in
    let+ inventory = Inventory.open_ ~start_idx ~chunk_num ~open_chunk in
    { inventory }

  let appendable_ao t = (Inventory.appendable t.inventory).ao
  let end_poff t = appendable_ao t |> Ao.end_poff

  let read_exn t ~off ~len buf =
    let chunk, poff = Inventory.find ~off t.inventory in
    Ao.read_exn chunk.ao ~off:poff ~len buf

  let append_exn t s = Ao.append_exn (appendable_ao t) s
  let close t = Inventory.close t.inventory
  let empty_buffer t = appendable_ao t |> Ao.empty_buffer
  let flush t = appendable_ao t |> Ao.flush
  let fsync t = appendable_ao t |> Ao.fsync

  let refresh_end_poff t new_end_poff =
    Ao.refresh_end_poff (appendable_ao t) new_end_poff

  let readonly t = appendable_ao t |> Ao.readonly
  let auto_flush_threshold t = appendable_ao t |> Ao.auto_flush_threshold
end
