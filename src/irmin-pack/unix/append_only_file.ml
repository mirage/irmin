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
include Append_only_file_intf

module Make (Io : Io.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs

  type t = {
    io : Io.t;
    mutable persisted_end_poff : int63;
    dead_header_size : int63;
    rw_perm : rw_perm option;
  }

  and auto_flush_procedure = [ `Internal | `External of t -> unit ]

  and rw_perm = {
    buf : Buffer.t;
    auto_flush_threshold : int;
    auto_flush_procedure : auto_flush_procedure;
  }
  (** [rw_perm] contains the data necessary to operate in readwrite mode. *)

  let create_rw ~path ~overwrite ~auto_flush_threshold ~auto_flush_procedure =
    let open Result_syntax in
    let+ io = Io.create ~path ~overwrite in
    let persisted_end_poff = Int63.zero in
    let buf = Buffer.create 0 in
    {
      io;
      persisted_end_poff;
      dead_header_size = Int63.zero;
      rw_perm = Some { buf; auto_flush_threshold; auto_flush_procedure };
    }

  (** A store is consistent if the real offset of the suffix/dict files is the
      one recorded in the control file. When opening the store, the offset from
      the control file is passed as the [end_poff] argument to the [open_ro],
      [open_rw] functions. The [end_poff] from the control file is then used as
      the real offset.

      In case of a crash, we can only recover if the [end_poff] is smaller than
      the real offset. We cannot recover otherwise, because we have no
      guarantees that the last object fsynced to disk is written entirely to
      disk. *)
  let check_consistent_store ~end_poff ~dead_header_size io =
    let open Result_syntax in
    let* real_offset = Io.read_size io in
    let dead_header_size = Int63.of_int dead_header_size in
    let real_offset_without_header =
      Int63.Syntax.(real_offset - dead_header_size)
    in
    if real_offset_without_header < end_poff then Error `Inconsistent_store
    else (
      if real_offset_without_header > end_poff then
        [%log.warn
          "The end offset in the control file %a is smaller than the offset on \
           disk %a for %s; the store was closed in a inconsistent state."
          Int63.pp end_poff Int63.pp real_offset_without_header (Io.path io)];
      Ok ())

  let open_rw ~path ~end_poff ~dead_header_size ~auto_flush_threshold
      ~auto_flush_procedure =
    let open Result_syntax in
    let* io = Io.open_ ~path ~readonly:false in
    let+ () = check_consistent_store ~end_poff ~dead_header_size io in
    let persisted_end_poff = end_poff in
    let dead_header_size = Int63.of_int dead_header_size in
    let buf = Buffer.create 0 in
    {
      io;
      persisted_end_poff;
      dead_header_size;
      rw_perm = Some { buf; auto_flush_threshold; auto_flush_procedure };
    }

  let open_ro ~path ~end_poff ~dead_header_size =
    let open Result_syntax in
    let* io = Io.open_ ~path ~readonly:true in
    let+ () = check_consistent_store ~end_poff ~dead_header_size io in
    let persisted_end_poff = end_poff in
    let dead_header_size = Int63.of_int dead_header_size in
    { io; persisted_end_poff; dead_header_size; rw_perm = None }

  let empty_buffer = function
    | { rw_perm = Some { buf; _ }; _ } when Buffer.length buf > 0 -> false
    | _ -> true

  let close t =
    if not @@ empty_buffer t then Error `Pending_flush else Io.close t.io

  let readonly t = Io.readonly t.io
  let path t = Io.path t.io

  let auto_flush_threshold = function
    | { rw_perm = None; _ } -> None
    | { rw_perm = Some rw_perm; _ } -> Some rw_perm.auto_flush_threshold

  let end_poff t =
    match t.rw_perm with
    | None -> t.persisted_end_poff
    | Some rw_perm ->
        let open Int63.Syntax in
        t.persisted_end_poff + (Buffer.length rw_perm.buf |> Int63.of_int)

  let refresh_end_poff t new_end_poff =
    match t.rw_perm with
    | Some _ -> Error `Rw_not_allowed
    | None ->
        t.persisted_end_poff <- new_end_poff;
        Ok ()

  let flush t =
    match t.rw_perm with
    | None -> Error `Ro_not_allowed
    | Some rw_perm ->
        let open Result_syntax in
        let open Int63.Syntax in
        let s = Buffer.contents rw_perm.buf in
        let off = t.persisted_end_poff + t.dead_header_size in
        let+ () = Io.write_string t.io ~off s in
        t.persisted_end_poff <-
          t.persisted_end_poff + (String.length s |> Int63.of_int);
        (* [truncate] is semantically identical to [clear], except that
           [truncate] doesn't deallocate the internal buffer. We use
           [clear] in legacy_io. *)
        Buffer.truncate rw_perm.buf 0

  let fsync t = Io.fsync t.io

  let read_exn t ~off ~len b =
    let open Int63.Syntax in
    let off' = off + Int63.of_int len in
    if off' > t.persisted_end_poff then
      raise (Errors.Pack_error `Read_out_of_bounds);
    let off = off + t.dead_header_size in
    Io.read_exn t.io ~off ~len b

  let read_to_string t ~off ~len =
    let open Int63.Syntax in
    let off' = off + Int63.of_int len in
    if off' > t.persisted_end_poff then Error `Read_out_of_bounds
    else
      let off = off + t.dead_header_size in
      Io.read_to_string t.io ~off ~len

  let append_exn t s =
    match t.rw_perm with
    | None -> raise Errors.RO_not_allowed
    | Some rw_perm -> (
        assert (Buffer.length rw_perm.buf < rw_perm.auto_flush_threshold);
        Buffer.add_string rw_perm.buf s;
        if Buffer.length rw_perm.buf >= rw_perm.auto_flush_threshold then
          match rw_perm.auto_flush_procedure with
          | `Internal -> flush t |> Errs.raise_if_error
          | `External cb ->
              cb t;
              assert (empty_buffer t))
end
