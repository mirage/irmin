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

module Make (Io : Io_intf.S) (Errs : Io_errors.S with module Io = Io) = struct
  module Io = Io
  module Errs = Errs

  let auto_flush_threshold = 16_384

  type rw_perm = {
    fsync_required : bool Atomic.t;
    buf : Buffer.t;
    buf_length : int Atomic.t;
  }
  (** [rw_perm] contains the data necessary to operate in readwrite mode. *)

  type t = {
    io : Io.t;
    persisted_end_poff : int63 Atomic.t;
    dead_header_size : int63;
    rw_perm : rw_perm option;
  }

  let create_rw_perm () =
    Some
      {
        fsync_required = Atomic.make false;
        buf = Buffer.create 0;
        buf_length = Atomic.make 0;
      }

  let create_rw ~sw ~path ~overwrite =
    let open Result_syntax in
    let+ io = Io.create ~sw ~path ~overwrite in
    let persisted_end_poff = Atomic.make Int63.zero in
    {
      io;
      persisted_end_poff;
      dead_header_size = Int63.zero;
      rw_perm = create_rw_perm ();
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
           disk %a for %a; the store was closed in a inconsistent state."
          Int63.pp end_poff Int63.pp real_offset_without_header Eio.Path.pp
            (Io.path io)];
      Ok ())

  let open_rw ~sw ~path ~end_poff ~dead_header_size =
    let open Result_syntax in
    let* io = Io.open_ ~sw ~path ~readonly:false in
    let+ () = check_consistent_store ~end_poff ~dead_header_size io in
    let persisted_end_poff = Atomic.make end_poff in
    let dead_header_size = Int63.of_int dead_header_size in
    { io; persisted_end_poff; dead_header_size; rw_perm = create_rw_perm () }

  let open_ro ~sw ~path ~end_poff ~dead_header_size =
    let open Result_syntax in
    let* io = Io.open_ ~sw ~path ~readonly:true in
    let+ () = check_consistent_store ~end_poff ~dead_header_size io in
    let persisted_end_poff = Atomic.make end_poff in
    let dead_header_size = Int63.of_int dead_header_size in
    { io; persisted_end_poff; dead_header_size; rw_perm = None }

  let empty_buffer = function
    | { rw_perm = Some { buf_length; _ }; _ } -> Atomic.get buf_length = 0
    | _ -> true

  let close t =
    if not @@ empty_buffer t then Error `Pending_flush else Io.close t.io

  let readonly t = Io.readonly t.io
  let path t = Io.path t.io

  let end_poff t =
    let persisted_end_poff = Atomic.get t.persisted_end_poff in
    match t.rw_perm with
    | None -> persisted_end_poff
    | Some rw_perm ->
        let open Int63.Syntax in
        persisted_end_poff + (Atomic.get rw_perm.buf_length |> Int63.of_int)

  let refresh_end_poff t new_end_poff =
    match t.rw_perm with
    | Some _ -> Error `Rw_not_allowed
    | None ->
        Atomic.set t.persisted_end_poff new_end_poff;
        Ok ()

  let flush t =
    match t.rw_perm with
    | None -> Error `Ro_not_allowed
    | Some rw_perm ->
        let open Result_syntax in
        let open Int63.Syntax in
        let s = Buffer.contents rw_perm.buf in
        let persisted_end_poff = Atomic.get t.persisted_end_poff in
        let off = persisted_end_poff + t.dead_header_size in
        let+ () = Io.write_string t.io ~off s in
        Atomic.set rw_perm.buf_length 0;
        Atomic.set t.persisted_end_poff
          (persisted_end_poff + (String.length s |> Int63.of_int));
        (* [truncate] is semantically identical to [clear], except that
           [truncate] doesn't deallocate the internal buffer. We use
           [clear] in legacy_io. *)
        Buffer.truncate rw_perm.buf 0;
        Atomic.set rw_perm.fsync_required true

  let fsync t =
    match t.rw_perm with
    | None -> Error `Ro_not_allowed
    | Some rw ->
        assert (Buffer.length rw.buf = 0);
        if Atomic.get rw.fsync_required then
          let open Result_syntax in
          let+ () = Io.fsync t.io in
          Atomic.set rw.fsync_required true
        else Ok ()

  let read_exn t ~off ~len b =
    let open Int63.Syntax in
    let off' = off + Int63.of_int len in
    if off' > Atomic.get t.persisted_end_poff then
      raise (Errors.Pack_error `Read_out_of_bounds);
    let off = off + t.dead_header_size in
    Io.read_exn t.io ~off ~len b

  let read_to_string t ~off ~len =
    let open Int63.Syntax in
    let off' = off + Int63.of_int len in
    if off' > Atomic.get t.persisted_end_poff then Error `Read_out_of_bounds
    else
      let off = off + t.dead_header_size in
      Io.read_to_string t.io ~off ~len

  let append_exn t s =
    match t.rw_perm with
    | None -> raise Errors.RO_not_allowed
    | Some rw_perm ->
        assert (Atomic.get rw_perm.buf_length < auto_flush_threshold);
        Buffer.add_string rw_perm.buf s;
        let (_ : int) =
          Atomic.fetch_and_add rw_perm.buf_length (String.length s)
        in
        let buf_length = Atomic.get rw_perm.buf_length in
        if buf_length >= auto_flush_threshold then
          flush t |> Errs.raise_if_error
end
