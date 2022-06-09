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

module Make (Io : Io.S) = struct
  module Io = Io

  type rw_perm = {
    buf : Buffer.t;
    auto_flush_threshold : int;
    auto_flush_callback : unit -> unit;
  }
  (** [rw_perm] contains the data necessary to operate in readwrite mode. *)

  type t = {
    io : Io.t;
    mutable persisted_end_offset : int63;
    dead_header_size : int63;
    rw_perm : rw_perm option;
  }

  let create_rw ~path ~overwrite ~auto_flush_threshold ~auto_flush_callback =
    let open Result_syntax in
    let+ io = Io.create ~path ~overwrite in
    let persisted_end_offset = Int63.zero in
    let buf = Buffer.create 0 in
    {
      io;
      persisted_end_offset;
      dead_header_size = Int63.zero;
      rw_perm = Some { buf; auto_flush_threshold; auto_flush_callback };
    }

  let open_rw ~path ~end_offset ~dead_header_size ~auto_flush_threshold
      ~auto_flush_callback =
    let open Result_syntax in
    let+ io = Io.open_ ~path ~readonly:false in
    let persisted_end_offset = end_offset in
    let dead_header_size = Int63.of_int dead_header_size in
    let buf = Buffer.create 0 in
    {
      io;
      persisted_end_offset;
      dead_header_size;
      rw_perm = Some { buf; auto_flush_threshold; auto_flush_callback };
    }

  let open_ro ~path ~end_offset ~dead_header_size =
    let open Result_syntax in
    let+ io = Io.open_ ~path ~readonly:true in
    let persisted_end_offset = end_offset in
    let dead_header_size = Int63.of_int dead_header_size in
    { io; persisted_end_offset; dead_header_size; rw_perm = None }

  let close t =
    let open Result_syntax in
    let* () =
      match t.rw_perm with
      | None -> Ok ()
      | Some rw_perm ->
          if Buffer.length rw_perm.buf <> 0 then Error `Pending_flush else Ok ()
    in
    Io.close t.io

  let readonly t = Io.readonly t.io

  let end_offset t =
    match t.rw_perm with
    | None -> t.persisted_end_offset
    | Some rw_perm ->
        let ( + ) = Int63.add in
        t.persisted_end_offset + (Buffer.length rw_perm.buf |> Int63.of_int)

  let refresh_end_offset t new_end_offset =
    match t.rw_perm with
    | Some _ -> Error `Rw_not_allowed
    | None ->
        t.persisted_end_offset <- new_end_offset;
        Ok ()

  let flush t =
    match t.rw_perm with
    | None -> Error `Ro_not_allowed
    | Some rw_perm ->
        let open Result_syntax in
        let ( + ) = Int63.add in
        let s = Buffer.contents rw_perm.buf in
        let off = t.persisted_end_offset + t.dead_header_size in
        let+ () = Io.write_string t.io ~off s in
        t.persisted_end_offset <-
          t.persisted_end_offset + (String.length s |> Int63.of_int);
        (* [truncate] is sementically identical to [clear], except that
           [truncate] doesn't deallocate the internal buffer. We used to use
           [clear] in legacy_io. *)
        Buffer.truncate rw_perm.buf

  let fsync t = Io.fsync t.io

  let read_exn t ~off ~len b =
    let ( + ) = Int63.add in
    let off = off + t.dead_header_size in
    Io.read_exn t.io ~off ~len b

  let append_exn t s =
    match t.rw_perm with
    | None -> raise (Io.Write_error `Ro_not_allowed)
    | Some rw_perm ->
        assert (Buffer.length rw_perm.buf < rw_perm.auto_flush_threshold);
        Buffer.add_string rw_perm.buf s;
        if Buffer.length rw_perm.buf >= rw_perm.auto_flush_threshold then (
          rw_perm.auto_flush_callback ();
          assert (Buffer.length rw_perm.buf = 0))
end
