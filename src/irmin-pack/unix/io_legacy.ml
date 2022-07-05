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

open! Import
include Io_legacy_intf

module Unix : S = struct
  module Raw = Index_unix.Private.Raw

  type t = {
    file : string;
    raw : Raw.t;
    mutable offset : int63;
    mutable flushed : int63;
    readonly : bool;
    mutable version : Version.t;
    buf : Buffer.t;
  }

  let name t = t.file
  let header_size = (* offset + version *) Int63.of_int 16

  let unsafe_flush t =
    [%log.debug "IO flush %s" t.file];
    let buf = Buffer.contents t.buf in
    if buf = "" then ()
    else
      let offset = t.offset in
      Buffer.clear t.buf;
      Raw.unsafe_write t.raw ~off:t.flushed buf 0 (String.length buf);
      Raw.Offset.set t.raw offset;
      let open Int63.Syntax in
      (* concurrent append might happen so here t.offset might differ
         from offset *)
      if
        not (t.flushed + Int63.of_int (String.length buf) = header_size + offset)
      then
        Fmt.failwith "reload error: %s flushed=%a offset+header=%a\n%!" t.file
          Int63.pp t.flushed Int63.pp (offset + header_size);
      t.flushed <- offset + header_size

  let flush t =
    if t.readonly then raise Irmin_pack.RO_not_allowed;
    unsafe_flush t

  let auto_flush_limit = Int63.of_int 1_000_000

  let append t buf =
    Buffer.add_string t.buf buf;
    let len = Int63.of_int (String.length buf) in
    let open Int63.Syntax in
    t.offset <- t.offset + len;
    if t.offset - t.flushed > auto_flush_limit then flush t

  let set t ~off buf =
    if t.readonly then raise Irmin_pack.RO_not_allowed;
    unsafe_flush t;
    let buf_len = String.length buf in
    let open Int63.Syntax in
    Raw.unsafe_write t.raw ~off:(header_size + off) buf 0 buf_len;
    assert (
      let len = Int63.of_int buf_len in
      let off = header_size + off + len in
      off <= t.flushed)

  exception Invalid_read of string

  let raise_invalid_read fmt = Fmt.kstr (fun s -> raise (Invalid_read s)) fmt

  let read_buffer t ~off ~buf ~len =
    let open Int63.Syntax in
    let off = header_size + off in
    if (not t.readonly) && off > t.flushed then
      raise_invalid_read
        "Requested read of %d bytes at offset %a, but only flushed to %a" len
        Int63.pp off Int63.pp t.flushed;
    Raw.unsafe_read t.raw ~off ~len buf

  let read t ~off buf = read_buffer t ~off ~buf ~len:(Bytes.length buf)
  let offset t = t.offset

  let force_offset t =
    t.offset <- Raw.Offset.get t.raw;
    t.offset

  let version t =
    [%log.debug
      "[%s] version: %a" (Filename.basename t.file) Version.pp t.version];
    t.version

  let set_version t v =
    [%log.debug
      "[%s] set_version: %a -> %a" (Filename.basename t.file) Version.pp
        t.version Version.pp v];
    Raw.Version.set t.raw (Version.to_bin v);
    t.version <- v

  let readonly t = t.readonly

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with e -> protect_unix_exn e
  let safe f x = try f x with e -> ignore_enoent e

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then safe Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) (fun () ->
            protect (Unix.mkdir dir) 0o755;
            k ()))
    in
    aux dirname (fun () -> ())

  let raw ~flags ~version ~offset file =
    let x = Unix.openfile file flags 0o644 in
    let raw = Raw.v x in
    let header =
      { Raw.Header_prefix.version = Version.to_bin version; offset }
    in
    Raw.Header_prefix.set raw header;
    raw

  let v ~version ~fresh ~readonly file =
    let get_version () =
      match version with
      | Some v -> v
      | None ->
          Fmt.invalid_arg
            "Must supply an explicit version when creating a new store ({ file \
             = %s })"
            file
    in
    let v ~offset ~version raw =
      {
        version;
        file;
        offset;
        raw;
        readonly;
        buf = Buffer.create (4 * 1024);
        flushed = Int63.Syntax.(header_size + offset);
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let version = get_version () in
        let raw =
          raw
            ~flags:[ O_CREAT; mode; O_CLOEXEC ]
            ~version ~offset:Int63.zero file
        in
        v ~offset:Int63.zero ~version raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; mode; O_CLOEXEC ] 0o644 in
        let raw = Raw.v x in
        if fresh then (
          let version = get_version () in
          let header =
            {
              Raw.Header_prefix.version = Version.to_bin version;
              offset = Int63.zero;
            }
          in
          Raw.Header_prefix.set raw header;
          v ~offset:Int63.zero ~version raw)
        else
          let actual_version =
            let v_string = Raw.Version.get raw in
            match Version.of_bin v_string with
            | Some v -> v
            | None -> Version.invalid_arg v_string
          in
          (match version with
          | Some v when Version.compare actual_version v > 0 ->
              raise (Version.Invalid { expected = v; found = actual_version })
          | _ -> ());
          let offset = Raw.Offset.get raw in
          v ~offset ~version:actual_version raw

  let close t = Raw.close t.raw
  let exists file = Sys.file_exists file
  let size { raw; _ } = (Raw.fstat raw).st_size
end
