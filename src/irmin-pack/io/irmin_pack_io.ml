(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.pack.io" ~doc:"IO for irmin-pack"

module Log = (val Logs.src_log src : Logs.LOG)

exception RO_Not_Allowed

module type S = sig
  type t

  val v : fresh:bool -> ?version:string -> readonly:bool -> string -> t

  val name : t -> string

  val clear : t -> unit

  val append : t -> string -> unit

  val set : t -> off:int64 -> string -> unit

  val read : t -> off:int64 -> bytes -> int

  val offset : t -> int64

  val force_offset : t -> int64

  val readonly : t -> bool

  val version : t -> string

  val sync : t -> unit
end

let ( ++ ) = Int64.add

let ( -- ) = Int64.sub

module Unix : S = struct
  module Raw = struct
    type t = { fd : Unix.file_descr; mutable cursor : int64 }

    let v fd = { fd; cursor = 0L }

    let really_write fd buf =
      let rec aux off len =
        let w = Unix.write fd buf off len in
        if w = 0 then () else (aux [@tailcall]) (off + w) (len - w)
      in
      (aux [@tailcall]) 0 (Bytes.length buf)

    let really_read fd len buf =
      let rec aux off len =
        let r = Unix.read fd buf off len in
        if r = 0 then off (* end of file *)
        else if r = len then off + r
        else (aux [@tailcall]) (off + r) (len - r)
      in
      (aux [@tailcall]) 0 len

    let lseek t off =
      if off = t.cursor then ()
      else
        let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
        t.cursor <- off

    let unsafe_write t ~off buf =
      lseek t off;
      let buf = Bytes.unsafe_of_string buf in
      really_write t.fd buf;
      t.cursor <- off ++ Int64.of_int (Bytes.length buf)

    let unsafe_read t ~off ~len buf =
      lseek t off;
      let n = really_read t.fd len buf in
      t.cursor <- off ++ Int64.of_int n;
      n

    external get_64 : bytes -> int -> int64 = "%caml_string_get64"

    external set_64 : bytes -> int -> int64 -> unit = "%caml_string_set64u"

    external swap64 : int64 -> int64 = "%bswap_int64"

    let get_uint64 s off =
      if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off

    let set_uint64 s off v =
      if not Sys.big_endian then set_64 s off (swap64 v) else set_64 s off v

    let int64_buf = Bytes.create 8

    let unsafe_set_offset t n =
      set_uint64 int64_buf 0 n;
      unsafe_write t ~off:0L (Bytes.unsafe_to_string int64_buf)

    let unsafe_get_offset t =
      let n = unsafe_read t ~off:0L ~len:8 int64_buf in
      assert (n = 8);
      get_uint64 int64_buf 0

    let version_buf = Bytes.create 8

    let unsafe_get_version t =
      let n = unsafe_read t ~off:8L ~len:8 version_buf in
      assert (n = 8);
      Bytes.to_string version_buf

    let unsafe_set_version t v = unsafe_write t ~off:8L v
  end

  type t = {
    file : string;
    mutable raw : Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    readonly : bool;
    version : string;
    buf : Buffer.t
  }

  let name t = t.file

  let header = 16L (* offset + version *)

  let sync t =
    if t.readonly then raise RO_Not_Allowed;
    Log.debug (fun l -> l "IO sync %s" t.file);
    let buf = Buffer.contents t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if buf = "" then ()
    else (
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      Raw.unsafe_set_offset t.raw offset;
      (* concurrent append might happen so here t.offset might differ
         from offset *)
      if not (t.flushed ++ Int64.of_int (String.length buf) = header ++ offset)
      then
        Fmt.failwith "sync error: %s flushed=%Ld offset+header=%Ld\n%!" t.file
          t.flushed (offset ++ header);
      t.flushed <- offset ++ header )

  let auto_flush_limit = 1_000_000L

  let append t buf =
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then sync t

  let set t ~off buf =
    if t.readonly then raise RO_Not_Allowed;
    sync t;
    Raw.unsafe_write t.raw ~off:(header ++ off) buf;
    let len = Int64.of_int (String.length buf) in
    let off = header ++ off ++ len in
    assert (off <= t.flushed)

  let read t ~off buf =
    if not t.readonly then assert (header ++ off <= t.flushed);
    Raw.unsafe_read t.raw ~off:(header ++ off) ~len:(Bytes.length buf) buf

  let offset t = t.offset

  let force_offset t =
    t.offset <- Raw.unsafe_get_offset t.raw;
    t.offset

  let version t = t.version

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
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        protect (Unix.mkdir dir) 0o755;
        k () )
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let clear t =
    t.offset <- 0L;
    t.flushed <- header;
    Buffer.clear t.buf

  let buffers = Hashtbl.create 256

  let buffer file =
    try
      let buf = Hashtbl.find buffers file in
      Buffer.clear buf;
      buf
    with Not_found ->
      let buf = Buffer.create (4 * 1024) in
      Hashtbl.add buffers file buf;
      buf

  let err_missing_version file = Fmt.failwith "%s: missing version" file

  let check_version v f = match v with None -> () | Some v -> f v

  let map_version file v f =
    match v with None -> err_missing_version file | Some v -> f v

  let v ~fresh ?version:current_version ~readonly file =
    check_version current_version (fun v -> assert (String.length v = 8));
    let v ~offset ~version raw =
      { version;
        file;
        offset;
        raw;
        readonly;
        buf = buffer file;
        flushed = header ++ offset
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        if readonly then raise RO_Not_Allowed;
        let x = Unix.openfile file Unix.[ O_CREAT; mode ] 0o644 in
        let raw = Raw.v x in
        Raw.unsafe_set_offset raw 0L;
        map_version file current_version @@ fun version ->
        Raw.unsafe_set_version raw version;
        v ~offset:0L ~version raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; mode ] 0o644 in
        let raw = Raw.v x in
        if fresh then (
          Raw.unsafe_set_offset raw 0L;
          map_version file current_version @@ fun version ->
          Raw.unsafe_set_version raw version;
          v ~offset:0L ~version raw )
        else
          let offset = Raw.unsafe_get_offset raw in
          let version = Raw.unsafe_get_version raw in
          v ~offset ~version raw
end

let ( // ) = Filename.concat

let with_cache ~v ~clear file =
  let files = Hashtbl.create 13 in
  fun ?(fresh = false) ?(shared = true) ?(readonly = false) root ->
    let file = root // file in
    if fresh && readonly then raise RO_Not_Allowed;
    if not shared then (
      Log.debug (fun l ->
          l "[%s] v fresh=%b shared=%b readonly=%b" (Filename.basename file)
            fresh shared readonly );
      let t = v ~fresh ~shared ~readonly file in
      if fresh then clear t;
      t )
    else
      try
        if not (Sys.file_exists file) then (
          Log.debug (fun l ->
              l "[%s] does not exist anymore, cleaning up the fd cache"
                (Filename.basename file) );
          Hashtbl.remove files file;
          raise Not_found );
        let t = Hashtbl.find files file in
        Log.debug (fun l -> l "%s found in cache" file);
        if fresh then clear t;
        t
      with Not_found ->
        Log.debug (fun l ->
            l "[%s] v fresh=%b shared=%b readonly=%b" (Filename.basename file)
              fresh shared readonly );
        let t = v ~fresh ~shared ~readonly file in
        if fresh then clear t;
        Hashtbl.add files file t;
        t
