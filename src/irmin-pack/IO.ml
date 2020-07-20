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

(* For every new version, update the [version] type and [versions]
   headers. *)
type version = [ `V1 | `V2 ]

let versions = [ (`V1, "00000001"); (`V2, "00000002") ]

module type S = sig
  type t

  exception RO_Not_Allowed

  val v : version:version -> fresh:bool -> readonly:bool -> string -> t

  val name : t -> string

  val clear : t -> unit

  val append : t -> string -> unit

  val set : t -> off:int64 -> string -> unit

  val read : t -> off:int64 -> bytes -> int

  val offset : t -> int64

  val force_offset : t -> int64

  val generation : t -> int64

  val force_generation : t -> int64

  val readonly : t -> bool

  val version : t -> version

  val flush : t -> unit

  val close : t -> unit
end

let ( ++ ) = Int64.add

let ( -- ) = Int64.sub

let pp_version ppf v =
  let rec aux i = function
    | (x, _) :: t -> if v = x then i else aux (i + 1) t
    | [] -> assert false
  in
  Fmt.pf ppf "v%d" (aux 1 versions)

let bin_of_version v = List.assoc v versions

let version_of_bin b =
  try Some (List.assoc b (List.map (fun (x, y) -> (y, x)) versions))
  with Not_found -> None

module Unix : S = struct
  exception RO_Not_Allowed

  module Raw = Index_unix.Private.Raw

  type t = {
    file : string;
    mutable raw : Raw.t;
    mutable generation : int64;
    mutable offset : int64;
    mutable flushed : int64;
    readonly : bool;
    mutable version : [ `V1 | `V2 ];
    buf : Buffer.t;
  }

  let name t = t.file

  let header_v1 = 16L (* offset + version *)

  let header_v2 = 24L (* offset + version + generation *)

  let header_of_version = function `V1 -> header_v1 | `V2 -> header_v2

  let header t = header_of_version t.version

  let unsafe_flush t =
    Log.debug (fun l -> l "IO flush %s" t.file);
    let buf = Buffer.contents t.buf in
    if buf = "" then ()
    else
      let offset = t.offset in
      Buffer.clear t.buf;
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      (* version and generation can change after a clear *)
      Raw.Header.set t.raw
        {
          Raw.Header.version = bin_of_version t.version;
          offset;
          generation = t.generation;
        };

      (* concurrent append might happen so here t.offset might differ
         from offset *)
      let h = header t in
      if not (t.flushed ++ Int64.of_int (String.length buf) = h ++ offset) then
        Fmt.failwith "sync error: %s flushed=%Ld offset+header=%Ld\n%!" t.file
          t.flushed (offset ++ h);
      t.flushed <- offset ++ h

  let flush t =
    if t.readonly then raise RO_Not_Allowed;
    unsafe_flush t

  let auto_flush_limit = 1_000_000L

  let append t buf =
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then flush t

  let set t ~off buf =
    if t.readonly then raise RO_Not_Allowed;
    unsafe_flush t;
    Raw.unsafe_write t.raw ~off:(header t ++ off) buf

  let read t ~off buf =
    assert (if not t.readonly then header t ++ off <= t.flushed else true);
    Raw.unsafe_read t.raw ~off:(header t ++ off) ~len:(Bytes.length buf) buf

  let offset t = t.offset

  let force_offset t =
    t.offset <- Raw.Offset.get t.raw;
    t.offset

  let generation t = t.generation

  let force_generation t =
    t.generation <- Raw.Generation.get t.raw;
    t.generation

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
        k ())
    in
    aux dirname (fun () -> ())

  let raw ~mode ~version file =
    let x = Unix.openfile file Unix.[ O_CREAT; mode; O_CLOEXEC ] 0o644 in
    let raw = Raw.v x in
    let header = { Raw.Header.version; offset = 0L; generation = 0L } in
    Raw.Header.set raw header;
    raw

  let clear t =
    Log.debug (fun l -> l "clear %s" t.file);
    if t.readonly then invalid_arg "Read-only IO cannot be cleared";
    Buffer.clear t.buf;
    (* no-op if the file is already empty; this is to avoid bumping
       the version number when this is not necessary. *)
    if t.offset = 0L then ()
    else (
      t.offset <- 0L;
      t.flushed <- header t;
      if t.version = `V1 then t.version <- `V2;
      t.generation <- Int64.succ t.generation)

  let v ~version ~fresh ~readonly file =
    let v ~offset ~version ~generation raw =
      {
        version;
        file;
        offset;
        raw;
        readonly;
        buf = Buffer.create (4 * 1024);
        flushed = header_of_version version ++ offset;
        generation;
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let raw = raw ~mode ~version:(bin_of_version version) file in
        v ~offset:0L ~version ~generation:0L raw
    | true -> (
        let x = Unix.openfile file Unix.[ O_EXCL; mode; O_CLOEXEC ] 0o644 in
        let raw = Raw.v x in
        if fresh then (
          let headers =
            {
              Raw.Header.version = bin_of_version version;
              offset = 0L;
              generation = 0L;
            }
          in
          Raw.Header.set raw headers;
          v ~offset:0L ~version ~generation:0L raw)
        else
          let { Raw.Header.version; offset; generation } = Raw.Header.get raw in
          match version_of_bin version with
          | Some `V1 -> v ~offset ~version:`V1 ~generation:0L raw
          | Some version -> v ~offset ~version ~generation raw
          | None ->
              let pp_full_version ppf v =
                Fmt.pf ppf "%a (%S)" pp_version v (bin_of_version v)
              in
              Fmt.failwith "invalid version: got %S, expecting %a" version
                Fmt.(Dump.list pp_full_version)
                [ `V1; `V2 ])

  let close t = Raw.close t.raw
end

let ( // ) = Filename.concat

let with_cache ~v ~clear ~valid file =
  let files = Hashtbl.create 13 in
  let cached_constructor extra_args ?(version = `V2) ?(fresh = false)
      ?(readonly = false) root =
    let file = root // file in
    if fresh && readonly then invalid_arg "Read-only IO cannot be fresh";
    try
      if not (Sys.file_exists file) then (
        Log.debug (fun l ->
            l "[%s] does not exist anymore, cleaning up the fd cache"
              (Filename.basename file));
        Hashtbl.remove files (file, true);
        Hashtbl.remove files (file, false);
        raise Not_found);
      let t = Hashtbl.find files (file, readonly) in
      if valid t then (
        Log.debug (fun l -> l "%s found in cache" file);
        if fresh then clear t;
        t)
      else (
        Hashtbl.remove files (file, readonly);
        raise Not_found)
    with Not_found ->
      Log.debug (fun l ->
          l "[%s] v version=%a fresh=%b readonly=%b" (Filename.basename file)
            pp_version version fresh readonly);
      let t = v extra_args ~version ~fresh ~readonly file in
      if fresh then clear t;
      Hashtbl.add files (file, readonly) t;
      t
  in
  `Staged cached_constructor
