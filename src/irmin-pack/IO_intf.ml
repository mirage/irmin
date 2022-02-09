(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

(** This interface defines a type [t] which wraps a file descriptor and provides some
    additional support for metadata fields (version, offset), and support for periodic
    flushing of data (writes to file go via an internal buffer which is flushed when the
    amount of buffered data is beyond a certain limit). *)
module type S = sig
  type t

  type path := string

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> path -> t
  (** [v ~version ~fresh ~readonly pth] creates a new IO instance. [version] must be
      [Some] if the [pth] does not point to a file. If the file exists, then [fresh] will
      cause all data to be discarded (except for metadata); if [fresh] is false, and the
      version of the file is greater than [version], an exception is thrown. [readonly]
      indicates that operations like {!append} which change the file data or metadata
      should be disallowed. 

      In order to store the metadata, a [header_size] of 16 bytes is reserved at the
      beginning of the file; reads and writes to the file at offset [off] will be
      translated to offset [off+header_size].  

      For a given [pth], there should be at most one read/write instance. There can be
      multiple readonly instances, but only one read/write instance is allowed. This is
      not currently enforced (but could be, using lock files for example).  *)

  val name : t -> string
  (** [name t] is the [path] argument provided to {!v} when [t] was created *)  

  val append : t -> string -> unit
  (** [append t s] appends [s] to the end of the file pointed to by [t]. The data is first
      placed in a buffer, and a flush to file only happens when the amount of data in the
      buffer exceeds the [auto_flush_limit], which is currently 1M.

      [append] and [set] are the only operations that write to the file, apart from
      metadata operations. *)

  val set : t -> off:int63 -> string -> unit
  (** [set t ~off s] writes the string [s] at offset [off+header_size]. *)

  val read : t -> off:int63 -> bytes -> int
  (** [read t ~off buf] tries to read [Bytes.length buf] bytes from [t] at offset [off +
      header_size]; it returns the number of bytes actually read, which may be less than
      requested if we are near the end of the file *)

  val read_buffer : t -> off:int63 -> buf:bytes -> len:int -> int
  (** [read_buffer t ~off ~buf ~len] tries to read [len] bytes from [t] at offset
      [off+header_size] *)

  val offset : t -> int63
  (** [offset t] returns the offset field of [t], which for the read/write instance is the
      position at which data is written using {!append}. *)

  val force_offset : t -> int63
  (** [force_offset t] returns the last offset for data that was reliably synced to disk
      from the internal buffer, and as a side effect it updates the [t.offset] field to
      this value. This should {b only} be called for read-only instances, where it enables
      the instance to detect that there is more data to read. It does not make sense to
      call this function for the read-write instance, but this is not currently
      enforced. *)

  val readonly : t -> bool
  val flush : t -> unit
  val close : t -> unit

  val exists : string -> bool
  (** [exists pth] is just [Sys.file_exists pth]; it does not check that the [pth] points
      to a valid backing file that can be opened with {!v}. FIXME why include in the
      interface? *)

  val size : t -> int
  (** Returns the real size of the underlying file, including the header data. *)

  val truncate : t -> unit
  (** [truncate t] sets the internal field [t.offset] to 0 and [t.flushed] to point just
      after the header data. The underlying file is not truncated - the data is still
      there. A future flush will persist [t.offset] in the underlying file header.

      [t.flushed] is an internal field that is used to trigger flushing from the internal
      buffer *)

  (* {2 Versioning} *)

  val version : t -> Version.t
  val set_version : t -> Version.t -> unit
end

module type Sigs = sig
  module type S = S

  module Unix : S

  module Cache : sig
    type ('a, 'v) t = {
      v : 'a -> ?fresh:bool -> ?readonly:bool -> string -> 'v;
    }

    val memoize :
      v:('a -> fresh:bool -> readonly:bool -> string -> 'v) ->
      clear:('v -> unit) ->
      valid:('v -> bool) ->
      (root:string -> string) ->
      ('a, 'v) t
  end
end
