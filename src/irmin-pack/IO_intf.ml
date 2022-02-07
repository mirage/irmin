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

module type S = sig
  type t
  type path := string

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> path -> t
  val name : t -> string 
  (** [name t] returns the name of the underlying file, using the path string that was
      used when the [t] was created FIXME tjr: I think the name is somehow used for
      caching purposes, and to enable sharing with other store instances???  *)

  val append : t -> string -> unit
  val set : t -> off:int63 -> string -> unit
  val read : t -> off:int63 -> bytes -> int
  val read_buffer : t -> off:int63 -> buf:bytes -> len:int -> int
  val offset : t -> int63
  val force_offset : t -> int63
  (** The underlying implementation uses a {!Index_unix.Private.Raw} value which wraps a
      file descriptor and allows to get/set 4 fields of metadata which are stored at the
      beginning of the file: offset, version, generation and fan. The "offset" is the
      "offset of [t] at which new data is written". [force_offset t] forces the file seek
      offset to be that last set in the metadata (so, potentially before the actual end of
      the file) and also returns the value to the user. Any data after this is potentially
      considered garbage because it was not successfully flushed to disk. [force_offset]
      is used when recovering from a previous crash, for example. It is also used by RO
      instances- if the offset has changed in the underlying file, an RO instance can
      detect this by calling force_offset. This typically happens in the [sync] function
      of a pack store, for example. *)

  val readonly : t -> bool
  val flush : t -> unit
  val close : t -> unit
  val exists : string -> bool
  val size : t -> int

  val truncate : t -> unit
  (** Sets the length of the underlying IO to be 0, without actually purging the
      associated data. *)

  (* {2 Versioning} *)

  (** A [t] is like a file, but also includes metadata; at the moment, this metadata
      consists of a "version", see {!Version}. FIXME what does this affect, in terms of
      this interface? Or is it just that some users of [t] will behave differently
      depending on the version? *)

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
