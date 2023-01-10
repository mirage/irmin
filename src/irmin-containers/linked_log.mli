(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

(** The linked list implementation of log. Due to the linked property, two
    versions of the log share their common predecessor. As it is a linked data
    structure, a content addressable store is required. Along with that, a
    method to obtain timestamps, a hash for the content addressable store and
    the type of values stored must also be provided. *)

(** Signature of [Linked_log] *)
module type S = sig
  include Blob_log.S
  (** @inline *)

  type cursor
  (** Type of cursor. Cursor is like a marker from which a certain number of
      entries can be read *)

  val get_cursor : path:Store.path -> Store.t -> cursor
  (** Create a new cursor over the log entires at the given path *)

  val read : num_items:int -> cursor -> value list * cursor
  (** Read at most [num_items] entries from the cursor. If the number specified
      is greater than the number of log entries from the cursor, the log is read
      till the end. If the input cursor has already reached the end, then an
      empty list is returned *)
end

(** [Make] returns a mergeable linked log using the backend and other parameters
    as specified by the user. *)
module Make
    (Backend : Irmin.KV_maker)
    (C : Stores.Content_addressable)
    (T : Time.S)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S)
    () : S with type value = V.t

(** Linked log instantiated using the {{!Irmin_fs_unix} FS backend} provided by
    [Irmin_fs_unix], timestamp method {!Time.Unix} and hash {!Irmin.Hash.SHA1} *)
module FS (C : Stores.Content_addressable) (V : Irmin.Type.S) () :
  S with type value = V.t

(** Linked log instantiated using the {{!Irmin_mem} in-memory backend} provided
    by [Irmin_mem], timestamp method {!Time.Unix} and hash {!Irmin.Hash.SHA1} *)
module Mem (C : Stores.Content_addressable) (V : Irmin.Type.S) () :
  S with type value = V.t
