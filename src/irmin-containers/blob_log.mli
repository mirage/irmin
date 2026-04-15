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

(** The implementation of log in which it is maintained as a single unit, or
    blob. Hence, two versions of the log cannot share their common predecessor.
    The type of values to be stored as well as a method to obtain timestamps are
    provided by the user.

    Merging does the following: the newer entries from each branch, with respect
    to the least common ancestor, are taken, merged and then appended in front
    of the LCA. *)

(** Signature of [Blob_log] *)
module type S = sig
  module Store : Irmin.KV
  (** Store for the log. All store related operations like branching, cloning,
      merging, etc are done through this module. *)

  type value
  (** Type of log entry *)

  val append : path:Store.path -> Store.t -> value -> unit
  (** Append an entry to the log *)

  val read_all : path:Store.path -> Store.t -> value list
  (** Read the entire log *)
end

(** [Make] returns a mergeable blob log using the backend and other parameters
    as specified by the user. *)
module Make (Backend : Irmin.KV_maker) (T : Time.S) (V : Irmin.Type.S) :
  S with type value = V.t

(** Blob log instantiated using the {{!Irmin_fs_unix} FS backend} provided by
    [Irmin_fs_unix] and the timestamp method {!Time.Unix} *)
module FS (V : Irmin.Type.S) : S with type value = V.t

(** Blob log instantiated using the {{!Irmin_mem} in-memory backend} provided by
    [Irmin_mem] and the timestamp method {!Time.Unix} *)
module Mem (V : Irmin.Type.S) : S with type value = V.t
