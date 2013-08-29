(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Disk persistence *)

open IrminTypes

(** Disk-related errors. *)
exception Error of string

(** Main signature *)
module type S = sig

  (** Abstract type for the disk location *)
  type t

  (** Create a disk handler from a directory name *)
  val create: string -> t

  (** Initialize a disk *)
  val init: string -> unit Lwt.t

  (** Persist keys *)
  module Key_store: KEY_STORE with type t = t

  (** Persist values *)
  module Value_store: sig
    include VALUE_STORE with type t = t
    val dump: t -> unit Lwt.t
  end

  (** Persists tags *)
  module Tag_store: TAG_STORE with type t = t

end

(** Disk *)
module Disk (K: KEY) (V: VALUE with module Key = K) (T: TAG)
  : S with module Key_store.Key = K
       and module Value_store.Key = K
       and module Tag_store.Key = K
       and module Value_store.Value = V
       and module Tag_store.Tag = T
