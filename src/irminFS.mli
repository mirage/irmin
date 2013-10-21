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

(** Disk-related errors. *)
exception Error of string

(** Main signature *)
module type S = sig

  (** A disk store is a global store. *)
  include IrminStore.S

  (** Initialize a disk. *)
  val init: string -> unit Lwt.t

  (** Dump the disk state to stdout. *)
  val dump: t -> unit Lwt.t

end

(** Functor to create an on-disk Irminsule instance.*)
module Make (C: IrminStore.CORE) (FD: IrminChannel.S):
  S with type t = FD.t
     and module Core = C

(** Create a filesystem store, using a given directory name. *)
val create: string -> (module IrminStore.S)
