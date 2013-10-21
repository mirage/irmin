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

(** Tags *)

(** Main signature *)
module type S = sig

  include IrminBase.S

  (** Convert a tag to a suitable name *)
  val to_string: t -> string

  (** Convert a name to a tag *)
  val of_string: string -> t

end

module Simple: S with type t = private string

module type SYNC = sig

  type t
  (** Type for remote Irminsule connections. *)

  type tag
  (** Type for tags. *)

  type path
  (** Type for paths. *)

  type revision
  (** Type for revision. *)

  type tree
  (** Type for trees. *)

  val watch: t -> (tag * path) -> (revision -> path -> tree -> unit Lwt.t) -> unit Lwt.t
  (** Watch for changes for a given set of tags and path of
      labels. Call a callback on ([revision] * [path'] * [tree]),
      where [revision] is the new revision pointed by [tag] where the
      subtree [tree] pointing by [path'] has been updated. Note:
      [path] is necessary a sprefix of [path']. *)

end
