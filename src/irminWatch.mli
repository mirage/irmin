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

module Watch: sig

  (** {2 Event notifications} *)

  type watch = int
  (** Type of watches identifiers. *)

  type path = string list
  (** Type for paths. *)

  val add: t -> path -> (revision -> path -> tree option -> unit Lwt.t) -> watch Lwt.t
  (** Add a watch for changes for a given set of tags and path of
      labels. Call a callback on ([revision] * [path'] * [tree]),
      where [revision] is the new revision pointed by [tag] where
      the subtree [tree] pointing by [path'] has been updated. Note:
      [path] is necessary a sprefix of [path']. *)

  val remove: t -> path -> watch -> unit Lwt.t
  (** Remove a watch. *)

  val list: unit -> (t * path * watch) list Lwt.t
  (** List of the available watches. *)

end
