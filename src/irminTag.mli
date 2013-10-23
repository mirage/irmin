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

module type S = sig

  (** Signature for tags. Tags are supposed to be easily convertible
      to and from string. *)

  include IrminBase.S

  val to_string: t -> string
  (** Convert a tag to a suitable name *)

  val of_string: string -> t
  (** Convert a name to a tag *)

end

module Simple: S with type t = private string
(** Simple tags. *)

(** {2 Store} *)

module type STORE = sig

  (** The *tag store* is a key/value store, where keys are names
      created by users (and/or global names created by convention) and
      values are keys from the low-level data-store. The tag data-store
      is neither immutable nor consistent, so it is very different from
      the low-level one.

      A typical Irminsule application should have a very low number of
      keys, are this store is not supposed to be really efficient.  *)

  type t
  (** Type of tags. *)

  type key
  (** Type of keys. *)

  val set: t -> key -> unit Lwt.t
  (** Update a tag. If the tag does not exist before, just create a
      new tag. *)

  val remove: t -> unit Lwt.t
  (** Remove a tag. *)

  val read: t -> key option Lwt.t
  (** Read a tag. Return [None] if the tag is not defined. *)

  val read_exn: t -> key Lwt.t
  (** Read a tag. Raise [Not_found t] if the tag is not defined. *)

  val list: unit -> t list Lwt.t
  (** List all the available tags. *)

end

module Make (S: IrminStore.MRAW) (T: S) (K: IrminKey.S with type t = S.key)
  : STORE with type key = S.key
           and type t = T.t
