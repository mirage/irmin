(*
 * Copyright (c) 2013 Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

(** Tree-like structures of values. *)

type ('a, 'b) tree = {
  value   : 'a option;
  children: (string * 'b) list;
}
(** Type of concrete trees .*)

module type STORE = sig

  (** Tree stores. *)

  type key
  (** Type of keys. *)

  type t = (key, key) tree
  (** Type of tree nodes. *)

  include IrminBase.S with type t := t
  (** Tree are base types. *)

  include IrminStore.I with type key := key
                        and type value := t
  (** Tree stores are immutable. *)

  type value
  (** Type of values. *)

  type path = string list
  (** Type of labeled path to go from one node to node. *)

  val empty: t
  (** The empty tree. *)

  val create: ?value:value -> (string * t) list -> key Lwt.t
  (** Create a new node. *)

  val value: t -> value Lwt.t option
  (** Return the contents. *)

  val children: t -> (string * t Lwt.t) list
  (** Return the child nodes. *)

  val sub: t -> path -> t option Lwt.t
  (** Find a subtree. *)

  val add: t -> path -> value -> t Lwt.t
  (** Add a value by recusively saving subtrees and subvalues into the
      corresponding stores. *)

  val find: t -> path -> value Lwt.t
  (** Find a value. *)

  val remove: t -> path -> t Lwt.t
  (** Remove a value. *)

  val mem: t -> path -> bool Lwt.t
  (** Is a path valid. *)

  val iter: (path -> value -> unit Lwt.t) -> t -> unit Lwt.t
  (** Iter on all tree nodes containing a value, top-down. *)

end

module Make
    (S: IrminStore.IRAW)
    (K: IrminKey.S with type t = S.key)
    (V: IrminValue.STORE with type key = S.key):
  STORE with type key = K.t
         and type value = V.t
(** Create a tree store implementation. *)
