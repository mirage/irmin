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

type ('a, 'b) node = {
  value   : 'a option;
  children: (string * 'b) list;
}
(** Type of concrete trees .*)

module type STORE = sig

  (** Tree stores. *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  type tree = (key, key) node
  (** Type of tree nodes. *)

  type path = string list
  (** Type of labeled path to go from one node to node. *)

  include IrminBase.S with type t := tree
  (** Tree are base types. *)

  include IrminStore.A with type key := key
                        and type value := tree
  (** Tree stores are append-onlye. *)

  val empty: tree
  (** The empty tree. *)

  val create: t -> ?value:value -> (string * tree) list -> key Lwt.t
  (** Create a new node. *)

  val value: t -> tree -> value Lwt.t option
  (** Return the contents. *)

  val children: t -> tree -> (string * tree Lwt.t) list
  (** Return the child nodes. *)

  val sub: t -> tree -> path -> tree option Lwt.t
  (** Find a subtree. *)

  val add: t -> tree -> path -> value -> tree Lwt.t
  (** Add a value by recusively saving subtrees and subvalues into the
      corresponding stores. *)

  val find: t -> tree -> path -> value Lwt.t
  (** Find a value. *)

  val remove: t -> tree -> path -> unit Lwt.t
  (** Remove a value. *)

  val mem: t -> tree -> path -> bool Lwt.t
  (** Is a path valid. *)

end

module Make
    (S: IrminStore.ARAW)
    (K: IrminKey.S with type t = S.key)
    (V: IrminValue.STORE with type key = S.key):
  STORE with type key = K.t
         and type value = V.value
(** Create a tree store implementation. *)
