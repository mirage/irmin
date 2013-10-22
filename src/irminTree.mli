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

module type STORE = sig

  (** Tree stores. *)

  type tree
  (** Type of tree nodes. *)

  include IrminStore.S

  val create: t -> ?value:key -> (string * key) list -> tree
  (** Create a new node. *)

  val value: tree ->  (key * value Lwt.t) option
  (** Return the contents. *)

  val children: tree -> (string * key * tree Lwt.t) list
  (** Return the child nodes. *)

  val subtree: tree -> string list -> tree option Lwt.t
  (** Find a subtree. *)

  val add: tree -> string list -> value -> tree Lwt.t
  (** Add a value. *)

  val find: tree -> string list -> value option Lwt.t
  (** Find a value. *)

  val remove: tree -> string list -> tree Lwt.t
  (** Remove a value. *)

  val mem: tree -> string list -> bool Lwt.t
  (** Is a path valid. *)

  val iter: (string list -> value -> unit Lwt.t) -> tree -> unit Lwt.t
  (** Iter on all tree nodes containing a value. *)

  val iter_all: (string list -> value option -> unit Lwt.t) -> tree -> unit Lwt.t
  (** Iter on all tree nodes. *)

end

module Make
    (S: IrminStore.RAW)
    (K: IrminKey.S)
    (V: IrminValue.STORE with type key = K.t):
  STORE with type key = K.t
         and type value = V.value
