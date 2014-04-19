(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Node-like structures of values. *)

open Core_kernel.Std

type 'key t = {
  contents: 'key option;
  succ    : 'key String.Map.t;
} with bin_io, compare, sexp
(** Type of nodes .*)

val equal: ('key -> 'key -> bool) -> 'key t -> 'key t -> bool
(** Compare two nodes. *)

val contents_exn: 'key t -> 'key
(** Get the contents key, raise [Not_found] if None. *)

val edges: 'a t -> ('a, 'b) IrminGraph.vertex list
(** The graph edges. *)

val of_json: (Ezjsonm.t -> 'a) -> Ezjsonm.t -> 'a t
val to_json: ('a -> Ezjsonm.t) -> 'a t -> Ezjsonm.t

val empty: 'key t
(** The empty node. *)

val leaf: 'key -> 'key t
(** Create a leaf node (with some contents and no successors). *)

val is_empty: 'key t -> bool
(** Is the node empty. *)

val is_leaf: 'key t -> bool
(** Is it a leaf node (see [leaf]) ? *)

module type S = sig

  (** Node values. *)

  type key
  (** Foreign keys. *)

  type nonrec t = key t

  include IrminContents.S with type t := t

end

module S (K: IrminKey.S): S with type key = K.t
(** Base functions for nodes. *)

module SHA1: S with type key = IrminKey.SHA1.t
(** Simple node implementation, where keys are SHA1s. *)

module type STORE = sig

  (** Node stores. *)

  type key
  (** Foreign keys. *)

  type contents
  (** Node contents. *)

  type value = key t
  (** Type of values. *)

  include IrminStore.AO with type key := key
                         and type value := value
  (** Node stores are append-only. *)

  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  (** Create a new node. *)

  val contents: t -> value -> contents Lwt.t option
  (** Return the node contents. *)

  val succ: t -> value -> value Lwt.t String.Map.t
  (** Return the node successors. *)

  val sub: t -> value -> IrminPath.t -> value option Lwt.t
  (** Find a subvalue. *)

  val sub_exn: t -> value -> IrminPath.t -> value Lwt.t
  (** Find a subvalue. Raise [Not_found] if it does not exist. *)

  val update: t -> value -> IrminPath.t -> contents -> value Lwt.t
  (** Add a value by recusively saving subvalues and subvalues into the
      corresponding stores. *)

  val find: t -> value -> IrminPath.t -> contents option Lwt.t
  (** Find a value. *)

  val find_exn: t -> value -> IrminPath.t -> contents Lwt.t
  (** Find a value. Raise [Not_found] is [path] is not defined. *)

  val remove: t -> value -> IrminPath.t -> value Lwt.t
  (** Remove a value. *)

  val valid: t -> value -> IrminPath.t -> bool Lwt.t
  (** Is a path valid. *)

  val merge: t -> key IrminMerge.t
  (** Merge two nodes together. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type key = key
  (** Base functions for values. *)

end

module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (Contents: IrminContents.STORE with type key = K.t and type value = C.t)
    (Node    : IrminStore.AO       with type key = K.t and type value = K.t t)
  : STORE with type t = Contents.t * Node.t
           and type key = K.t
           and type contents = C.t
(** Create a node store from an append-only database. *)
