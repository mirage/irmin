(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  (** {1 Slices} *)

  type t
  (** The type for slices. *)

  type contents
  (** The type for exported contents. *)

  type node
  (** The type for exported nodes. *)

  type commit
  (** The type for exported commits. *)

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  (** The type for exported values. *)

  val empty : unit -> t Lwt.t
  (** Create a new empty slice. *)

  val add : t -> value -> unit Lwt.t
  (** [add t v] adds [v] to [t]. *)

  val iter : t -> (value -> unit Lwt.t) -> unit Lwt.t
  (** [iter t f] calls [f] on all values of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val contents_t : contents Type.t
  (** [content_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type Slice = sig
  module type S = S
  (** The signature for slices. *)

  (** Build simple slices. *)
  module Make (C : Contents.STORE) (N : Node.STORE) (H : Commit.STORE) :
    S
      with type contents = C.key * C.value
       and type node = N.key * N.value
       and type commit = H.key * H.value
end
