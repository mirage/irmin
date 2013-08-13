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

(** Datamodel for Irminsule backends *)

(** Channels *)
module type CHANNEL = sig
  (** Abstract type for channels *)
  type t

  (** Read [n] bytes on a channel *)
  val read: t -> int -> Cstruct.t Lwt.t

  (** Read [n] bytes on a channel *)
  val read_string: t -> int -> string Lwt.t

  (** Write on a channel *)
  val write: t -> Cstruct.t -> unit Lwt.t

  (** Write on a channel *)
  val write_string: t -> string -> unit Lwt.t

end

(** Base types *)
module type BASE = sig
  (** Abstract type *)
  type t

  (** Pretty-printing *)
  val to_string: t -> string

  (** Convert from JSON *)
  val of_json: IrminJSON.t -> t

  (** Convert to IrminJSON *)
  val to_json: t -> IrminJSON.t

  (** Abstract channel *)
  type channel

  (** Read from a channel *)
  val read: channel -> t Lwt.t

  (** Write to a channel *)
  val write: channel -> t -> unit Lwt.t

end

(** Keys *)
module type KEY = sig

  include BASE

  (** Compute the key associated to any value. *)
  val create: 'a -> t

  (** Compute the key length *)
  val length: t -> int

  (** Grap of keys *)
  type graph

  module Graph: sig
    include (Graph.Sig.I
             with type t := graph
              and type V.t = t)
    include BASE
      with type t = graph
       and type channel = channel
  end

end

(** Values *)
module type VALUE = BASE

(** Tags *)
module type TAG = sig

  (** Local tags *)
  type local

  (** Remote tags *)
  type remote

  (** Type of tags *)
  type t

  (** Import a remote tag locally *)
  val local: remote -> local

  (** Export a local tag remotely *)
  val remote: local -> remote

  include BASE with type t := t

  module L: BASE
    with type t = local
     and type channel = channel

  module R: BASE
    with type t = remote
     and type channel = channel
end

(** A low-level immutable and consistent key/value
    data-store. Deterministic computation of keys + immutability
    induces two very nice properties on the database structure:

    - if you modify a value, its computed key changes as well and you
    are creating a new key/value pair; and

    - if two data-stores share the same values, they will have the
    same keys: the overall structure of the data-store only depend on
    the stored data and not on any external user choices.
*)
module type STORE = sig

  (** Database handler *)
  type t

  (** Type of keys *)
  module K: KEY

  (** Type of values *)
  module V: VALUE

  (** Add a value in the store. *)
  val write: t -> V.t -> K.t Lwt.t

  (** Read the value associated to a key. Return [None] if the key
       does not exist in the store. *)
  val read: t -> K.t -> V.t option Lwt.t

  (** Add a relation between two keys *)
  val add_edge: t -> K.t -> K.t -> unit

  (** Return the graph of keys *)
  val keys: t -> K.graph

end

(** The *tag data-store* is a key/value store, where keys are names
    created by users (and/or global names created by convention) and
    values are keys from the low-level data-store. The tag data-store
    is neither immutable nor consistent, so it is very different from
    the low-level one.
*)
module type TAG_STORE = sig

  (** Database handler *)
  type t

  (** Type of tags *)
  module T: TAG

  (** Type of keys *)
  module K: KEY

  (** Update a tag. If the tag didn't existed before, just
      create a new tag. *)
  val update: t -> T.t -> K.t -> unit Lwt.t

  (** Read a tag. Return [None] if the tag does not exist in the
      store. *)
  val read: t -> T.t -> K.t option Lwt.t

  (** List all the available tags *)
  val list: t -> (T.t * K.t) list

end

module type REMOTE = sig

  (** Abstract type for channels *)
  type channel

  (** Type of keys *)
  module K: KEY

  (** Type of tags *)
  module T: TAG

  (** [pull_keys fd roots tags] pulls changes related to a given set
      known remote [tags]. Return the transitive closure of all the
      unknown keys, with [roots] as graph roots and [tags] as graph
      sinks. If [root] is the empty list, return the complete history
      up-to the given remote tags. *)
  val pull_keys: channel -> K.t list -> T.remote list -> K.graph Lwt.t

  (** Get all the remote tags. *)
  val pull_tags: channel -> (T.remote * K.t) list Lwt.t

  (** Push changes related to a given (sub)-graph of keys, given set
      of local tags (which should belong to the graph). The does not
      modify the local tags on the remote instance. It is the user
      responsability to compute the smallest possible graph
      beforhand. *)
  val push_keys: channel -> K.graph -> (T.local * K.t) list -> unit Lwt.t

  (** Modify the local tags of the remote instance. *)
  val push_tags: channel -> (T.remote * K.t) list -> unit Lwt.t

  (** Watch for changes for a given set of tags. Return the new
      graph. Return a stream of ([tags] * [graphs]) where [tags] are
      the updated tags and [graph] the corresponding set of new keys
      (if any). *)
  val watch: channel -> T.remote list -> (T.remote list * K.graph) Lwt_stream.t Lwt.t

end
