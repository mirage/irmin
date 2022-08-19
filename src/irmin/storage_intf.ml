(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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
  type t
  type key
  type value

  val v : Conf.t -> t Lwt.t
  (** [v config] initialises a storage layer, with the configuration [config]. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val keys : t -> key list Lwt.t
  (** [keys t] it the list of keys in [t]. *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] sets the contents of [k] to [v] in [t]. *)

  val remove : t -> key -> unit Lwt.t
  (** [remove t k] removes the key [k] in [t]. *)

  val batch : t -> (t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the operations in [f] in a batch. The exact guarantees
      depend on the implementation. *)

  val clear : t -> unit Lwt.t
  (** [clear t] clears the storage. This operation is expected to be slow. *)

  val close : t -> unit Lwt.t
  (** [close t] frees up all the resources associated with [t]. *)
end

module type Make = functor (Key : Type.S) (Value : Type.S) ->
  S with type key = Key.t and type value = Value.t

module type Sigs = sig
  module type S = S
  (** [S] is a storage layer that can be used to build Irmin stores. *)

  module type Make = Make
  (** [Make] parameterizes a storage layer over a key [Key] and a value [Value].
      This is the signature to implement when building custom storage for Irmin. *)
end
