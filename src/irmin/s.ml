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

(** Irmin signatures *)

open! Import

type config = Conf.t

module Store_properties = struct
  module type BATCH = sig
    type 'a t

    val batch : read t -> ([ read | write ] t -> 'a Lwt.t) -> 'a Lwt.t
    (** [batch t f] applies the writes in [f] in a separate batch. The exact
        guarantees depend on the implementation. *)
  end

  module type CLOSEABLE = sig
    type 'a t

    val close : 'a t -> unit Lwt.t
    (** [close t] frees up all the resources associated with [t]. Any operations
        run on a closed handle will raise {!Closed}. *)
  end

  module type OF_CONFIG = sig
    type 'a t

    val v : config -> read t Lwt.t
    (** [v config] is a function returning fresh store handles, with the
        configuration [config], which is provided by the backend. *)
  end

  module type CLEARABLE = sig
    type 'a t

    val clear : 'a t -> unit Lwt.t
    (** Clear the store. This operation is expected to be slow. *)
  end
end

open Store_properties

module type CONTENT_ADDRESSABLE_STORE = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read and add
      new values. Keys are derived from the values raw contents and hence are
      deterministic. *)

  type 'a t
  (** The type for content-addressable backend stores. The ['a] phantom type
      carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val add : [> write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the responsibility of the
      content-addressable store to generate a consistent key. *)

  val unsafe_add : [> write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows to specify the key directly. The backend might
      choose to discared that key and/or can be corrupt if the key scheme is not
      consistent. *)

  include CLEARABLE with type 'a t := 'a t
end

module type CONTENT_ADDRESSABLE_STORE_MAKER = functor
  (K : Hash.S)
  (V : Type.S)
  -> sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t
  include BATCH with type 'a t := 'a t
  include OF_CONFIG with type 'a t := 'a t
  include CLOSEABLE with type 'a t := 'a t
end

module type APPEND_ONLY_STORE = sig
  (** {1 Append-only stores}

      Append-onlye stores are store where it is possible to read and add new
      values. *)

  type 'a t
  (** The type for append-only backend stores. The ['a] phantom type carries
      information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val add : [> write ] t -> key -> value -> unit Lwt.t
  (** Write the contents of a value to the store. *)

  include CLEARABLE with type 'a t := 'a t
end

module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t
  include BATCH with type 'a t := 'a t
  include OF_CONFIG with type 'a t := 'a t
  include CLOSEABLE with type 'a t := 'a t
end

module type METADATA = sig
  type t [@@deriving irmin]
  (** The type for metadata. *)

  val merge : t Merge.t
  (** [merge] is the merge function for metadata. *)

  val default : t
  (** The default metadata to attach, for APIs that don't care about metadata. *)
end

type 'a diff = 'a Diff.t

module type ATOMIC_WRITE_STORE = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read, update and
      remove elements, with atomically guarantees. *)

  type t
  (** The type for atomic-write backend stores. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k] is not
      already defined in [t], create a fresh binding. Raise [Invalid_argument]
      if [k] is the {{!Path.empty} empty path}. *)

  val test_and_set :
    t -> key -> test:value option -> set:value option -> bool Lwt.t
  (** [test_and_set t key ~test ~set] sets [key] to [set] only if the current
      value of [key] is [test] and in that case returns [true]. If the current
      value of [key] is different, it returns [false]. [None] means that the
      value does not have to exist or is removed.

      {b Note:} The operation is guaranteed to be atomic. *)

  val remove : t -> key -> unit Lwt.t
  (** [remove t k] remove the key [k] in [t]. *)

  val list : t -> key list Lwt.t
  (** [list t] it the list of keys in [t]. *)

  type watch
  (** The type of watch handlers. *)

  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers and returns
      the watch handler to be used with {!unwatch}. [init] is the optional
      initial values. It is more efficient to use {!watch_key} to watch only a
      single given key.*)

  val watch_key :
    t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch handlers for
      the key [k] and returns the watch handler to be used with {!unwatch}.
      [init] is the optional initial value of the key. *)

  val unwatch : t -> watch -> unit Lwt.t
  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)

  include CLOSEABLE with type _ t := t
  include CLEARABLE with type _ t := t
end

module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t
  include OF_CONFIG with type _ t := t
end

type remote = ..
