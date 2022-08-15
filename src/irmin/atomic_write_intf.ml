(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Store_properties

type 'a diff = 'a Diff.t

module type S = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read, update and
      remove elements, with atomically guarantees. *)

  type t
  (** The type for atomic-write backend stores. *)

  include Read_only.S with type _ t := t
  (** @inline *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k] is not
      already defined in [t], create a fresh binding. Raise [Invalid_argument]
      if [k] is the {{!Irmin.Path.S.empty} empty path}. *)

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

  include Clearable with type _ t := t
  (** @inline *)

  include Closeable with type _ t := t
  (** @inline *)
end

module type Maker = functor (K : Type.S) (V : Type.S) -> sig
  include S with type key = K.t and type value = V.t

  include Of_config with type _ t := t
  (** @inline *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  module Check_closed_store (AW : S) : sig
    include
      S
        with type key = AW.key
         and type value = AW.value
         and type watch = AW.watch

    val make_closeable : AW.t -> t
    (** [make_closeable t] returns a version of [t] that raises {!Irmin.Closed}
        if an operation is performed when it is already closed. *)

    val get_if_open_exn : t -> AW.t
    (** [get_if_open_exn t] returns the store (without close checks) if it is
        open; otherwise raises {!Irmin.Closed} *)
  end

  module Check_closed (M : Maker) : Maker
end
