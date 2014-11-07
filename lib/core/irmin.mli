(*
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

(** A library database following the same design principle as Git.

    Irmin is a distributed and history-preserving library database
    with built-in snapshot, branch and revert mechanisms. It is
    designed to use a large variety of backends. Irmin is written in
    pure OCaml and does not depend on external C stubs; it aims is to
    run everywhere, from Linux to Xen unikernels -- and can be be
    compiled to JavaScipt to run in a browser.

    Irmin uses a set of {{!Store}store signatures} describing what
    {{!RO}read-only}, {{!AO}append-only}, {{!RW}read-write} stores are
    and {{!Backend}backend functor} generating these signature,
    providing the proper store contents.

*)

(** {2 Read-only Stores} *)

module Store: sig

  module type RO = sig

    (** Read-only stores. *)

    type t
    (** Type for stores. *)

    type key
    (** Type for keys. *)

    type value
    (** Type for values. *)

    type origin
    (** Type for keep track of the provenance of operations. *)

    val create: unit -> t Lwt.t
    (** Create a store handle. The operation can be used multiple times
        as it is supposed to be very cheap (and usually
        non-blocking). *)

    val read: t -> origin -> key -> value option Lwt.t
    (** Read a value from the store. *)

    val read_exn: t -> origin -> key -> value Lwt.t
    (** Read a value from the store. Raise [Unknown k] if [k] does not
        have an associated value. *)

    val mem: t -> origin -> key -> bool Lwt.t
    (** Check if a key exists. *)

    val list: t -> origin -> key list -> key list Lwt.t
    (** Return all the keys that are allowed to access, knowing a given
        collection of keys (which might be seen as a passwords). *)

    val dump: t -> origin -> (key * value) list Lwt.t
    (** Return the store contents. *)

  end

end

(** {2 Makers} *)

module Maker: sig

  module type MAKER =
    functor (K: Tc.I0) ->
    functor (V: Tc.I0) ->
    functor (O: Tc.I0) ->
      STORE with type key = K.t and type value = V.t and type origin = O.t
  (** Signature for functor creating read-only stores. *)

  (** {2 Binary stores} *)
  module type BINARY = STORE with
    type key = Cstruct.t and type value = Cstruct.t and type origin = Cstruct.t
  (** Binary read-only stores. Keys, values and origin are cstruct
      buffers. *)

  module Binary (S: BINARY) (K: Tc.I0) (V: Tc.I0): MAKER
  (** Create a typed read-only store from a binary one. *)

  module type JSON = STORE with
    type key = Ezjsonm.t and type value = Ezjsonm.t and type origin = Ezjsonm.t
  (** Binary read-only stores. Keys, values and origin are cstruct
      buffers. *)

  module Json (S: JSON) (K: Tc.I0) (V: Tc.I0): MAKER
  (** Create a typed read-only store from a JSON one. *)

end
