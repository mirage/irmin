(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *                         Ioana Cristescu <ioana@tarides.com>
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

val config :
  ?conf:Irmin.config ->
  ?lower_root:string ->
  ?upper_root1:string ->
  ?upper_root0:string ->
  ?keep_max:bool ->
  string ->
  Irmin.config
(** Setting up the configuration for a layered store. *)

(** Signature for layer stores. *)
module type STORE = sig
  include Irmin.S

  val freeze :
    ?min:commit list ->
    ?max:commit list ->
    ?squash:bool ->
    ?keep_max:bool ->
    ?heads:commit list ->
    ?recovery:bool ->
    repo ->
    unit Lwt.t
  (** [freeze ~min ~max ~squash ~heads t] copies the upper layer of [t] into the
      lower layer and clears the upper.

      If [squash] then copy the commits, nodes and contents of the [max]
      commits. Otherwise, copy the closure of commits, nodes and contents
      between the commits [min] and [max].

      If [max] is empty then the current [heads] are used instead.

      If [keep_max] then keep max (and its contents and nodes) as duplicates in
      upper. *)

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  val layer_id : repo -> store_handle -> string Lwt.t
  (** [layer_id t store_handle] returns the layer where an object, identified by
      its hash, is stored. *)

  val async_freeze : unit -> bool
  (** [async_freeze t] returns true if there is an ongoing freeze. To be used
      with caution, as a freeze can start (or stop) just after the test. It is
      helpful when a single freeze is called, to check whether it completed or
      not. *)

  val upper_in_use : repo -> string
  (** [upper_in_use t] returns the name of the upper currently used. *)

  (** These modules should not be used. They are exposed purely for testing
      purposes. *)
  module PrivateLayer : sig
    module Hook : sig
      type 'a t

      val v : ('a -> unit Lwt.t) -> 'a t
    end

    val wait_for_freeze : unit -> unit Lwt.t

    val freeze_with_hook :
      ?min:commit list ->
      ?max:commit list ->
      ?squash:bool ->
      ?keep_max:bool ->
      ?heads:commit list ->
      ?recovery:bool ->
      ?hook:[ `After_Copy | `After_PostCopy | `Before ] Hook.t ->
      repo ->
      unit Lwt.t
  end
end

module Make_ext
    (L : Irmin.S)
    (U : Irmin.S
           with type step = L.step
            and type key = L.key
            and type metadata = L.metadata
            and type contents = L.contents
            and type hash = L.hash
            and type branch = L.branch
            and type Private.Node.value = L.Private.Node.value
            and type Private.Commit.value = L.Private.Commit.value) :
  STORE
    with type step = U.step
     and type contents = U.contents
     and type key = U.key
     and type branch = U.branch
     and type metadata = U.metadata
     and type hash = U.hash

module Make
    (Make : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) :
  STORE
    with type step = P.step
     and type contents = C.t
     and type key = P.t
     and type branch = B.t
     and type metadata = M.t
     and type hash = H.t

(** [L_MAKER] is the signature exposed by any backend providing {!LAYERED_STORE}
    implementations. It has the same arguments as [S_MAKER]. *)
module type L_MAKER = functor
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  (H : Irmin.Hash.S)
  ->
  STORE
    with type step = P.step
     and type contents = C.t
     and type key = P.t
     and type branch = B.t
     and type metadata = M.t
     and type hash = H.t
