(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include Irmin.S

  val freeze :
    ?min:commit list ->
    ?max:commit list ->
    ?squash:bool ->
    ?copy_in_upper:bool ->
    ?min_upper:commit list ->
    ?heads:commit list ->
    repo ->
    unit Lwt.t

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  val layer_id : repo -> store_handle -> [ `Upper0 | `Upper1 | `Lower ] Lwt.t
  (** [layer_id t store_handle] returns the layer where an object, identified by
      its hash, is stored. *)

  val async_freeze : unit -> bool
  (** [async_freeze t] returns true if there is an ongoing freeze. To be used
      with caution, as a freeze can start (or stop) just after the test. It is
      helpful when a single freeze is called, to check whether it completed or
      not. *)

  (** These modules should not be used. They are exposed purely for testing
      purposes. *)
  module PrivateLayer : sig
    module Hook : sig
      type 'a t

      val v : ('a -> unit Lwt.t) -> 'a t
    end

    val wait_for_freeze : unit -> unit Lwt.t

    val freeze' :
      ?min:commit list ->
      ?max:commit list ->
      ?squash:bool ->
      ?copy_in_upper:bool ->
      ?min_upper:commit list ->
      ?heads:commit list ->
      ?hook:[ `After_Clear | `Before_Clear | `Before_Copy ] Hook.t ->
      repo ->
      unit Lwt.t

    val upper_in_use : repo -> [ `Upper0 | `Upper1 ]
  end
end

module type S_MAKER = functor
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  (H : Irmin.Hash.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

module Stats = Stats
