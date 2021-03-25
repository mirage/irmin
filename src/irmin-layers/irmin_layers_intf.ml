(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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

type layer_id = [ `Upper0 | `Upper1 | `Lower ] [@@deriving irmin]

module type S = sig
  include Irmin.S

  val freeze :
    ?min:commit list ->
    ?max:commit list ->
    ?squash:bool ->
    ?copy_in_upper:bool ->
    ?min_upper:commit list ->
    ?recovery:bool ->
    repo ->
    unit Lwt.t
  (** [freeze ?min ?max ?squash ?copy_in_upper ?min_upper ?recovery t] launches
      an asynchronous freezing operation on the repo [t] to reduce the size of
      the upper layer and discard unnecessary branches of objects (i.e. commits,
      nodes and contents).

      Let [o] be the set of objects reachable from the [max] commits and bounded
      by the [min] commits. During the freeze, all objects in [o] are copied to
      the lower layer, if there is one. Setting [squash] to [true] is equivalent
      to setting [min] to [max]. [min] defaults to the empty list and [max]
      defaults to the head commits of the repo.

      Let [o'] be the set of objects reachable from the [max] commits and
      bounded by the [min_upper] commits. When the freeze is over, if
      [copy_in_upper] is true, then the new upper layer will only contain the
      objects of [o'], otherwise the new upper layer will be empty. [min_upper]
      defaults to the empty list and [copy_in_upper] defaults to the repo's
      configuration.

      If [recovery] is true then the function will first try to recover from a
      previously interrupted freeze. See {!needs_recovery}.

      If a freeze is already ongoing, the behavior depends on the
      freeze_throttle configuration of the repo:

      - When [`Overcommit_memory], the function returns without launching a new
        freeze.
      - When [`Cancel_existing], the function blocks until the ongoing freeze
        safely cancels and then a new one is started afterwards. The time spent
        doing the canceled freeze is not completely wasted, objects copied to
        the lower layer will not have to be copied again, but objects copied to
        the next upper layer are discarded from that layer.
      - When [`Block_writes], the function blocks until the ongoing freeze ends
        and then a new one is started afterwards. *)

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  val layer_id : repo -> store_handle -> layer_id Lwt.t
  (** [layer_id t store_handle] returns the layer where an object, identified by
      its hash, is stored. *)

  val async_freeze : repo -> bool
  (** [async_freeze t] returns true if there is an ongoing freeze. To be used
      with caution, as a freeze can start (or stop) just after the test. It is
      helpful when a single freeze is called, to check whether it completed or
      not. *)

  val self_contained : ?min:commit list -> max:commit list -> repo -> unit Lwt.t
  (** [self_contained min max t] copies the commits in the range of [min, max]
      from lower into upper, in order to make the upper self contained. If [min]
      is missing then only the [max] commits are copied. *)

  val check_self_contained :
    ?heads:commit list ->
    repo ->
    ([> `Msg of string ], [> `Msg of string ]) result Lwt.t
  (** [check_self_contained ?heads] checks that the current upper layer of a
      store is self contained. *)

  val needs_recovery : repo -> bool
  (** [needs_recovery repo] detects if an ongoing freeze was interrupted during
      the last node crash. If it returns [true] then the next call to freeze
      needs to have its [recovery] flag set. *)

  (** These modules should not be used. They are exposed purely for testing
      purposes. *)
  module PrivateLayer : sig
    module Hook : sig
      type 'a t

      val v : ('a -> unit Lwt.t) -> 'a t
    end

    val wait_for_freeze : repo -> unit Lwt.t

    val freeze' :
      ?min:commit list ->
      ?max:commit list ->
      ?squash:bool ->
      ?copy_in_upper:bool ->
      ?min_upper:commit list ->
      ?recovery:bool ->
      ?hook:
        [ `After_Clear
        | `Before_Clear
        | `Before_Copy
        | `Before_Copy_Newies
        | `Before_Copy_Last_Newies
        | `Before_Flip ]
        Hook.t ->
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

module type Irmin_layers = sig
  module Layer_id : sig
    type t = layer_id [@@deriving irmin]

    val pp : Format.formatter -> t -> unit
    val to_string : t -> string
  end

  module type S = S
  module type S_MAKER = S_MAKER

  module Make_ext
      (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io := 'a Lwt.t)
      (AW : Irmin.ATOMIC_WRITE_STORE_MAKER with type 'a io := 'a Lwt.t)
      (Metadata : Irmin.Metadata.S)
      (Contents : Irmin.Contents.S)
      (Path : Irmin.Path.S)
      (Branch : Irmin.Branch.S)
      (Hash : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S
                with type metadata = Metadata.t
                 and type hash = Hash.t
                 and type step = Path.step)
      (Commit : Irmin.Private.Commit.S with type hash = Hash.t) :
    S
      with type key = Path.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type Key.step = Path.step

  module Make
      (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io := 'a Lwt.t)
      (AW : Irmin.ATOMIC_WRITE_STORE_MAKER with type 'a io := 'a Lwt.t) :
    S_MAKER

  module Stats = Stats
end
