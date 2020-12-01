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
      (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
      (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
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
      (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
      (AW : Irmin.ATOMIC_WRITE_STORE_MAKER) : S_MAKER

  module Stats = Stats
end
