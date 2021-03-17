(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open Store_properties

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    read t Lwt.t

  include BATCH with type 'a t := 'a t
  module Key : Irmin.Hash.S with type t = key
  module Val : Irmin.Private.Inode.VAL with type t = value and type hash = key
  include S.CHECKABLE with type 'a t := 'a t and type key := key
  include CLOSEABLE with type 'a t := 'a t

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  val clear_caches : 'a t -> unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> key) -> string -> int -> int

  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t
end

(** Unstable internal API agnostic about the underlying storage. Use it only to
    implement or test inodes. *)
module type INTER = sig
  include Irmin.Private.Inode.S
  module Elt : Pack.ELT with type hash := hash and type t = Bin.t

  val decode_bin :
    dict:(int -> string option) ->
    hash:(int64 -> hash) ->
    string ->
    int ->
    int * Elt.t
end

module type Inode = sig
  module type S = S
  module type INTER = INTER

  module Make_intermediate
      (Conf : Config.S)
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S with type hash = H.t) :
    INTER
      with type hash = H.t
       and type metadata = Node.metadata
       and type step = Node.step

  module Make_ext
      (H : Irmin.Hash.S)
      (Inter : INTER with type hash = H.t)
      (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t) : sig
    include
      S
        with type key = H.t
         and type Val.metadata = Inter.metadata
         and type Val.step = Inter.step
         and type index = Pack_index.Make(H).t
         and type value = Inter.t
  end

  module Make
      (Conf : Config.S)
      (H : Irmin.Hash.S)
      (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t)
      (Node : Irmin.Private.Node.S with type hash = H.t) : sig
    include
      S
        with type key = H.t
         and type Val.metadata = Node.metadata
         and type Val.step = Node.step
         and type index = Pack_index.Make(H).t
  end
end
