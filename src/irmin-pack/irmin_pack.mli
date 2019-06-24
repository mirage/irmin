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

val config : ?fresh:bool -> string -> Irmin.config

val reset_stats : unit -> unit

val dump_stats : unit -> unit

module Make_ext
    (Metadata : Irmin.Metadata.S)
    (Contents : Irmin.Contents.S)
    (Path : Irmin.Path.S)
    (Branch : Irmin.Branch.S)
    (Hash : Irmin.Hash.S)
    (N : Irmin.Private.Node.S
         with type metadata = Metadata.t
          and type hash = Hash.t
          and type step = Path.step)
    (CT : Irmin.Private.Commit.S with type hash = Hash.t) :
  Irmin.S
  with type key = Path.t
   and type contents = Contents.t
   and type branch = Branch.t
   and type hash = Hash.t
   and type step = Path.step
   and type metadata = Metadata.t
   and type Key.step = Path.step

module Make : Irmin.S_MAKER

module KV : Irmin.KV_MAKER

module Dict : sig
  type t

  val find : t -> int -> string option Lwt.t

  val index : t -> string -> int Lwt.t

  val v : ?fresh:bool -> string -> t Lwt.t
end

module Index (H : Irmin.Hash.S) : sig
  type t

  type entry = { hash : H.t; offset : int64; len : int }

  val v : ?fresh:bool -> string -> t Lwt.t

  val find : t -> H.t -> entry option Lwt.t

  val append : t -> H.t -> off:int64 -> len:int -> unit
end

module type S = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val to_bin :
    dict:(string -> int Lwt.t) ->
    offset:(hash -> int64 option Lwt.t) ->
    t ->
    hash ->
    string Lwt.t

  val decode_bin :
    dict:(int -> string option Lwt.t) ->
    hash:(int64 -> hash Lwt.t) ->
    string ->
    int ->
    t Lwt.t
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) : sig
  include Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : ?fresh:bool -> string -> t Lwt.t
end

module Pack (K : Irmin.Hash.S) : sig
  module Make (V : S with type hash = K.t) : sig
    type 'a t

    val v : ?fresh:bool -> string -> [ `Read ] t Lwt.t

    val find : [> `Read ] t -> K.t -> V.t option Lwt.t

    val append : 'a t -> K.t -> V.t -> unit Lwt.t
  end
end
