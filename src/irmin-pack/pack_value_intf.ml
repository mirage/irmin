(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

type length_header = [ `Varint ] option

module type S = sig
  include Irmin.Type.S

  type hash
  type key
  type kind

  val hash : t -> hash
  val kind : t -> kind

  val length_header : kind -> length_header
  (** Describes the length header formats for the {i data} sections of pack
      entries. *)

  val weight : t -> int
  (** [weight t] is the [t]'s LRU weight. *)

  val encode_bin :
    dict:(string -> int option) ->
    offset_of_key:(key -> int63 option) ->
    hash ->
    t Irmin.Type.encode_bin

  val decode_bin :
    dict:(int -> string option) ->
    key_of_offset:(int63 -> key) ->
    key_of_hash:(hash -> key) ->
    t Irmin.Type.decode_bin

  val decode_bin_length : string -> int -> int
end

module type Persistent = sig
  type hash

  include S with type hash := hash and type key = hash Pack_key.t
end

module type T = sig
  type t
end

(* A subset of [Irmin_pack.Conf.S] relevant to the format of pack entries,
   copied here to avoid cyclic dependencies. *)
module type Config = sig
  val contents_length_header : length_header
end

module type Sigs = sig
  module Kind : sig
    type t =
      | Commit_v1
      | Commit_v2
      | Contents
      | Inode_v1_unstable
      | Inode_v1_stable
      | Inode_v2_root
      | Inode_v2_nonroot
      | Dangling_parent_commit
    [@@deriving irmin]

    val all : t list
    val to_enum : t -> int
    val to_magic : t -> char
    val of_magic_exn : char -> t
    val pp : t Fmt.t

    val length_header_exn : t -> length_header
    (** Raises an exception on [Contents], as the availability of a length
        header is user defined. *)
  end

  module type S = S with type kind := Kind.t
  module type Persistent = Persistent with type kind := Kind.t
  module type Config = Config

  module Of_contents
      (_ : Config)
      (Hash : Irmin.Hash.S)
      (Key : T)
      (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t and type key = Key.t

  module Of_commit
      (Hash : Irmin.Hash.S)
      (Key : Irmin.Key.S with type hash = Hash.t)
      (Commit : Irmin.Commit.Generic_key.S
                  with type node_key = Key.t
                   and type commit_key = Key.t) :
    S with type t = Commit.t and type hash = Hash.t and type key = Key.t
end
