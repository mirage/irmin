(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

type length_header = [ `Varint ] option

type inode_child_order =
  [ `Seeded_hash  (** use a non-crypto seeded-hash of the step *)
  | `Hash_bits  (** crypto hash the step and extract the relevant bits. *)
  | `Custom of depth:int -> bytes -> int  (** use a custom index *) ]

module type S = sig
  val entries : int
  val stable_hash : int

  val contents_length_header : length_header
  (** Describes the length header of the user's contents values when
      binary-encoded. Supported modes are:

      - [Some `Varint]: the length header is a LEB128-encoded integer at the
        very beginning of the encoded value.

      - [None]: there is no length header, and values have unknown size. NOTE:
        when using [irmin-pack] in this mode, the selected indexing strategy
        {i must} index all contents values (as recovering contents values from
        the store will require referring to the index for their length
        information). *)

  val inode_child_order : inode_child_order

  val forbid_empty_dir_persistence : bool
  (** If [true], irmin-pack raises [Failure] if it is asked to save the empty
      inode. This default is [false]. It should be set to [true] if the [Schema]
      of the store allows a hash collision between the empty inode and this
      string of length 1: ["\000"].

      See https://github.com/mirage/irmin/issues/1304 *)
end

val spec : Irmin.Backend.Conf.Spec.t

type merge_throttle = [ `Block_writes | `Overcommit_memory ] [@@deriving irmin]
type freeze_throttle = [ merge_throttle | `Cancel_existing ] [@@deriving irmin]

module Key : sig
  val fresh : bool Irmin.Backend.Conf.key
  val lru_size : int Irmin.Backend.Conf.key
  val index_log_size : int Irmin.Backend.Conf.key
  val readonly : bool Irmin.Backend.Conf.key
  val root : string Irmin.Backend.Conf.key
  val merge_throttle : merge_throttle Irmin.Backend.Conf.key
  val freeze_throttle : freeze_throttle Irmin.Backend.Conf.key
  val indexing_strategy : Indexing_strategy.t Irmin.Backend.Conf.key
end

val fresh : Irmin.Backend.Conf.t -> bool
val lru_size : Irmin.Backend.Conf.t -> int
val index_log_size : Irmin.Backend.Conf.t -> int
val readonly : Irmin.Backend.Conf.t -> bool
val merge_throttle : Irmin.Backend.Conf.t -> merge_throttle
val freeze_throttle : Irmin.Backend.Conf.t -> freeze_throttle
val root : Irmin.Backend.Conf.t -> string
val indexing_strategy : Irmin.Backend.Conf.t -> Indexing_strategy.t

val init :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  ?merge_throttle:merge_throttle ->
  ?freeze_throttle:freeze_throttle ->
  ?indexing_strategy:Indexing_strategy.t ->
  string ->
  Irmin.config
