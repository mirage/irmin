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

module type S = sig
  val entries : int
  val stable_hash : int
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
end

val fresh : Irmin.Backend.Conf.t -> bool
val lru_size : Irmin.Backend.Conf.t -> int
val index_log_size : Irmin.Backend.Conf.t -> int
val readonly : Irmin.Backend.Conf.t -> bool
val merge_throttle : Irmin.Backend.Conf.t -> merge_throttle
val freeze_throttle : Irmin.Backend.Conf.t -> freeze_throttle
val root : Irmin.Backend.Conf.t -> string

val init :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  ?merge_throttle:merge_throttle ->
  ?freeze_throttle:freeze_throttle ->
  string ->
  Irmin.config
