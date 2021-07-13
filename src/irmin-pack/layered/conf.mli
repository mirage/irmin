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

type config := Irmin.Private.Conf.t

module Pack : module type of Irmin_pack.Conf
include Irmin.Private.Conf.S

module Key : sig
  val lower_root : string key
  val upper_root1 : string key
  val upper_root0 : string key
  val with_lower : bool key
  val blocking_copy_size : int key
end

val lower_root : config -> string
val upper_root0 : config -> string
val upper_root1 : config -> string
val with_lower : config -> bool
val blocking_copy_size : config -> int

val init :
  ?lower_root:string ->
  ?upper_root1:string ->
  ?upper_root0:string ->
  ?with_lower:bool ->
  ?blocking_copy_size:int ->
  config ->
  config
