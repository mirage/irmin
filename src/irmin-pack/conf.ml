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

module Default = struct
  let fresh = false
  let lru_size = 100_000
  let index_log_size = 2_500_000
  let readonly = false
  let merge_throttle = `Block_writes
  let freeze_throttle = `Block_writes
end

open Irmin.Private.Conf

let spec = Spec.v "pack"

type merge_throttle = [ `Block_writes | `Overcommit_memory ] [@@deriving irmin]

type freeze_throttle = [ `Block_writes | `Overcommit_memory | `Cancel_existing ]
[@@deriving irmin]

module Key = struct
  let fresh =
    key ~spec ~doc:"Start with a fresh disk." "fresh" Irmin.Type.bool
      Default.fresh

  let lru_size =
    key ~spec ~doc:"Size of the LRU cache for pack entries." "lru-size"
      Irmin.Type.int Default.lru_size

  let index_log_size =
    key ~spec ~doc:"Size of index logs." "index-log-size" Irmin.Type.int
      Default.index_log_size

  let readonly =
    key ~spec ~doc:"Start with a read-only disk." "readonly" Irmin.Type.bool
      Default.readonly

  let merge_throttle =
    key ~spec
      ~doc:"Strategy to use for large writes when index caches are full."
      "merge-throttle" merge_throttle_t Default.merge_throttle

  let freeze_throttle =
    key ~spec ~doc:"Strategy to use for long-running freezes." "freeze-throttle"
      freeze_throttle_t Default.freeze_throttle

  let root = root spec
end

let fresh config = get config Key.fresh
let lru_size config = get config Key.lru_size
let readonly config = get config Key.readonly
let index_log_size config = get config Key.index_log_size
let merge_throttle config = get config Key.merge_throttle
let freeze_throttle config = get config Key.freeze_throttle
let root config = get config Key.root

let init ?(fresh = Default.fresh) ?(readonly = Default.readonly)
    ?(lru_size = Default.lru_size) ?(index_log_size = Default.index_log_size)
    ?(merge_throttle = Default.merge_throttle)
    ?(freeze_throttle = Default.freeze_throttle) root =
  let config = empty spec in
  let config = add config Key.root root in
  let config = add config Key.fresh fresh in
  let config = add config Key.lru_size lru_size in
  let config = add config Key.index_log_size index_log_size in
  let config = add config Key.readonly readonly in
  let config = add config Key.merge_throttle merge_throttle in
  let config = add config Key.freeze_throttle freeze_throttle in
  verify config
