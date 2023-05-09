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

type length_header = [ `Varint ] option

type inode_child_order =
  [ `Seeded_hash | `Hash_bits | `Custom of depth:int -> bytes -> int ]

module type S = sig
  val entries : int
  val stable_hash : int
  val contents_length_header : length_header
  val inode_child_order : inode_child_order
  val forbid_empty_dir_persistence : bool
end

module Default = struct
  let fresh = false
  let lru_size = 100_000
  let lru_max_memory = None
  let index_log_size = 2_500_000
  let readonly = false
  let merge_throttle = `Block_writes
  let indexing_strategy = Indexing_strategy.default
  let use_fsync = false
  let dict_auto_flush_threshold = 1_000_000
  let suffix_auto_flush_threshold = 1_000_000
  let no_migrate = false
  let lower_root = None
end

open Irmin.Backend.Conf

let spec = Spec.v "pack"

type merge_throttle = [ `Block_writes | `Overcommit_memory ] [@@deriving irmin]

module Key = struct
  let fresh =
    key ~spec ~doc:"Start with a fresh disk." "fresh" Irmin.Type.bool
      Default.fresh

  let lru_size =
    key ~spec ~doc:"Maximum size of the LRU cache for pack entries." "lru-size"
      Irmin.Type.int Default.lru_size

  let lru_max_memory =
    key ~spec ~doc:"Maximum memory in bytes of the LRU cache for pack entries."
      "lru-max-memory"
      Irmin.Type.(option int)
      Default.lru_max_memory

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

  let root = root spec

  let lower_root =
    key ~spec ~doc:"Optional path for lower layer directory." "lower-root"
      Irmin.Type.(option string)
      Default.lower_root

  let indexing_strategy =
    let serialisable_t = [%typ: [ `Always | `Minimal ]] in
    key ~spec ~doc:"Strategy to use for adding objects to the index"
      "indexing-strategy"
      (Irmin.Type.map serialisable_t
         (function
           | `Always -> Indexing_strategy.always
           | `Minimal -> Indexing_strategy.minimal)
         (fun _ -> Fmt.failwith "Can't serialise indexing strategy"))
      Default.indexing_strategy

  let use_fsync =
    key ~spec
      ~doc:"Whether fsync should be used to ensure persistence order of files"
      "use-fsync" Irmin.Type.bool Default.use_fsync

  let dict_auto_flush_threshold =
    key ~spec ~doc:"Buffer size of the dict at which automatic flushes occur"
      "dict-auto-flush-threshold" Irmin.Type.int
      Default.dict_auto_flush_threshold

  let suffix_auto_flush_threshold =
    key ~spec ~doc:"Buffer size of the suffix at which automatic flushes occur"
      "suffix-auto-flush-threshold" Irmin.Type.int
      Default.suffix_auto_flush_threshold

  let no_migrate =
    key ~spec ~doc:"Prevent migration of V1 and V2 stores" "no-migrate"
      Irmin.Type.bool Default.no_migrate
end

let fresh config = get config Key.fresh
let lru_size config = get config Key.lru_size
let lru_max_memory config = get config Key.lru_max_memory
let readonly config = get config Key.readonly
let index_log_size config = get config Key.index_log_size
let merge_throttle config = get config Key.merge_throttle

let root config =
  match find_root config with
  | None ->
      failwith
        "unintialised root, call [Irmin_pack.Conf.init root] before opening \
         the store"
  | Some root -> root

let lower_root config = get config Key.lower_root
let indexing_strategy config = get config Key.indexing_strategy
let use_fsync config = get config Key.use_fsync
let dict_auto_flush_threshold config = get config Key.dict_auto_flush_threshold

let suffix_auto_flush_threshold config =
  get config Key.suffix_auto_flush_threshold

let no_migrate config = get config Key.no_migrate

let init ?(fresh = Default.fresh) ?(readonly = Default.readonly)
    ?(lru_size = Default.lru_size) ?(lru_max_memory = Default.lru_max_memory)
    ?(index_log_size = Default.index_log_size)
    ?(merge_throttle = Default.merge_throttle)
    ?(indexing_strategy = Default.indexing_strategy)
    ?(use_fsync = Default.use_fsync)
    ?(dict_auto_flush_threshold = Default.dict_auto_flush_threshold)
    ?(suffix_auto_flush_threshold = Default.suffix_auto_flush_threshold)
    ?(no_migrate = Default.no_migrate) ?(lower_root = Default.lower_root) root =
  let config = empty spec in
  let config = add config Key.root root in
  let config = add config Key.lower_root lower_root in
  let config = add config Key.fresh fresh in
  let config = add config Key.lru_size lru_size in
  let config = add config Key.lru_max_memory lru_max_memory in
  let config = add config Key.index_log_size index_log_size in
  let config = add config Key.readonly readonly in
  let config = add config Key.merge_throttle merge_throttle in
  let config = add config Key.indexing_strategy indexing_strategy in
  let config = add config Key.use_fsync use_fsync in
  let config =
    add config Key.dict_auto_flush_threshold dict_auto_flush_threshold
  in
  let config =
    add config Key.suffix_auto_flush_threshold suffix_auto_flush_threshold
  in
  let config = add config Key.no_migrate no_migrate in
  verify config
