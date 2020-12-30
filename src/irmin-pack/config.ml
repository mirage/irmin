module Conf = Irmin.Private.Conf

module type S = sig
  val entries : int

  val stable_hash : int
end

module Default = struct
  let fresh = false

  let lru_size = 100_000

  let index_log_size = 500_000

  let readonly = false

  let merge_throttle = `Block_writes

  let freeze_throttle = `Block_writes
end

let fresh_key =
  Conf.key ~doc:"Start with a fresh disk." "fresh" Conf.bool Default.fresh

let lru_size_key =
  Conf.key ~doc:"Size of the LRU cache for pack entries." "lru-size" Conf.int
    Default.lru_size

let index_log_size_key =
  Conf.key ~doc:"Size of index logs." "index-log-size" Conf.int
    Default.index_log_size

let readonly_key =
  Conf.key ~doc:"Start with a read-only disk." "readonly" Conf.bool
    Default.readonly

type merge_throttle = [ `Block_writes | `Overcommit_memory ] [@@deriving irmin]

let merge_throttle_converter : merge_throttle Conf.converter =
  let parse = function
    | "block-writes" -> Ok `Block_writes
    | "overcommit-memory" -> Ok `Overcommit_memory
    | s ->
        Fmt.error_msg
          "invalid %s, expected one of: `block-writes' or `overcommit-memory'" s
  in
  let print =
    Fmt.of_to_string (function
      | `Block_writes -> "block-writes"
      | `Overcommit_memory -> "overcommit-memory")
  in
  (parse, print)

type freeze_throttle = [ `Block_writes | `Overcommit_memory | `Cancel_existing ]
[@@deriving irmin]

let freeze_throttle_converter : freeze_throttle Conf.converter =
  let parse = function
    | "block-writes" -> Ok `Block_writes
    | "overcommit-memory" -> Ok `Overcommit_memory
    | "cancel-existing" -> Ok `Cancel_existing
    | s ->
        Fmt.error_msg
          "invalid %s, expected one of: `block-writes, `overcommit-memory' or \
           `cancel-existing'"
          s
  in
  let print =
    Fmt.of_to_string (function
      | `Block_writes -> "block-writes"
      | `Overcommit_memory -> "overcommit-memory"
      | `Cancel_existing -> "cancel-existing")
  in
  (parse, print)

let merge_throttle_key =
  Conf.key ~doc:"Strategy to use for large writes when index caches are full."
    "merge-throttle" merge_throttle_converter Default.merge_throttle

let freeze_throttle_key =
  Conf.key ~doc:"Strategy to use for long-running freezes." "freeze-throttle"
    freeze_throttle_converter Default.freeze_throttle

let fresh config = Conf.get config fresh_key

let lru_size config = Conf.get config lru_size_key

let readonly config = Conf.get config readonly_key

let index_log_size config = Conf.get config index_log_size_key

let merge_throttle config = Conf.get config merge_throttle_key

let freeze_throttle config = Conf.get config freeze_throttle_key

let root_key = Conf.root

let root config =
  match Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let v ?(fresh = Default.fresh) ?(readonly = Default.readonly)
    ?(lru_size = Default.lru_size) ?(index_log_size = Default.index_log_size)
    ?(merge_throttle = Default.merge_throttle)
    ?(freeze_throttle = Default.freeze_throttle) root =
  let config = Conf.empty in
  let config = Conf.add config fresh_key fresh in
  let config = Conf.add config root_key (Some root) in
  let config = Conf.add config lru_size_key lru_size in
  let config = Conf.add config index_log_size_key index_log_size in
  let config = Conf.add config readonly_key readonly in
  let config = Conf.add config merge_throttle_key merge_throttle in
  let config = Conf.add config freeze_throttle_key freeze_throttle in
  config

module Layered = struct
  module Default = struct
    let lower_root = Irmin_layers.Layer_id.to_string `Lower

    let upper0_root = Irmin_layers.Layer_id.to_string `Upper0

    let upper1_root = Irmin_layers.Layer_id.to_string `Upper1

    let copy_in_upper = false

    let with_lower = true

    let blocking_copy_size = 64
  end

  let lower_root_key =
    Conf.key ~doc:"The root directory for the lower layer." "root_lower"
      Conf.string Default.lower_root

  let lower_root conf = Conf.get conf lower_root_key

  let upper_root1_key =
    Conf.key ~doc:"The root directory for the upper layer." "root_upper"
      Conf.string Default.upper1_root

  let upper_root1 conf = Conf.get conf upper_root1_key

  let upper_root0_key =
    Conf.key ~doc:"The root directory for the secondary upper layer."
      "root_second" Conf.string Default.upper0_root

  let upper_root0 conf = Conf.get conf upper_root0_key

  let copy_in_upper_key =
    Conf.key ~doc:"Copy the max commits in upper after a freeze."
      "copy_in_upper" Conf.bool false

  let copy_in_upper conf = Conf.get conf copy_in_upper_key

  let with_lower_key =
    Conf.key ~doc:"Use a lower layer." "with-lower" Conf.bool Default.with_lower

  let with_lower conf = Conf.get conf with_lower_key

  let blocking_copy_size_key =
    Conf.key
      ~doc:
        "Specify the maximum size (in bytes) that can be copied in the \
         blocking portion of the freeze."
      "blocking-copy" Conf.int Default.blocking_copy_size

  let blocking_copy_size conf = Conf.get conf blocking_copy_size_key

  let v ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
      ?(upper_root1 = Default.upper1_root) ?(upper_root0 = Default.upper0_root)
      ?(copy_in_upper = Default.copy_in_upper)
      ?(with_lower = Default.with_lower)
      ?(blocking_copy_size = Default.blocking_copy_size) () =
    let config = Conf.add conf lower_root_key lower_root in
    let config = Conf.add config upper_root1_key upper_root1 in
    let config = Conf.add config upper_root0_key upper_root0 in
    let config = Conf.add config copy_in_upper_key copy_in_upper in
    let config = Conf.add config with_lower_key with_lower in
    let config = Conf.add config blocking_copy_size_key blocking_copy_size in
    config
end
