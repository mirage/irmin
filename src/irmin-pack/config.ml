module type S = sig
  val entries : int

  val stable_hash : int
end

module Default = struct
  let fresh = false

  let lru_size = 100_000

  let index_log_size = 500_000

  let readonly = false

  let index_throttle = `Block_writes
end

let fresh_key =
  Irmin.Private.Conf.key ~doc:"Start with a fresh disk." "fresh"
    Irmin.Private.Conf.bool Default.fresh

let lru_size_key =
  Irmin.Private.Conf.key ~doc:"Size of the LRU cache for pack entries."
    "lru-size" Irmin.Private.Conf.int Default.lru_size

let index_log_size_key =
  Irmin.Private.Conf.key ~doc:"Size of index logs." "index-log-size"
    Irmin.Private.Conf.int Default.index_log_size

let readonly_key =
  Irmin.Private.Conf.key ~doc:"Start with a read-only disk." "readonly"
    Irmin.Private.Conf.bool Default.readonly

let throttle_converter =
  let parse = function
    | "Block_writes" -> Ok `Block_writes
    | "Overcommit_memory" -> Ok `Overcommit_memory
    | s ->
        Fmt.error_msg
          "invalid %s, expected one of: Block_writes Overcommit_memory" s
  in
  let print =
    Fmt.of_to_string (function
      | `Block_writes -> "Block_writes"
      | `Overcommit_memory -> "Overcommit_memory")
  in
  (parse, print)

let index_throttle_key =
  Irmin.Private.Conf.key
    ~doc:"Strategy to use for large writes when index caches are full."
    "index-throttle" throttle_converter Default.index_throttle

let fresh config = Irmin.Private.Conf.get config fresh_key

let lru_size config = Irmin.Private.Conf.get config lru_size_key

let readonly config = Irmin.Private.Conf.get config readonly_key

let index_log_size config = Irmin.Private.Conf.get config index_log_size_key

let index_throttle config = Irmin.Private.Conf.get config index_throttle_key

let root_key = Irmin.Private.Conf.root

let root config =
  match Irmin.Private.Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let v ?(fresh = Default.fresh) ?(readonly = Default.readonly)
    ?(lru_size = Default.lru_size) ?(index_log_size = Default.index_log_size)
    ?(index_throttle = Default.index_throttle) root =
  let config = Irmin.Private.Conf.empty in
  let config = Irmin.Private.Conf.add config fresh_key fresh in
  let config = Irmin.Private.Conf.add config root_key (Some root) in
  let config = Irmin.Private.Conf.add config lru_size_key lru_size in
  let config =
    Irmin.Private.Conf.add config index_log_size_key index_log_size
  in
  let config = Irmin.Private.Conf.add config readonly_key readonly in
  let config =
    Irmin.Private.Conf.add config index_throttle_key index_throttle
  in
  config
