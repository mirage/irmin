module type S = sig
  val entries : int

  val stable_hash : int
end

val fresh_key : bool Irmin.Private.Conf.key

val lru_size_key : int Irmin.Private.Conf.key

val index_log_size_key : int Irmin.Private.Conf.key

val readonly_key : bool Irmin.Private.Conf.key

val index_throttle_key :
  [ `Block_writes | `Overcommit_memory ] Irmin.Private.Conf.key

val root_key : string option Irmin.Private.Conf.key

val fresh : Irmin.Private.Conf.t -> bool

val lru_size : Irmin.Private.Conf.t -> int

val index_log_size : Irmin.Private.Conf.t -> int

val readonly : Irmin.Private.Conf.t -> bool

type throttle = [ `Block_writes | `Overcommit_memory ]

val index_throttle : Irmin.Private.Conf.t -> throttle

val freeze_throttle : Irmin.Private.Conf.t -> throttle

val root : Irmin.Private.Conf.t -> string

val v :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  ?index_throttle:throttle ->
  ?freeze_throttle:throttle ->
  string ->
  Irmin.config
