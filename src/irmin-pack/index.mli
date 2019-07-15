module Make (K : Irmin.Hash.S) : sig
  type t

  type key = K.t

  type value = int64 * int * char

  val v :
    ?fresh:bool ->
    ?read_only:bool ->
    log_size:int ->
    fan_out_size:int ->
    string ->
    t

  val clear : t -> unit

  val find : t -> key -> value option

  val mem : t -> key -> bool

  val replace : t -> key -> value -> unit

  val iter : (key -> value -> unit) -> t -> unit

  val flush : t -> unit
end
