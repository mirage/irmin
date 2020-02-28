module type S = sig
  type t

  val v : string -> bytes -> t

  val write : t -> bytes -> unit

  val read : t -> bytes -> unit

  val close : t -> unit
end

module Unix : S
