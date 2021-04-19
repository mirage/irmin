open! Import

module type S = sig
  include Irmin.Key.S

  val to_offset : t -> int63
  val to_length : t -> int
  val null : t
  val unfindable_of_hash : hash -> t
end

module type Sigs = sig
  type 'h t [@@deriving irmin]

  val v : hash:'h -> offset:int63 -> length:int -> 'h t

  module type S = sig
    type hash

    (** @inline *)
    include S with type t = hash t and type hash := hash
  end

  module Make (Hash : Irmin.Hash.S) : S with type hash = Hash.t
end
