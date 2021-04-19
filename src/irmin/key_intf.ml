module type Poly = sig
  type 'a t [@@deriving irmin]
  (** The type for keys.*)

  type hash
  (** The type for hashes. *)

  val hash : 'a t -> hash
  (** [hash t] it [t]'s hash. *)

  val v : hash -> 'a t
  (** [v h] is the key which contains they hash [h]. *)
end

module type S = sig
  type t
  (** The type for keys.*)

  include Poly with type _ t := t
  (** @inline *)

  val t : t Type.t
end

module type Maker = functor (H : Hash.S) -> Poly with type hash = H.t

module type Sigs = sig
  module type S = S
  module type Poly = Poly
  module type Maker = Maker

  module Make (H : Hash.S) : sig
    include Poly with type hash = H.t

    val of_value : 'a -> hash -> 'a t

    val value : 'a t -> 'a option
    (** [metadata t] is [t]'s metadata if it has been provided to [v]. *)

    val set : 'a t -> 'a -> unit
    val clear : 'a t -> unit
  end

  module Mono (P : Poly) (V : Type.S) :
    S with type t = V.t P.t and type hash = P.hash

  module Id (H : Hash.S) : S with type t = H.t and type hash = H.t
end
