(* module type Poly = sig
 *   type 'a t [@@deriving irmin]
 *   (\** The type of keys.*\)
 * 
 *   type hash
 *   (\** The type of hashes. *\)
 * 
 *   val hash : 'a t -> hash
 *   (\** [hash t] is [t]'s hash. This function is assumed to be efficient and is
 *       called frequently (e.g. for consistency checking of the store). *\)
 * end *)

module type S = sig
  type t [@@deriving irmin]
  (** The type for keys.*)

  type hash

  val hash : t -> hash
end

module type Hash_like = sig
  include S

  val v : hash -> t
end

module type Sigs = sig
  module type S = S
  module type Hash_like = Hash_like

  (** The simplest possible [Key] implementation is just a hash of the
      corresponding value, attaching no additional metadata about the value. *)
  module Of_hash (H : Hash.S) : Hash_like with type hash = H.t
end
