open! Import

module type S = sig
  include Irmin.Key.S

  val to_offset : t -> int63
  val to_length : t -> int
  val null : t
  val unfindable_of_hash : hash -> t
end

module type Sigs = sig
  type kind = Direct | Indexed

  type[@ocamlformat "disable"] 'hash t = private
    | Direct             of { hash : 'hash; offset : int63; length : int }
    | Direct_blindfolded of { hash : 'hash; offset : int63 }
    | Indexed            of 'hash
  [@@deriving irmin]

  val t : kind -> 'hash Irmin.Type.t -> 'hash t Irmin.Type.t
  val v : hash:'h -> offset:int63 -> length:int -> 'h t
  val v_blindfolded : hash:'h -> offset:int63 -> 'h t

  module type S = sig
    type hash

    (** @inline *)
    include S with type t = hash t and type hash := hash
  end

  module Make (Hash : Irmin.Hash.S) : S with type hash = Hash.t
  module Make_indexed (Hash : Irmin.Hash.S) : S with type hash = Hash.t

  module type Store_spec = sig
    type ('h, _) contents_key = 'h t
    type 'h node_key = 'h t
    type 'h commit_key = 'h t
  end

  module Store_spec : Store_spec
end
