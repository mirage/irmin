(* Extracted from https://github.com/pqwy/lru
   Copyright (c) 2016 David Kaloper Meršinjak *)

module Make (H : Hashtbl.HashedType) : sig
  type 'a t

  val create : int -> 'a t

  val add : 'a t -> H.t -> 'a -> unit

  val find : 'a t -> H.t -> 'a

  val mem : 'a t -> H.t -> bool
end
