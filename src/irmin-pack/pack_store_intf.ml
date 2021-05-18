open! Import

module type S = sig
  include Content_addressable.S

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    read t Lwt.t
end

module type Maker = sig
  type key
  type index

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)
  module Make (V : Pack_value.S with type hash := key) :
    S with type key = key and type value = V.t and type index = index
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  module Maker
      (V : Version.S)
      (Index : Pack_index.S)
      (K : Irmin.Hash.S with type t = Index.key) :
    Maker with type key = K.t and type index = Index.t
end
