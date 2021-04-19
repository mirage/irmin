include Key_intf

module Make (H : Hash.S) = struct
  module Hash = H

  type hash = H.t
  type 'a t = { hash : H.t; mutable value : 'a option } [@@deriving irmin]

  let pp_hash = Type.pp H.t

  let pp pp_v ppf t =
    match t.value with
    | None -> Fmt.pf ppf "[%a]" pp_hash t.hash
    | Some id -> Fmt.pf ppf "[%a:%a]" pp_hash t.hash pp_v id

  let t : type a. a Type.t -> a t Type.t =
   fun v ->
    let pre_hash =
      let f = Type.(unstage (pre_hash H.t)) in
      Type.stage (fun t -> f t.hash)
    in
    Type.like (t v) ~pre_hash ~pp:(pp (Type.pp v))

  let hash t = t.hash
  let value t = t.value
  let v hash = { hash; value = None }
  let of_value v hash = { hash; value = Some v }
  let clear t = t.value <- None
  let set t m = t.value <- Some m
end

module Id (H : Hash.S) = struct
  module Hash = H

  type t = H.t [@@deriving irmin]
  type hash = Hash.t

  let hash x = x [@@inline]
  let v x = x [@@inline]
end

module Mono (P : Poly) (V : Type.S) = struct
  type t = V.t P.t [@@deriving irmin]
  type hash = P.hash

  let hash = P.hash
  let v = P.v
end
