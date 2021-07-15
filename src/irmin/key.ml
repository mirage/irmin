include Key_intf

module Of_hash (Hash : Hash.S) = struct
  type t = Hash.t [@@deriving irmin]
  type hash = Hash.t

  let v x = x [@@inline]
  let hash x = x [@@inline]
end

(* module Make (H : Hash.S) = struct
 *   module Hash = H
 * 
 *   type hash = H.t
 *   type 'a t = { hash : H.t; mutable value : 'a option } [@@deriving irmin]
 * 
 *   let pp_hash = Type.pp H.t
 * 
 *   let pp pp_v ppf t =
 *     match t.value with
 *     | None -> Fmt.pf ppf "[%a]" pp_hash t.hash
 *     | Some id -> Fmt.pf ppf "[%a:%a]" pp_hash t.hash pp_v id
 * 
 *   let t : type a. a Type.t -> a t Type.t =
 *    fun v ->
 *     let pre_hash =
 *       let f = Type.(unstage (pre_hash H.t)) in
 *       Type.stage (fun t -> f t.hash)
 *     in
 *     Type.like (t v) ~pre_hash ~pp:(pp (Type.pp v))
 * 
 *   let hash t = t.hash
 *   let value t = t.value
 *   let v hash = { hash; value = None }
 *   let of_value v hash = { hash; value = Some v }
 *   let clear t = t.value <- None
 *   let set t m = t.value <- Some m
 * end *)
