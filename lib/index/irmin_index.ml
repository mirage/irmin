
open Lwt
open Irmin


module Log = Log.Make(struct let section = "AOI" end)


module type PERSISTANT_INDEX = sig

    type index
    type key

    val find: index -> key
    val add: index -> key -> unit
    val length_index: int
    val digest_index: Cstruct.t -> index
    val length_key: int
    val digest_key: Cstruct.t -> key

  end


module HT (* (IK:Irmin.Hash.S) *) (K:Irmin.Hash.S) = struct

  type index = K.t
  type key = K.t

  let ht:(index, key) Hashtbl.t = Hashtbl.create 99

  let find i = Hashtbl.find ht i

  let add i k =
    Hashtbl.add ht i k

  let length_index = K.length

  let digest_index = K.digest

  let length_key = K.length

  let digest_key = K.digest

end

module type PERSISTANT_INDEX_MAKER =
  (*  functor (IK:Irmin.Hash.S) -> *)
  functor (K:Irmin.Hash.S) ->
  PERSISTANT_INDEX with type index = K.t and type key = K.t


(*
module Symlink (I:H) (E:H) : PERSISTANT_INDEX = struct (* A COMPLETER *)   end
*)


module type AO_MAKER_RAW =
  functor (K: Irmin.Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = K.t and type value = V.t


module AOI (P: PERSISTANT_INDEX_MAKER) (S:AO_MAKER_RAW) (K: Irmin.Hash.S) (V: Tc.S0) =
struct


    module AO = S(K)(Irmin.Contents.Cstruct)
    module PI = P(K)
    type key = K.t

    type value = V.t

    type t = AO.t

    let to_cstruct x = Tc.write_cstruct (module V) x
    let of_cstruct x = Tc.read_cstruct (module V) x

    let create config task =
      AO.create config task

    let task t =
      AO.task t

    let read t index =
      try
        let k = PI.find index in
        AO.read t k >>= function
        | None -> return_none
        | Some v -> return (Some (of_cstruct v))
      with
      | Not_found -> return_none


    let read_exn t index =
      try
        let k = PI.find index in
        AO.read_exn t k >>=
          function v -> return (of_cstruct v)
      with
      | Not_found -> fail Not_found


    let mem t index =
      try
        let k = PI.find index in
        AO.mem t k
      with
      | Not_found -> return_false (* VERIEFIER LA VALEUR DE RETOUR SI PAS TROUVE *)


    let add t v =
      let value = to_cstruct v in
      let index = PI.digest_index value in
      AO.add t value >>=
        (fun x -> let _ = PI.add index x in Lwt.return index)


    (* TODO iter .... *)
    let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"
     (* AO.iter t (fun k v ->
                 let v = v >|= fun v -> of_cstruct v in
                 fn k v)
*)

end

module Make (P: PERSISTANT_INDEX_MAKER) (AO: AO_MAKER) (RW:RW_MAKER) =
  Irmin.Make (AOI(P)(AO)) (RW)
