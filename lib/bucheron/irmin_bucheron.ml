(**

*)


open Lwt
open Irmin


module Log = Log.Make(struct let section = "BUCHERON" end)


module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = K.t and type value = V.t

(*
module type AO_MAKER_CSTRUCT =
  functor (IK: Hash.S) ->
  functor (K: Hash.S) ->
  functor (V: Tc.S0 with type t = Cstruct.t) ->
  AO with type key = IK.t and type value = V.t
 *)

module BUCHERON_AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V:Tc.S0) = struct

    module AO = S(K)(Irmin.Contents.Cstruct)

    type key = AO.key

    type value = AO.value

    type t = AO.t

    let hash_length = K.length

    let data_length = K.length * 4096;;

    cenum datatype {
      IND;
      DATA
    } as uint8_t


    cstruct chunck {
      uint8_t t;
      uint16_t size;
      uint8_t data[81920]
    } as little_endian


    let key_to_cstruct x = Tc.write_cstruct (module K) x
    let key_of_cstruct x = Tc.read_cstruct (module K) x


    let value_to_cstruct x = Tc.write_cstruct (module V) x
    let value_of_cstruct x = Tc.read_cstruct (module V) x


    let create_indirection l =
      let size = sizeof_chunck in
      let c = Cstruct.create size in
      set_chunck_t c (datatype_to_int IND);
      set_chunck_size c l;
      c


    let create_chunck l =
      let size = sizeof_chunck in
      let c = Cstruct.create size in
      set_chunck_t c (datatype_to_int DATA);
      set_chunck_size c l;
      c


    let create config task =
      AO.create config task


    let task t =
      AO.task t


    let read t key =
      AO.read t key >>= function
      | None -> return_none
      | Some v ->
        (* PAS BESOIN DE CHECKER IND OU DATA CAR INDIRECTION DE NIVEAU 1 *)
        let size = get_chunck_size v in
        let result = Cstruct.create (size * data_length) in
        let rec loop i =
          if i > size - 1 then Lwt.return_unit
          else
            let offset = i * hash_length in
            let k = Cstruct.create hash_length in
            Cstruct.blit v offset k 0 hash_length;
            let key = key_of_cstruct k in
            AO.read_exn t key >>= fun x ->
            Cstruct.blit (get_chunck_data x) 0 result (i*data_length) (get_chunck_size x);
            loop (i+1)
            (* ATTENTION TAILLE DES DONNEES; SI UN CHUNCK A DES SIZE DATA DIFFERENTS NON EGAL AU MAX *)
        in
        loop 0 >>= fun () ->
        return (Some result)


    let read_exn t key =
      AO.read_exn t key >>= fun  v ->
      (* PAS BESOIN DE CHECKER IND OU DATA CAR INDIRECTION DE NIVEAU
           1 *)
      let size = get_chunck_size v in
      let result = Cstruct.create (size * data_length) in
      let rec loop i =
        if i > size - 1 then Lwt.return_unit
        else
          let offset = i * hash_length in
          let k = Cstruct.create hash_length in
          Cstruct.blit v offset k 0 hash_length;
          let key = key_of_cstruct k in
          AO.read_exn t key >>= fun x ->
          Cstruct.blit (get_chunck_data x) 0 result (i*data_length) (get_chunck_size x);
          loop (i+1)
          (* ATTENTION TAILLE DES DONNEES; SI UN CHUNCK A DES SIZE DATA DIFFERENTS NON EGAL AU MAX *)
      in
      loop 0 >>= fun () ->
      return result

    let mem t k =
      AO.mem t k


    let add t v =
      let value_length = Cstruct.len v in
      let nb = (value_length/data_length) + 1 in
      let ind = create_indirection nb in
      let rec loop i =
        if i > nb - 2 then Lwt.return_unit
        else
          let offset = i * data_length in
          let dest_chunck = create_chunck data_length in
          Cstruct.blit v offset dest_chunck 0 data_length;
          let add () =
            AO.add t dest_chunck >>= fun x ->
            Cstruct.blit (key_to_cstruct x) 0 ind (i * hash_length) hash_length;
            Lwt.return_unit
          in
          Lwt.join [add (); loop (i+1)]
      in
      loop 0 >>= fun () ->
      let size = value_length - ((nb - 1) * data_length) in
      let dest_chunck = create_chunck size in
      Cstruct.blit v ((nb - 1) * data_length) dest_chunck 0 size;
      AO.add t dest_chunck >>= (function x -> Lwt.return (Cstruct.blit (key_to_cstruct x) 0 ind ((nb - 1) * hash_length) hash_length));
      AO.add t ind


    (* TODO iter .... *)
    let iter t (fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"
      (* AO.iter t (fun k v ->
         let ctr = Cstruct.of_string "1234abcd1234abcd" in
         let v = v >|= fun v -> of_cstruct (C.decrypt ~ctr v) in
         fn k v)
      *)
end
