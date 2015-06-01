(**

*)


open Lwt
open Irmin


module Log = Log.Make(struct let section = "BUCHERON" end)

module type RAW = Tc.S0 with type t = Cstruct.t

module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

					  

module BUCHERON_AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V: RAW) = struct

    module AO = S(K)(V)

    type key = AO.key

    type value = AO.value

    type t = AO.t

    (* Chunck Representation 	   

     uint8_t type;
     uint16_t size;
     byte data[data_length]  
 
     *)

    let hash_length = K.length

    let data_length = K.length * 40 (*4096*);;

    let type_length = 1

    let size_length = 2
      
    let chunck_length = data_length + type_length + size_length					    
						      
						      
    (* Getter Setter *)						      
    (*let get_chunck_type x = Cstruct.get_uint8 x 0 *)				      
    let set_chunck_type x v = Cstruct.set_uint8 x 0 v
    let get_chunck_size x = Cstruct.LE.get_uint16 x (type_length)				      
    let set_chunck_size x v = Cstruct.LE.set_uint16 x (type_length) v					   
    let get_chunck_data x = Cstruct.sub x (type_length + size_length) data_length
    let get_sub_chunck_data x offset len = Cstruct.sub x (type_length + size_length + offset) len 

    let set_data_from_raw src srcoff dst dstoff len =
      Cstruct.blit src srcoff dst (type_length + size_length + dstoff) len
		   
    let extract_size_data x = 
      (get_chunck_size x), (get_chunck_data x)
		   
    type chunck_type =
      | Indirect 
      | Data

    let chunck_type_to_int = function
      | Indirect -> 0
      | Data -> 1

    (* let int_to_chunck_type = function 
      | 0 -> Indirect
      | 1 -> Data
      | _ -> failwith "Unknow type" *)	       


		  
    (* Indirection and data chunck creation *)
    let create_indirection len =
      let c = Cstruct.create chunck_length in
      set_chunck_type c (chunck_type_to_int Indirect);
      set_chunck_size c len;
      c

    let create_chunck len =
      let c = Cstruct.create chunck_length in
      set_chunck_type c (chunck_type_to_int Data);
      set_chunck_size c len;
      c

	
    (* AO BUCHERON *)	
    let create config task =
      AO.create config task
		

    let task t =
      AO.task t
	      

    (* Just one indirection, need to be expanded *)	      
    let read t key =
      AO.read t key >>= function
      | None -> return_none
      | Some v ->
	 let chunck_number = get_chunck_size v in
         let result = Cstruct.create (chunck_number * data_length) in
         let rec loop i =
	   (* Finish *)
           if i > chunck_number - 1 then
	     Lwt.return_unit
	   (* Last chunck *)
	   else
	     let offset_key = (i * hash_length) in
	     let offset_data = (i * data_length) in
	     let key = K.of_raw (get_sub_chunck_data v offset_key hash_length) in
	     AO.read_exn t key >>= fun x ->
	     let size,data = extract_size_data x in
	     Cstruct.blit data 0 result offset_data size;
             loop (i+1) 
	 in
         loop 0 >>= fun () ->
         return (Some result)

		
    (* Just one indirection, need to be expanded *) 
    let read_exn t key =
      AO.read_exn t key >>= fun  v ->
      let size = get_chunck_size v in
      let result = Cstruct.create (size * data_length) in
      let rec loop i =
        if i > size - 1 then Lwt.return_unit
        else
          let offset_key = i * hash_length in
	  let key = K.of_raw (get_sub_chunck_data v offset_key hash_length) in
          AO.read_exn t key >>= fun x ->
          Cstruct.blit (get_chunck_data x) 0 result (i*data_length) (get_chunck_size x);
          loop (i+1)
      in
      loop 0 >>= fun () ->
      return result
	     

    (* TODO *)
    let mem t k =
      AO.mem t k

	     
    let add t v =
      let value_size = Cstruct.len v in
      let number_chunck = (value_size/data_length) + 1 in
      let indirection = create_indirection number_chunck in
      let rec loop i =
	if i > number_chunck - 1 then
	  Lwt.return_unit
	else
	  let len =
	    if i = number_chunck - 1 then
	      value_size mod data_length
	    else
	      data_length
	  in
	  let offset_data = i * data_length in
	  let offset_hash = i * hash_length in  
	  let chunck = create_chunck len in
	  set_data_from_raw v offset_data chunck 0 len;
	  let add () =
	    AO.add t chunck >>= fun x ->
	    set_data_from_raw (K.to_raw x) 0 indirection offset_hash hash_length;
	    Lwt.return_unit
	  in
	  Lwt.join [add (); loop (i+1)]
      in
      loop 0 >>= fun () ->
      AO.add t indirection
	     
	     
    (* TODO iter .... *)
    let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"
      (* AO.iter t (fun k v ->
         let ctr = Cstruct.of_string "1234abcd1234abcd" in
         let v = v >|= fun v -> of_cstruct (C.decrypt ~ctr v) in
         fn k v)
      *)
end
