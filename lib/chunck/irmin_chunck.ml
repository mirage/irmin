(**

Chunck Norris Module.
The module who split your data on same size chuncks.

*)


open Lwt
open Irmin


module Log = Log.Make(struct let section = "CHUNCK" end)

module type RAW = Tc.S0 with type t = Cstruct.t

module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

					  

module CHUNCK_AO (S:AO_MAKER_RAW) (K:Irmin.Hash.S) (V: RAW) = struct


    
    (* Types *)
    module AO = S(K)(V)
    type key = AO.key
    type value = AO.value
    type t = {
	db:AO.t;
	size:int;
	hash_length: int;
	nb_indirect: int;
	data_length:int;
      }

	       
    (* Configuration module *)
(*    module Conf = struct
	exception Wrong_Param
		    
	let chunck_size =
	  Irmin.Private.Conf.key
	    ~doc:"Size of chunck"
	    "size"
	    (Ir_conf.int)
	    4096

	let get_ratio_dir r = (fst r)	
	let get_ratio_indir r = (snd r)
	let size_ratio r = (get_ratio_dir r) + (get_ratio_indir r)

      end
 *)   		    		  		     

    (* Chunck Module 

     All functions for manipulate chuncks representation, a chunck is represented as above : 
      --------------------------
     | uint8_t type             | 
     ---------------------------
     | uint16_t chunck_length   |
     ---------------------------
     | uint32_t total_length    |
     ---------------------------
     | byte data[data_length]   |
     ---------------------------
     Where type define if the chunck contain data or indirection, size represent the data length to 
     consider, and data field is the payload. 
     *)
	       
    module Chunck = struct 
	
	let hash_length = K.length
			    
	let l_type = 1
	let get_chunck_type x = Cstruct.get_uint8 x 0 				      
	let set_chunck_type x v = Cstruct.set_uint8 x 0 v
			    
	let l_chunck_length = 2
	let get_chunck_length x = Cstruct.LE.get_uint16 x (l_type)			
	let set_chunck_length x v = Cstruct.LE.set_uint16 x (l_type) v	

	let l_total_length = 4		       	
(*	let get_total_length x = Cstruct.LE.get_uint32 x (l_type + l_chunck_length)			
	let set_total_length x v = Cstruct.LE.set_uint32 x (l_type + l_chunck_length) v	
 *)
	let head_length = l_type + l_chunck_length + l_total_length 
			       
	let offset_payload = head_length - 1
			       
(*	let get_chunck_total_length x = Cstruct.LE.get_uint32 x (l_type + l_chunck_length)
	let set_chunck_total_length x v = Cstruct.LE.set_uint32 x (l_type + l_chunck_length) v
 		 				
	let get_chunck_data x = Cstruct.sub x (l_type + l_chunck_length + l_total_length ) (get_chunck_length x)
 *)
					     (*
	let get_sub_chunck_data x offset len = Cstruct.sub x (offset_payload + offset) len 
					      *)
	let get_sub_key x pos =
	  let offset = offset_payload + (pos * hash_length) in
	  Cstruct.sub x offset hash_length
							   
	(*	let head_size = l_type + l_chunck_length + l_total_length *)
							   
	let set_data_from_raw src srcoff dst dstoff len =
	  Cstruct.blit src srcoff dst (offset_payload + dstoff) len

	let set_data_to_raw src srcoff dst dstoff len =
	  Cstruct.blit src (offset_payload + srcoff) dst dstoff len

	(*	       
	let extract_size_data x = 
	  (get_chunck_length x), (get_chunck_data x)
	 *)			 
	type chunck_type =
	  | Indirect 
	  | Data
	      
	let chunck_type_to_int = function
	  | Indirect -> 0
	  | Data -> 1		      
		      
	let int_to_chunck_type = function 
	  | 0 -> Indirect
	  | 1 -> Data
	  | _ -> failwith "Unknow type"	       
			  
	let create_indirection t chunck_len =
	  let c = Cstruct.create t.size in
	  Cstruct.memset c 0x00;
	  set_chunck_type c (chunck_type_to_int Indirect);
	  set_chunck_length c chunck_len;
	  c
	    
	let create_chunck t chunck_len =
	  let c = Cstruct.create t.size in
	  Cstruct.memset c 0x00;
	  set_chunck_type c (chunck_type_to_int Data);
	  set_chunck_length c chunck_len;
	  c
(*
	let concat x = Cstruct.concat x 
 *)
      end


    module Tree = struct
	open Chunck
(*
	let get_first_child t node =
	  let key = K.of_raw (get_sub_chunck_data node 0 t.hash_length) in
	  AO.read_exn t.db key
 *)		      

	let get_child t node pos =
	  let key = K.of_raw (get_sub_key node pos) in
	  AO.read_exn t.db key		     		      
		 (*     
	let aux_height node =
	  match (int_to_chunck_type (get_chunck_type node)) with 
	  | Data -> 0
	  | Indirect -> 1

			  
	let rec height t node =
	  get_first_child t node >>=
	    (fun x ->
	     match (int_to_chunck_type (get_chunck_type node)) with 
	     | Data -> Lwt.return 0
	     | Indirect -> (height t x) >>= (fun y -> Lwt.return (1 + y))
	    )
		  *)
	      	    
	let walk_to_list t root =
	  let rec aux_walk t node =
	    match (int_to_chunck_type (get_chunck_type node)) with
	    | Data ->
	       let dlen = get_chunck_length node in
	       let leaf = Cstruct.create dlen in
	       set_data_to_raw node 0 leaf 0 dlen;
	       Lwt.return [leaf]
	    | Indirect -> 
	       let nbr_child = get_chunck_length node in 
	       let rec loop i accu =
		 if i < nbr_child then
		   (get_child t node i) >>=
		     (fun x ->
		      aux_walk t x >>=
			(fun y ->
			 (loop (i+1) (List.append accu y))
			)
		     )
		 else
		   Lwt.return accu
	       in
	       loop 0 []
	  in
	  aux_walk t root
		   


	let rec aux_botom_up t l r =
	  match l with
	  | [] -> Lwt.return r
	  | l ->
	   
	     let nbr =
	       if (List.length l) >= t.nb_indirect then
		 t.nb_indirect
	       else
		 List.length l
	     in

	     let indir = create_indirection t nbr in

	     let rec loop i l =
	       if (i >= nbr) then
		 l
	       else
		 let x = List.hd l in
		 let rest = List.tl l in 
		 let offset_hash = i * t.hash_length in
		 set_data_from_raw (K.to_raw x) 0 indir offset_hash t.hash_length;
		 loop (i+1) rest
	     in

	     let calc = loop 0 l in
	     
	     AO.add t.db indir >>=
	       (fun x ->
		(aux_botom_up t calc (x::r))
	       )

		 

	let rec bottom_up t l =
	  match l with
	  | [] -> failwith "Error 1"
 	  | [e] -> Lwt.return e
	  | l -> aux_botom_up t l [] >>= (fun x -> bottom_up t x)
		 
	  
      end
		  
		      
(*    let config ?conf ?size () =
      let module C = Irmin.Private.Conf in
      let config = match conf with
	| Some v -> v
	| None -> C.empty
      in
      let config = 
	match size with
	| Some v -> if v <= Chunck.head_size then
		      raise Conf.Wrong_Param
		    else
		      C.add config Conf.chunck_size v
	| None -> C.add config Conf.chunck_size (C.default Conf.chunck_size)
      in
      config
 *)
		    
    let create config task =
      let open Chunck in 
      let module C = Irmin.Private.Conf in
      let size = 108 in (*C.get config Conf.chunck_size in *)
      let data_length = size - head_length in
      let nb_indirect = data_length / K.length in
      AO.create config task >>= fun t ->
      return (fun a -> {db = t a; size; hash_length; nb_indirect; data_length})
		
    let task t =
      AO.task t.db


    let read t key =
      let open Chunck in
      AO.read_exn t.db key >>=
	(fun x ->
	 match (int_to_chunck_type (get_chunck_type x)) with
	 | Data ->
	    let dlen = (get_chunck_length x) in
	    let result = Cstruct.create dlen in
	    set_data_to_raw x 0 result 0 dlen;
	    Lwt.return (Some result)
	 | Indirect ->
	    (Tree.walk_to_list t x) >>=
	      (fun y -> 
	       let result = Cstruct.concat y in
	       return (Some result)
	      )
	)

	  
    let read_exn t key =
      let open Chunck in
      AO.read_exn t.db key >>=
	(fun x ->
	 match (int_to_chunck_type (get_chunck_type x)) with
	 | Data ->
	    let dlen = (get_chunck_length x) in
	    let result = Cstruct.create dlen in
	    set_data_to_raw x 0 result 0 dlen;
	    return result
	 | Indirect ->
	    (Tree.walk_to_list t x) >>=
	      (fun y ->
	       let result = Cstruct.concat y in
	       return result
	      ))

	  
    (* TODO *)
    let mem t k =
      AO.mem t.db k

	  
    let add t v =
      let open Chunck in
      let value_length = Cstruct.len v in
      let rest_value_length = value_length mod t.data_length in
 
      if value_length <= t.data_length then
	let chunck = create_chunck t value_length in
	set_data_from_raw v 0 chunck 0 value_length;  
	AO.add t.db chunck >>= (fun x -> Lwt.return x)

      else
	let nbr_data_bloc, last_data_bloc_length =
	let x =  value_length / t.data_length in
	if rest_value_length == 0 then
	  x, t.data_length
	else
	  (x + 1), rest_value_length
      in
      let nbr_indir_bloc, last_indir_bloc_length =
	let x = nbr_data_bloc / t.nb_indirect in
	let y = (nbr_data_bloc mod t.nb_indirect) in
	if y == 0 then
	  x, t.nb_indirect
	else
	  (x + 1), y 
      in
      
      let split_data_first_level offset_second_level =
	
	let number_chunck =
	  if offset_second_level == (nbr_indir_bloc - 1) then
	    last_indir_bloc_length
	  else
	    t.nb_indirect
	in
	
	let indir = create_indirection t number_chunck in
	
	let rec loop offset_first_level =
	  let dlen, max_loop =
	    if (offset_second_level == (nbr_indir_bloc - 1)) then
	      if (offset_first_level == (last_indir_bloc_length - 1)) then
		last_data_bloc_length, last_indir_bloc_length
	      else
		t.data_length, last_indir_bloc_length 
	    else
	      t.data_length, t.nb_indirect 
	  in
	  
	  if offset_first_level >= max_loop then
	    Lwt.return_unit
	  else
	    let chunck = create_chunck t dlen in
	    let offset_hash = offset_first_level * t.hash_length in
	    let offset_value = (offset_second_level * t.nb_indirect * t.data_length) + (offset_first_level * t.data_length) in 	    
	    set_data_from_raw v offset_value chunck 0 dlen;  
	    let add_chunck () =
	      AO.add t.db chunck >>= fun x ->
	      set_data_from_raw (K.to_raw x) 0 indir offset_hash hash_length;
	      Lwt.return_unit
	    in
	    Lwt.join [add_chunck (); loop (offset_first_level + 1)]
	in
	loop 0 >>= (fun _ ->
		    AO.add t.db indir >>=
		      (fun x ->
		       Lwt.return x)
		   )
      in
      
      let main_loop () =
	let rec first_level i =
	  match i with
	  | e when e == nbr_indir_bloc -> 
	      Lwt.return []
	  | e when e < nbr_indir_bloc ->
	     (split_data_first_level i) >>=
	       (fun x ->
		first_level (i+1) >>=
		  (fun y ->
		   Lwt.return (x::y)
		  )
		)
	  | _ -> failwith "Error 2"
	in
	first_level 0
      in
      (main_loop ()) >>= Tree.bottom_up t
						       
	
	     
    (* TODO iter .... *)
    let iter _t (_fn : key -> value Lwt.t -> unit Lwt.t) =
      failwith "TODO"
      (* AO.iter t (fun k v ->
         let ctr = Cstruct.of_string "1234abcd1234abcd" in
         let v = v >|= fun v -> of_cstruct (C.decrypt ~ctr v) in
         fn k v)
      *)
end
