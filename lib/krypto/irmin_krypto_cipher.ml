

module type KEY_MANAGEMENT = Irmin_krypto_km.KEY_MANAGEMENT



module type CTR_MODE = sig

    type key

    val of_secret : Cstruct.t -> key

    val stream : key:key -> ctr:Cstruct.t -> ?off:int -> int -> Cstruct.t
    val encrypt : key:key -> ctr:Cstruct.t -> ?off:int -> Cstruct.t -> Cstruct.t
    val decrypt : key:key -> ctr:Cstruct.t -> ?off:int -> Cstruct.t -> Cstruct.t

  end



module type CIPHER = sig

   type header
(*
    val header_length: int
    val extract_header: Cstruct.t -> header
    val inject_header: header -> Cstruct.t 
 *)   
    val encrypt: Cstruct.t -> Cstruct.t
    val decrypt: Cstruct.t -> Cstruct.t

end



module Make_CTR (K:Irmin.Hash.S) (KM: KEY_MANAGEMENT) (C:CTR_MODE) : CIPHER = struct

    type header = Cstruct.t

    (** Initialization of the secret keys *)
    let key_data = C.of_secret KM.key_data
    let key_header = C.of_secret KM.key_header

    let header_length = 16 (*K.length*)
  

    let compute_header v = 
    let tmp = K.to_raw (K.digest v) in
    let res = Cstruct.create header_length in 
    Cstruct.blit tmp 0 res 0 header_length; (* PAY ATTENTION !!! *)
    res

    let extract_header blob = 
      let res = Cstruct.create header_length in
      Cstruct.blit blob 0 res 0 header_length;
      res

    let inject_header ~header blob = 
      let len_blob = Cstruct.len blob in
      let res = Cstruct.create (header_length + len_blob) in
      Cstruct.blit header 0 res 0 header_length;
      Cstruct.blit blob 0 res header_length len_blob;
      res

    let extract_value blob =
      let len_blob = Cstruct.len blob in
      let len = len_blob - header_length in
      let res = Cstruct.create len in
      Cstruct.blit blob header_length res 0 len;
      res

    (** Encryption function *)
    let encrypt value =
       let ctr_data = compute_header value in
       let enc_data = C.encrypt ~key:key_data ~ctr:ctr_data value in
       (* !!! CREEPY !! PAY ATTENTION !*)
       let ctr_header = (Cstruct.sub (K.to_raw (K.digest enc_data)) 0 header_length) in
       (* --- *)
       let enc_header = C.encrypt ~key:key_header ~ctr:ctr_header ctr_data in
       inject_header ~header:enc_header enc_data
      
    (** Decryption function *)
    let decrypt cstr =
      let enc_header = extract_header cstr in
      let enc_value = extract_value cstr in 
     
      (* !!! CREEPY !! PAY ATTENTION !*)
      let ctr_header = (Cstruct.sub (K.to_raw (K.digest enc_value)) 0 header_length) in
      (* --- *)
      let dec_header = C.decrypt ~key:key_header ~ctr:ctr_header enc_header in
  
      let dec_value =  C.decrypt ~key:key_data ~ctr:dec_header enc_value in   
  
      let hash_value = K.to_raw (K.digest dec_value) in
      let hash_value = Cstruct.sub hash_value 0 header_length in
      match (Cstruct.compare dec_header hash_value) with
      | 0 -> dec_value
      | _ -> failwith "Data corruption !"

end


module Make_GCM (K:Irmin.Hash.S) (KM: KEY_MANAGEMENT) (C:CTR_MODE) : CIPHER = struct

    type header = {ctr:Cstruct.t; tag:Cstruct.t}

    (** Initialization of the secret keys *)
    let key_data = C.of_secret KM.key_data
    let key_header = C.of_secret KM.key_header

    let header_length = K.length
  
    let compute_header v = 
    let tmp = K.to_raw (K.digest v) in
    let res = Cstruct.create header_length in 
    Cstruct.blit tmp 0 res 0 header_length; (* PAY ATTENTION !!! *)
    res

    let extract_header blob = 
      let res = Cstruct.create header_length in
      Cstruct.blit blob 0 res 0 header_length;
      res

    let inject_header ~header blob = 
      let len_blob = Cstruct.len blob in
      let res = Cstruct.create (header_length + len_blob) in
      Cstruct.blit header 0 res 0 header_length;
      Cstruct.blit blob 0 res header_length len_blob;
      res

    let extract_value blob =
      let len_blob = Cstruct.len blob in
      let len = len_blob - header_length in
      let res = Cstruct.create len in
      Cstruct.blit blob header_length res 0 len;
      res

    (** Encryption function *)
    let encrypt value =
       let ctr_data = compute_header value in
       let enc_data = C.encrypt ~key:key_data ~ctr:ctr_data value in
       (* !!! CREEPY !! PAY ATTENTION !*)
       let ctr_header = Cstruct.sub enc_data 0 header_length in
       (* --- *)
       let enc_header = C.encrypt ~key:key_header ~ctr:ctr_header ctr_data in
       inject_header ~header:enc_header enc_data
      
    (** Decryption function *)
    let decrypt cstr =
      let enc_header = extract_header cstr in
      (* !!! CREEPY !! PAY ATTENTION !*)
      let ctr_header = Cstruct.sub cstr header_length header_length in
      (* --- *)
      let dec_header = C.decrypt ~key:key_header ~ctr:ctr_header enc_header in
  
      let enc_value = extract_value cstr in  
      let dec_value =  C.decrypt ~key:key_data ~ctr:dec_header enc_value in   
  
      let hash_value = K.to_raw (K.digest dec_value) in
      let hash_value = Cstruct.sub hash_value 0 header_length in

      match (Cstruct.compare dec_header hash_value) with
      | 0 -> dec_value
      | _ -> failwith "Data corruption !"

end

