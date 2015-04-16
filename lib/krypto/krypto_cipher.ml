

module type KEY_MANAGEMENT = Krypto_km.KEY_MANAGEMENT


(** We use the CTR Mode *)
module type CIPHER_BLOCK = sig

    type key

    val of_secret : Cstruct.t -> key

    val stream  : key:key -> ctr:Cstruct.t -> int -> Cstruct.t
    val encrypt : key:key -> ctr:Cstruct.t -> Cstruct.t -> Cstruct.t
    val decrypt : key:key -> ctr:Cstruct.t -> Cstruct.t -> Cstruct.t

  end


module type MAKER = sig

    val encrypt : ctr:Cstruct.t -> Cstruct.t -> Cstruct.t
    val decrypt : ctr:Cstruct.t -> Cstruct.t -> Cstruct.t
end


module Make (KM: KEY_MANAGEMENT) (C:CIPHER_BLOCK) : MAKER = struct

    let key = C.of_secret (KM.init_key ~way:KM.Debug_Test)

    (** Encryption function *)
    let encrypt ~ctr value =
       C.encrypt ~key ~ctr value

    (** Decryption function *)
    let decrypt ~ctr value =
      C.decrypt ~key ~ctr value

  end
