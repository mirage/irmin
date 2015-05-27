

module type KEY_MANAGEMENT = Irmin_krypto_km.KEY_MANAGEMENT

module type CIPHER_BLOCK = sig

    (* CTR MODE *)
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


module Make (KM: KEY_MANAGEMENT) (C:CIPHER_BLOCK) : MAKER
