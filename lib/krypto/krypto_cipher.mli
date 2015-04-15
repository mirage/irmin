

module type CIPHER_BLOCK = sig

    (* NOT WORKING WITH CBC MODE .... *)
    type t
    type key

    val of_secret : t -> key

    val encrypt : key:key -> t -> t
    val decrypt : key:key -> t -> t

  end



module type Cipher =
  functor (C:CIPHER_BLOCK) ->
  CIPHER_BLOCK with type t = C.t and type key = C.key


module type MAKER = sig
    (*    type t *)
    val encrypt : Cstruct.t -> Cstruct.t
    val decrypt : Cstruct.t -> Cstruct.t

end
