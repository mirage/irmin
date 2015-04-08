

(*
  Irmin Encryption Layer
*)



type t_cipher_block =
  | AES_CBC_16
  | AES_CBC_24
  | AES_CBC_32


module CIPHER_CONTENT = sig

    type t_content
    type t_key
    type t_hash
    type t_user_password option

    (* Store the key outside Irmin *)
    val init : t_cipher_block -> t_key -> unit

    (* Encryption/Decryption fonction with optional password, encrypted hash of blob, and the content *)
    val encrypt : ~hash:t_hash -> ?password:t_user_password -> ~content:t_content
    val decrypt : ~hash:t_hash -> ?password:t_user_password -> ~content:t_content


  end



module type S = sig

    type cipher =
      | AES_CBC_16
      | AES_CBC_24
      | AES_CBC_32


    type hash =
      | SHA1


    module type HASH = sig

        type t
        type primary_hash_key

        val init_key : t -> unit
        val encrypt : t -> t
        val decrypt : t -> t

      end


    module type CONTENT = sig

        type t
        type primary_content_key
        type hash_key
        type user_password

        val init_key : primary_content_key -> unit
        val encrypt : t -> hash_blob_key -> user_password option -> t
        val decrypt : t -> hash_blob_key -> user_password option -> t

      end

  end


module AO (C: CIPHER_CONTENT)
          (H: CIPHER_HASH)
          (K: KEY_RETREIVE)
          (KD : KEY_DERIVATION)
       : Irmin.AO_MAKER
