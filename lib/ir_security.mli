(*

  Security interface for Irmin

 *)



(* Error during decryption *)
exception Error_Decryption of string

(* Error during encryption *)
exception Error_Encryption of string

(* Error Handled when the key is wrong *)
exception Wrong_Key of string


module Recovery_Security_Key


module type S = sig

    type security_key

    type blob_key
    type blob

    val encrypt_blob : security_key -> blob -> blob
    val decrypt_blob : security_key -> blob -> blob

  end;;
