
open Nocrypto
open Nocrypto.Uncommon


(* Logs *)
module Log = Log.Make(struct let section = "KRYPO" end)

(* Exceptions *)
exception Error of string



(* Retreiving Method *)
type retriving_method =
  | File of string
  | Debug_Test (* must be removed after ... *)
(* | mirageOS ... *)


(* Key Store module : we can improve that... *)
module KeyStore : KEY_STORE = struct

    (* Temp stuff : generate "constant" keys randomly by size *)
    let gen_key length =
      let gen() = match Random.int(26+26+10) with
        | n when n < 26 -> int_of_char 'a' + n
        | n when n < 26 + 26 -> int_of_char 'A' + n - 26
        | n -> int_of_char '0' + n - 26 - 26 in
      let gen _ = String.make 1 (char_of_int(gen())) in
      String.concat "" (Array.to_list (Array.init length gen));;


    (* Retriving... | File -> .. open and read file .. *)
    let retreive_key m c =
      match m with
      | Debug_Test ->
         let size_expected = size_key_of_cipher c in
         gen_key size_expected


    (* Initialization of hash key store TODO: try...catch *)
    let init_key_hash
          ~cipher_hash:cipher
          ~method_hash:retriving_method
      let hk = retreive_key method_hash cipher_hash in
               Cstruct.(of_string hk);;


    (* Initialization of content key store TODO: try...catch *)
    let init_key_content
          ~cipher_content:cipher
          ~method_content:retriving_method =
      let ck = retreive_key method_content cipher_content in
      Cstruct.(of_string ck);;

    (* TEMP for CBC *)
    let get_iv =
      (Cstruct.of_string "1234abcd1234abcd")

  end



(* Cipher content module *)
module CipherContent (KS: KEY_STORE) (KD:KEY_DERIVATION) (C:Cipher_block): CIPHER_CONTENT = struct

    let master_key = C.of_secret KS.init_key_content
    let iv = KS.get_iv

    type t_hash
    type t_user_password
    type t_content

    (** Encryption function with optional password, encrypted hash of blob, and the content *)
    val encrypt ~hash:t_hash
                ~content:t_content
                ?password:t_user_password =
      let key = KD.derivate master_key hash password in
      let enc_result = C.encrypt ~key ~iv content

    (** Decryption function with optional password, encrypted hash of blob, and the content *)
    val decrypt ~hash:t_hash
                ~content:t_content
                ?password:t_user_password =
      let key = KD.derivate master_key hash password in
      let enc_result = C.decrypt ~key ~iv content

  end


module CipherHash (KS: KEY_STORE) = struct

    let key = C.of_secret KS.init_key_content
    let iv = KS.get_iv

    type t_hash
    type t_user_password
    type t_content

    (** Encryption function *)
    val encrypt value =
      C.encrypt ~key ~iv value

    (** Decryption function *)
    val decrypt value =
      C.decrypt ~key ~iv value

  end


(* Key derivation module *)
module KeyDerivation = struct

    (* Derviate key TODO : Key Derivation algorithm *)
    let derivate ukey mkey ~password =
      match password with
      | None -> ukey lxor mkey
      | Some x -> ukey lxor mkey lxor password

  end



module KRYPTO_AO (CC: CIPHER_CONTENT) (CH: CIPHER_HASH) (H: Irmin.Hash.S) (S:AO) = struct

    type t

    type key

    type value

    let create config task =
      S.create config task

    let task t =
      S.task t

    let read t key =
      let ekey = CH.encrypt key in
      let content = S.read t ekey in
      C.decrypt ~hash:dkey ~content:v

    let read_exn t key =
      let ekey = CH.ecrypt ekey in
      try
        let content = S.read_exn t ekey in
        CC.decrypt ~hash:dkey ~content:v
      with
      | Not_found -> fail Not_found

    let mem t k =
      let ekey = CH.encrypt key in
      S.mem t ekey

    val add t v =
      let h = H.digest v in
      let eh = CH.encrypt h in
      let ev = CC.encrypt ~hash:eh ~content:v in
      let _ = S.add t ev in
      H.of_raw h

  end
