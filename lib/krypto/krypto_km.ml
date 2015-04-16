



(* Logs *)

 module Log = Log.Make(struct let section = "KRYPO[RETREIVING]" end)



module type KEY_MANAGEMENT = sig

    type retriving_method =
      | File of string
      | Debug_Test (* must be removed after ... | mirageOS ... *)

    val init_key: way:retriving_method -> Cstruct.t
    val get_iv: Cstruct.t
  end


(* Key Store module : we can improve that... *)
module Make : KEY_MANAGEMENT = struct


    type retriving_method =
      | File of string
      | Debug_Test (* must be removed after ... | mirageOS ... *)

    (** Temp stuff : generate "constant" keys randomly by size : work like LFSR with a seed *)
    let gen_key length =
      let gen() = match Random.int(26+26+10) with
        | n when n < 26 -> int_of_char 'a' + n
        | n when n < 26 + 26 -> int_of_char 'A' + n - 26
        | n -> int_of_char '0' + n - 26 - 26 in
      let gen _ = String.make 1 (char_of_int(gen())) in
      String.concat "" (Array.to_list (Array.init length gen));;

    (** Retriving... | File -> .. open and read file .. *)
    let retreive_key m =
      match m with
      | Debug_Test -> gen_key 24
      | _ -> gen_key 24

    (** Initialization of hash key store TODO: try...catch *)
    let init_key ~way =
      let hk = retreive_key way in
      Cstruct.(of_string hk);;

    (** TEMP for CBC *)
    let get_iv =
      (Cstruct.of_string "1234abcd1234abcd")

  end
