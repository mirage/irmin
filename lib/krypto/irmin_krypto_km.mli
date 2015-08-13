

(** The Key Store module for retreiving key(s) *)
module type KEY_MANAGEMENT = sig

    (* must be removed after ... | mirageOS ... *)
    type retriving_method =
      | File of string
      | Debug_Test 

    val init_key: way:retriving_method -> Cstruct.t
  
end


(** Key Store module : we can improve that... *)
module Make (* K:KEY) (R:RESULT *) : KEY_MANAGEMENT
