

(** The Key Store module for retreiving key(s), it's a NOT SUCERED WAY FOR RETREIVING KEYS *)
module type KEY_MANAGEMENT = sig

    val key_data: Cstruct.t
    val key_header: Cstruct.t

end


(** Key Store module : we can improve that... *)
module Make : KEY_MANAGEMENT
