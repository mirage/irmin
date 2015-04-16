


(*
(** The module of key representation *)
module type KEY = sig
    type t
  end


(** The module of key memory representation *)
module type RESULT = sig
    type t
    val of_string: ?allocator:(int -> t) -> string -> t
  end
 *)

(** The Key Store module for retreiving key(s) *)
module type KEY_MANAGEMENT = sig
  (*  type k
    type result
   *)
    type retriving_method =
      | File of string
      | Debug_Test (* must be removed after ... | mirageOS ... *)

    val init_key: way:retriving_method -> Cstruct.t
    val get_iv: Cstruct.t (* To be more generic with others mode operator *)
  end


(* Key Store module : we can improve that... *)
module Make (*K:KEY) (R:RESULT*) : KEY_MANAGEMENT
