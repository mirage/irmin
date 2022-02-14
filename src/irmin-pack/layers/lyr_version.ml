(* Duplicate a minimal Version module here, to avoid dependency on irmin-pack; obviously
   when irmin-pack versions change, this needs to be updated *)

type t = [ `V1 | `V2 ]
