(* Duplicate a minimal Version module here, to avoid dependency on irmin-pack; obviously
   when irmin-pack versions change, this needs to be updated *)

type t = [ `V1 | `V2 ]

let to_int = function | `V1 -> 1 | `V2 -> 2

let of_int = function | 1 -> `V1 | 2 -> `V2 | _ -> Fmt.failwith "%s: of_int" __FILE__

let compare x y = Stdlib.compare (to_int x) (to_int y)
