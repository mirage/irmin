(** A simple utility to provide a human-readable version of the data in the sparse map file. *)

let path = Sys.argv |> Array.to_list |> List.tl |> function
  | [path] -> path
  | _ -> failwith (Printf.sprintf "Usage: %s <path to control file>" Sys.argv.(0))

module S = Irmin_pack_layers.Sparse_file

let _ = 
  S.Private_map.load path |> fun m -> 
  m |> S.Private_map.iter (fun voff (roff,len) -> 
      Printf.printf "%d, %d, %d\n%!" voff roff len);
  ()
  

