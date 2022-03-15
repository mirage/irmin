(** A simple utility to provide a human-readable version of the data in a control file. *)

let path = Sys.argv |> Array.to_list |> List.tl |> function
  | [path] -> path
  | _ -> failwith "Usage: print_control.exe <path to control file>"

let root,name = Filename.dirname path, Filename.basename path

module Control = Irmin_pack_layers.Control

let c = Control.open_ ~root ~name

let _ = print_endline (Control.to_string c)

let _ = Control.close c


