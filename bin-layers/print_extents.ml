(** Print out the contents of an extents file (a sequence of (off,len) entries) *)

let path = Sys.argv |> Array.to_list |> List.tl |> function
  | [path] -> path
  | _ -> failwith "Usage: print_extents.exe <path to extents file>"

open Irmin_pack_layers.Util

let _ = 
  let mmap = Int_mmap.open_ ~fn:path ~sz:(-1) in
  begin
    0 |> iter_k (fun ~k i -> 
        match i+2 < BA1.dim mmap.arr with
        | true -> (
            Printf.printf "%d,%d\n%!" mmap.arr.{i} mmap.arr.{i+1};
            k (i+2))
        | false -> ())
  end;
  Int_mmap.close mmap;
  ()




