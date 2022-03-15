(** Convert an old store.pack file to the new layers format.

The old store.pack is a single file. The first 16 bytes are metadata, the rest is data. We
pull the metadata out, and create a new trivial sparse (nothing in it) and suffix
(containing the data) together with control etc.  

*)

open Irmin_pack_layers.Util

let store_dot_pack_path,new_path = Sys.argv |> Array.to_list |> List.tl |> function
  | [store_dot_pack_path;new_path] -> store_dot_pack_path,new_path
  | _ -> failwith "Usage: convert.exe <path to store.pack> <path to new pack>"

let _ = assert(Sys.file_exists store_dot_pack_path)

module IO = Irmin_pack_unix.IO.Unix

let old_io = IO.v ~version:None ~fresh:false ~readonly:true store_dot_pack_path

let _ = Printf.printf "Opened old store.pack %s\n%!" store_dot_pack_path

let version = IO.version old_io |> function `V1 -> 1 | `V2 -> 2

(* now create new sparse+suffix *)

module Pre_io = Irmin_pack_layers.Pre_io

let new_io = Pre_io.create ~fn:new_path

let _  = Printf.printf "Created new layered store %s\n%!" new_path

(* set version *)

let _ = Pre_io.set_version new_io version

(* copy data *)

let _do_copy = 
  Printf.printf "Starting copy...\n%!";
  (* let size = IO.size old_io in *)
  (* NOTE unfortunately the size is the real size, but we can actually only read (size -
     16) bytes of data-non-metadata; so instead we just copy till no more bytes can be
     read *)
  let buf_len = 5 * 4096 in
  let buf = Bytes.create buf_len in
  begin 
    0 |> iter_k (fun ~k off -> 
        let nread = IO.read old_io ~off:(Optint.Int63.of_int off) buf in
        (* assert(nread > 0); - may hold at end of file *)
        Pre_io.append new_io (Bytes.sub_string buf 0 nread);
        if nread > 0 then k (off + nread) else ())
  end;
  Printf.printf "Finished copy\n%!";
  IO.close old_io;
  Pre_io.close new_io;          
  ()
  
