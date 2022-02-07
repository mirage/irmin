(** The object store is a readonly key/value store, where keys are "file offsets" and
    values are objects (bytes).

It is used to store the reachable objects from an initial segment of the pack file. 
*)

open Util

(** Signature implemented by the object store *)
module type S = sig

  (** An open instance, containing an open fd for the data file, and an in-mem map FIXME
      maybe want to mmap for ro instance *)
  type t 

  (** [create ~root] creates a directory [root] to hold the data file and the map file,
      and initializes both to empty *)
  val create: root:string -> t

  (** [open_ro ~root] opens the object store in readonly mode, under the given root
      directory path *)
  val open_ro: root:string -> t
    
  (** Save the map file; close the data file *)
  val close: t -> unit
    
  (** [append_object t ~obj ~virt_off] appends the data [obj] to the end of the object
      store t. A new map entry [virt_off -> (real_off,len)] is added, where real_off was
      the real offset of the end of the object store data file before the append took
      place.

      Requires [t] opened via [create]; requires [virt_off] not to have been used already
      in a previous call to [append_object].

      NOTE no checks are made on [virt_off]; it is possible to add the same region
      multiple times for example, or add overlapping regions, etc. {b These usages should
      be avoided.} FIXME perhaps we should check explicitly for these kinds of
      problems?  *)
  val append_object: t -> obj:string -> virt_off:int -> unit

  (** [read_object t ~virt_off] returns [`Ok s] iff [s] was a previously appended object
      for offset [virt_off]; otherwise return an error. Requires that [t] was opened
      readonly. NOTE no attempt is made to "read within a previous object" or anything
      like that - virtual offsets are treated as abstract keys *)
  val read_object: t -> virt_off:int -> [`Ok of string | `Error ]

  (* val to_string : t -> string *)
end

module Private_map = struct

  type map = (int * int) Int_map.t

  include Int_map

  let load fn = 
    let ints = Small_int_file_v1.load fn in
    let ok = List.length ints mod 3 = 0 in
    let _check_ints_size = 
      if not ok then 
        failwith (P.s "%s: file %s did not contain 3*n ints" __FILE__ fn)
    in
    assert(ok);
    (ints,empty) |> iter_k (fun ~k (ints,m) -> 
        match ints with 
        | [] -> m
        | voff::off::len::rest ->
          (* each set of 3 ints corresponds to voff (virtual offset), off and len *)
          let m = add voff (off,len) m in
          k (rest,m)
        | _ -> failwith "impossible")

  let save t fn = 
    let x = ref [] in
    t |> bindings |> List.iter (fun (voff,(off,len)) -> x:= voff::off::len::!x);
    Small_int_file_v1.save !x fn

end

module Private = struct

  open Private_map

  let map_name = "map"

  let data_name = "data"

  type t = { root:string; data:Unix.file_descr; mutable map:map; readonly:bool }

  let create ~root = 
    mkdir ~path:root;
    let data = File.create ~path:Fn.(root / data_name) in
    let _ = 
      let fd = File.create ~path:Fn.(root / map_name) in
      Unix.close fd
    in
    { root; data; map=Int_map.empty; readonly=false }

  let open_ro ~root =
    let ok = Sys.file_exists root in
    assert(ok);
    let data = File.open_ ~path:Fn.(root / data_name) in
    let map = Private_map.load Fn.(root / map_name) in
    { root; data; map; readonly=true }
    
  let close t =
    (if not t.readonly then Private_map.save t.map Fn.(t.root / map_name));
    Unix.close t.data;
    ()
  
  let append_object t ~obj ~virt_off =
    assert(not t.readonly);
    (* t.data should always be appending, so no need to seek to end, but we do anyway to
       get the real_off without having to remember it *)
    let real_off = Unix.(lseek t.data 0 SEEK_END) in
    let buf = Bytes.unsafe_of_string obj in
    let len = Bytes.length buf in
    let n = Unix.write t.data buf 0 len in
    assert(n=len);
    (* now update the map; first check we don't already have a binding for virt_off *)
    let ok = not (Int_map.mem virt_off t.map) in
    assert(ok);
    t.map <- Int_map.add virt_off (real_off,len) t.map;
    ()

  let read_object t ~virt_off =
    assert(t.readonly);
    Int_map.find_opt virt_off t.map |> function
    | None -> `Error
    | Some (off,len) ->
      `Ok (File.read_string ~fd:t.data ~off ~len)

end

include (Private : S)

