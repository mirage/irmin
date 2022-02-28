open Util

module Test_external_sort() = struct
  open External_sort
  open Private

  (** [fill_with_test_data ~max_k ~max_v ~arr] fills [arr] with random [(k,v)] ints
      (bounded by [max_k,max_v]), where each key and value is stored successively in the
      array (so, the array cannot have a length that is an odd number). *)
  let fill_with_test_data ~max_k ~max_v ~(arr:int_bigarray) = 
    let sz = BA1.dim arr in
    0 |> iter_k (fun ~k:kont off -> 
        if off >= sz then () else
          let k = Random.nativeint (Nativeint.of_int max_k) |> Nativeint.to_int in 
          let v = Random.int max_v in
          arr.{ off } <- k;
          arr.{ off +1} <- v;
          kont (off+2))
  (* NOTE this code allows objs to overlap, so not an accurate simulation *)


  let print_entries_flag = false

  let print_entries ~arr ~n =
    if print_entries_flag then 
      for i = 0 to n-1 do
        P.p "(%d,%d)\n%!" arr.{2*i} arr.{2*i+1}
      done

  let number_of_entries = 30_000_000 (* 30M reachable objs *)

  let max_k = 50_000_000_000

  let max_v = 400

  let sz = (number_of_entries * step_2)

  let unsorted = 
    let fn = "/home/tom/tmp/unsorted.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let time f = 
    let c = Mtime_clock.counter() in
    let r = f () in
    let _ = Format.printf "Finished in %a\n%!" Mtime.Span.pp (Mtime_clock.count c) in
    r    

  let _ = Printf.printf "Filling with test data\n%!"

  let _ = time (fun () -> fill_with_test_data ~max_k ~max_v ~arr:unsorted.arr)

  let _ = print_entries ~arr:unsorted.arr ~n:100

  let entries_per_MB = 1_000_000 / 16 (* two ints *)

  (** Use at most 1MB of memory for each chunk, which will result in 480 chunks *)
  let chunk_sz = 1 * entries_per_MB

  let _ = Printf.printf "Sorting chunks\n%!"

  let _ = time (fun () -> sort_chunks ~arr:unsorted.arr ~chunk_sz)

  let sorted = 
    let fn = "/home/tom/tmp/sorted.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let _ = Printf.printf "Merging chunks\n%!"

  let _ = time (fun () -> merge_chunks ~src:unsorted.arr ~chunk_sz ~dst:sorted.arr)

  let _ = Printf.printf "Checking is_sorted\n%!"  
  let _ = time (fun () -> assert(is_sorted ~arr:sorted.arr))

  let _ = print_entries ~arr:sorted.arr ~n:100


(*
  let extents = 
    let fn = "/home/tom/tmp/extents.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let _ = Printf.printf "Calculating extents\n%!"  
  let _ = 
    let dst_off = time @@ fun () -> calculate_extents ~src_is_sorted:() ~src:sorted.arr ~dst:extents.arr in
    P.p "Final dst_off was %d\n%!" dst_off
*)
  let extents = 
    let fn = "/home/tom/tmp/extents.map" in
    (try Unix.unlink fn with _ -> ());
    Stdlib.open_out_bin fn
  let _ = Printf.printf "Calculating extents\n%!"  
  let _ = 
    time @@ fun () -> calculate_extents_oc ~src_is_sorted:() ~gap_tolerance:0 ~src:sorted.arr ~dst:extents 

      
    
  let _ = Int_mmap.close unsorted; Int_mmap.close sorted; Stdlib.close_out_noerr extents; ()

(* 

Filling with test data
Finished in 1.662s
Sorting chunks
Finished in 8.39s
Merging chunks
Finished in 6.438s
Checking is_sorted
Finished in 117ms
Calculating extents
Regions combined: 3393607
Finished in 461ms
Final dst_off was 53212786

For a 500MB file, we can choose chunk size even as low as 1MB. Then we have to perform a
500-way merge, but this is fast and doesn't consume much process memory (the cache
presumably has to hold 500 blocks to make this fast, but that is [500*4k = 2MB], so fairly
small).
*)

end


