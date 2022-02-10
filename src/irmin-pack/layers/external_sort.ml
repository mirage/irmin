(** After graph traversal, we need to do some external sorting of data on disk (approx
    500MB worth), and further processing. *)

open Util

module Private = struct
  open struct module BA1 = Bigarray.Array1 end

  let number_of_entries = 30_000_000 (* 30M reachable objs *)

  (* each entry consists of [step] ints *)
  let step = 2

  let lim = 50_000_000_000 (* 50GB of pack store *)

  let fill_with_test_data ~arr = 
    let sz = BA1.dim arr in
    0 |> iter_k (fun ~k:kont off -> 
        if off >= sz then () else
          let k = Random.nativeint (Nativeint.of_int lim) |> Nativeint.to_int in 
          let v = Random.int 200 in  (* avg obj len 200 *)
          arr.{ off } <- k;
          arr.{ off +1} <- v;
          kont (off+2))

  (* NOTE above allows objs to overlap, so not an accurate simulation *)


  (* stupid implementation: read chunk sized amounts of ints as a list of tuples, sort the
     list, and write back out; chunk_sz must be a multiple of step
     
     chunk_sz is the number of ints that are kept in memory, and so the overall memory
     usage is 8 * chunk_sz; if we limit to 80MB (say), chunk_sz can be 10M; then we end up
     with 6 sorted regions that we need to merge, which isn't too bad (we are dealing with
     500MB of ints, so this isn't surprising)

     FIXME maybe chunk_sz should be the number of entries?
  *)
  let sort_chunks ~arr ~chunk_sz = 
    let arr_sz = Bigarray.Array1.dim arr in
    begin
      0 |> iter_k (fun ~k:kont1 off -> 
          match off > arr_sz with
          | true -> ()
          | false -> 
            let sz = min chunk_sz (arr_sz - off) in
            (* read in as a list FIXME we may prefer to sort an array *)
            assert(sz mod step = 0);
            let xs = List.init (sz / step) (fun i -> (arr.{off+2*i}, arr.{off+2*i +1})) in
            (* sort list *)
            let xs = List.sort (fun (k,_) (k',_) -> Int.compare k k') xs in
            (* write back out *)
            let _write_out = 
              (xs,off) |> iter_k (fun ~k:kont2 (xs,off) -> 
                  match xs with 
                  | [] -> ()
                  | (k,v)::rest -> 
                    arr.{off} <- k;
                    arr.{off+1} <- v;
                    kont2 (rest,off+2))
            in
            (* do next chunk *)
            kont1 (off+chunk_sz))
    end;
    ()

    
  [@@@warning "-27"]

  (* perform n-way merge on the n sorted chunks in [src] *)
  let merge_chunks ~src ~chunk_sz ~dst =
    let src_sz,dst_sz = BA1.dim src, BA1.dim dst in
    let _initial_checks =
      assert(step = 2); (* could generalize further *)
      assert(chunk_sz mod step = 0);
      assert(dst_sz >= src_sz);
      ()
    in
    (* form subarrays of size [chunk_sz] from [src] *)
    let xs = 
      (0,[]) |> iter_k (fun ~k (off,xs) -> 
          match off < src_sz with
          | false -> xs
          | true -> 
            let arr = BA1.sub src off (min chunk_sz (src_sz - off)) in
            k (off+chunk_sz,arr::xs))
    in
    (* for each subarr, we start at position 0, and successively move through the array
       until the end; we keep the tuple (arr.{off}, off, arr) in a priority queue *)
    let open struct
      type pos_in_arr = { key:int; off:int; arr:int_bigarray }

      (* Q stands for "priority queue" *)
      module Q = Binary_heap.Make(struct 
          type t = pos_in_arr
          let compare x y = compare x.key y.key
        end)
    end
    in
    let xs = xs |> List.map (fun arr -> { key=arr.{0};off=0;arr }) in
    (* form priority queue *)
    let q = 
      let q = Q.create ~dummy:{key=0;off=0;arr=BA1.sub src 0 0} (List.length xs) in
      let _ = xs |> List.iter (fun x -> Q.add q x) in
      q
    in
    (* now repeatedly pull the min elt from q, put corresponding entry in dst, advance elt
       offset and put elt back in q *)
    let dst_off = 
      begin
        0 |> iter_k (fun ~k dst_off -> 
            match Q.is_empty q with
            | true -> 
              (* return so we can check it is what we think it should be *)
              dst_off 
            | false -> 
              let {key;off;arr} = Q.pop_minimum q in
              let v = arr.{off+1} in
              dst.{dst_off} <- key;
              dst.{dst_off +1} <- v;
              match off+2 < BA1.dim arr with
              | true -> 
                let off = off+2 in
                Q.add q { key=arr.{off};off;arr };
                k (dst_off+2)
              | false -> 
                (* finished with this chunk *)
                k (dst_off+2))
      end
    in
    assert(dst_off = src_sz);
    ()

  (** [is_sorted ~arr] returns true iff the array is sorted; we also want the extra data
      in each entry to be related to the initial entries of course, but we don't check
      that at the moment *)
  let is_sorted ~arr =
    let sz = BA1.dim arr in
    assert(sz > 0);
    (2,arr.{0}) |> iter_k (fun ~k (off,prev) -> 
        match off >= sz with
        | true -> true
        | false -> 
          let curr = arr.{off} in
          match prev <= curr with
          | true -> k (off+2,curr)
          | false -> false)            

  let print_entries ~arr ~n =
    for i = 0 to n-1 do
      P.p "(%d,%d)\n%!" arr.{2*i} arr.{2*i+1}
    done


  (** [calculate_extents ~src ~dst] takes {b sorted} [(off,len)] data from [src], combines
      adjacent extents, and outputs a minimal set of (sorted) extents to [dst]; the return
      value is the length of the part of [dst] that was filled *)
  let calculate_extents ~src ~dst = 
    let src_sz,dst_sz = BA1.dim src, BA1.dim dst in    
    let _ = 
      assert(src_sz >= 2);
      assert(src_sz mod step = 0);
      assert(dst_sz >= src_sz);      
      ()
    in    
    let (off,len) = src.{0},src.{1} in
    let regions_combined = ref 0 in
    let dst_off =
      (* iterate over entries in src, combining adjacent entries *)
      (2,0,off,len) |> iter_k (fun ~k (src_off,dst_off,off,len) -> 
          match src_off >= src_sz with
          | true -> 
            (* write out "current" extent *)
            dst.{dst_off} <- off;
            dst.{dst_off+1} <- len;
            dst_off+2 (* return the length of dst *)
          | false ->
            (* check if we can combine the next region *)
            let off',len' = src.{src_off},src.{src_off+1} in
            assert(off <= off');
            match off' <= off+len with
            | false -> 
              (* we can't, so write out current extent and move to next *)
              dst.{dst_off} <- off;
              dst.{dst_off+1} <- len;
              k (src_off+2,dst_off+2,off',len')
            | true ->               
              (* we can combine *)
              incr regions_combined;
              (if false &&  off' < off+len then P.p "Offset %d occured within existing region (%d,%d)\n%!" off' off len);
              assert(off <= off'); (* offs are sorted *)
              let len = max len (off'+len' - off) in
              k (src_off+2,dst_off,off,len))
    in
    P.p "Regions combined: %d\n%!" (!regions_combined);
    dst_off          
          
end


module Test() = struct
  open Private

  let sz = (number_of_entries * step)

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

  let _ = time (fun () -> fill_with_test_data ~arr:unsorted.arr)

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

  let extents = 
    let fn = "/home/tom/tmp/extents.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let _ = Printf.printf "Calculating extents\n%!"  
  let _ = 
    let dst_off = time @@ fun () -> calculate_extents ~src:sorted.arr ~dst:extents.arr in
    P.p "Final dst_off was %d\n%!" dst_off
    
  let _ = Int_mmap.close unsorted; Int_mmap.close sorted; Int_mmap.close extents; ()

(* unsorted.map is 480M bytes; chunk_sz 10*...; output:

Filling with test data
Finished in 1.998s
Sorting chunks
Finished in 18.615s
Merging chunks
Finished in 3.476s
Checking is_sorted
Finished in 451ms

chunk_sz 100*; output:
Filling with test data
Finished in 2s
Sorting chunks
Finished in 28.499s
Merging chunks
Finished in 2.759s
Checking is_sorted
Finished in 453ms

chunk_sz 1*; output:
Filling with test data
Finished in 1.996s
Sorting chunks
Finished in 8.712s
Merging chunks
Finished in 4.601s
Checking is_sorted
Finished in 452ms

Conclusion: the merging is so good that it doesn't matter if we have a large number of
chunks to merge; so we should have relatively small chunks to reduce the overall time.
*)

end
