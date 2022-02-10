(** After graph traversal, we need to do some external sorting of data on disk (approx
    500MB worth), and further processing. *)

open Util

module Test_data = struct
  open struct module BA1 = Bigarray.Array1 end

  let n = 30_000_000

  (* each entry consists of [step] ints *)
  let step = 2

  let lim = Int.max_int

  let fill_with_test_data ~arr = 
    let sz = BA1.dim arr in
    0 |> iter_k (fun ~k:kont off -> 
        if off >= sz then () else
          let k = Random.nativeint (Nativeint.of_int lim) |> Nativeint.to_int in 
          let v = Random.int 400 in 
          arr.{ off } <- k;
          arr.{ off +1} <- v;
          kont (off+2))


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
            let xs = List.init (sz / step) (fun i -> (arr.{off+i}, arr.{off+i +1})) in
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
          
          
end


module Test() = struct

  let sz = 30_000_000 * 2

  let unsorted = 
    let fn = "/home/tom/tmp/unsorted.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let _ = Printf.printf "Filling with test data\n%!"

  let c = Mtime_clock.counter()

  let _ = Test_data.fill_with_test_data ~arr:unsorted.arr

  let _ = Format.printf "Finished in %a\n%!" Mtime.Span.pp (Mtime_clock.count c)

  let entries_per_MB = 1_000_000 / 16 (* two ints *)

  (** Use at most 1MB of memory for each chunk, which will result in 480 chunks *)
  let chunk_sz = 1 * entries_per_MB

  let _ = Printf.printf "Sorting chunks\n%!"

  let c = Mtime_clock.counter()
  let _ = Test_data.sort_chunks ~arr:unsorted.arr ~chunk_sz
  let _ = Format.printf "Finished in %a\n%!" Mtime.Span.pp (Mtime_clock.count c)

  let sorted = 
    let fn = "/home/tom/tmp/sorted.map" in
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz

  let _ = Printf.printf "Merging chunks\n%!"

  let c = Mtime_clock.counter()
  let _ = Test_data.merge_chunks ~src:unsorted.arr ~chunk_sz ~dst:sorted.arr
  let _ = Format.printf "Finished in %a\n%!" Mtime.Span.pp (Mtime_clock.count c)

  let _ = Printf.printf "Checking is_sorted\n%!"
  
  let c = Mtime_clock.counter()
  let _ = assert(Test_data.is_sorted ~arr:sorted.arr)
  let _ = Format.printf "Finished in %a\n%!" Mtime.Span.pp (Mtime_clock.count c)
    
  let _ = Int_mmap.close unsorted; Int_mmap.close sorted; ()

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
