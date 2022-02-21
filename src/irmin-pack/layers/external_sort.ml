(** After graph traversal, we need to do some external sorting of data on disk (approx
    500MB worth), and further processing. *)

open Util

type int_bigarray = Util.int_bigarray

open struct
  (* each entry consists of [step] ints *)
  let step = 2

end



module Private = struct
  [@@@warning "-27"](* FIXME *)

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
      

  (** [sort_chunks ~arr ~chunk_sz]  sorts each chunk in the bigarray [arr].

      The [arr] should contain [(k,v)] integer pairs stored successively in the array. The
      last chunk may have size less than [chunk_sz] - we don't require the [arr] to be
      sized as a multiple of [chunk_sz].

      The implementation reads chunk-sized amounts of ints into memory as a list of
      tuples, sorts the list, and writes the list back out.

      [chunk_sz] is the number of ints that are kept in memory, and so the overall memory
      usage is something like [8 * chunk_sz] (with some overhead for the list.. FIXME
      perhaps an array would be better) *)
  let sort_chunks ~(arr:int_bigarray) ~chunk_sz = 
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

  (* [merge_chunks ~src ~chunk_sz ~dst] takes previously sorted chunks of [(k,v)] data in
     [src] and performs an n-way merge into [dst]. *)
  let merge_chunks ~(src:int_bigarray) ~chunk_sz ~(dst:int_bigarray) =
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

  (** [sort ~chunk_sz ~src ~dst] sorts the (key,value) integer data in [src] and places it
      in [dst]; [chunk_sz] is the number of integers that are held in memory when sorting
      in memory. *)
  let sort ~chunk_sz ~(src:int_bigarray) ~(dst:int_bigarray) = 
    sort_chunks ~arr:src ~chunk_sz;
    merge_chunks ~src ~chunk_sz ~dst;
    ()

  (** [is_sorted ~arr] returns true iff the array is sorted; we also want the extra data
      in each entry to be related to the initial entries of course, but we don't check
      that at the moment *)
  let is_sorted ~(arr:int_bigarray) =
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


  (** [calculate_extents_oc ~src_is_sorted ~gap_tolerance ~src ~dst] takes {b sorted}
      [(off,len)] data from [src], combines adjacent extents, and outputs a minimal set of
      (sorted) extents to [dst:out_channel]; the return value is the length of the part of
      [dst] that was filled. [gap_tolerance] is used to provide some looseness when
      combining extents: if the next extent starts within [gap_tolerance] of the end of
      the previous extent, then it is combined with the previous (the data in the gap,
      which is not originally part of an extent, will be counted as part of the resulting
      extent). This can reduce the number of extents significantly, at a cost of including
      gaps where the data is not actually needed. *)
  let calculate_extents_oc ~(src_is_sorted:unit) ~gap_tolerance ~(src:int_bigarray) ~(dst:out_channel) : unit = 
    ignore(src_is_sorted);
    let src_sz = BA1.dim src in    
    let _ = 
      assert(src_sz >= 2);
      assert(src_sz mod step = 0);
      ()
    in    
    let output_dst ~off ~len = Util.Out_channel_extra.(
        output_int_ne dst off;
        output_int_ne dst len;
        ())
    in
    let (off,len) = src.{0},src.{1} in
    let regions_combined = ref 0 in
    let dst_off =
      (* iterate over entries in src, combining adjacent entries *)
      (2,off,len) |> iter_k (fun ~k (src_off,off,len) -> 
          match src_off >= src_sz with
          | true -> 
            (* write out "current" extent *)
            output_dst ~off ~len;
            ()
          | false ->
            (* check if we can combine the next region *)
            let off',len' = src.{src_off},src.{src_off+1} in
            assert(off <= off');
            match off' <= off+len+gap_tolerance with
            | false -> 
              (* we can't, so write out current extent and move to next *)
              output_dst ~off ~len;
              k (src_off+2,off',len')
            | true ->               
              (* we can combine *)
              incr regions_combined;
              (if false &&  off' < off+len then P.p "Offset %d occured within existing region (%d,%d)\n%!" off' off len);
              assert(off <= off'); (* offs are sorted *)
              let len = max len (off'+len' - off) in
              k (src_off+2,off,len))
    in
    P.p "Regions combined: %d\n%!" (!regions_combined);
    dst_off           
end

include (Private : sig
  val sort : chunk_sz:int -> src:int_bigarray -> dst:int_bigarray -> unit
  val is_sorted : arr:int_bigarray -> bool

  (* NOTE the following is about twice as slow as using the mmap via [calculate_extents]
     above; but this probably doesn't make much difference *)
  val calculate_extents_oc :
    src_is_sorted:unit -> gap_tolerance:int -> src:int_bigarray -> dst:out_channel -> unit
end)


module Test() = struct
  open Private

  let print_entries_flag = false

  let print_entries ~arr ~n =
    if print_entries_flag then 
      for i = 0 to n-1 do
        P.p "(%d,%d)\n%!" arr.{2*i} arr.{2*i+1}
      done

  let number_of_entries = 30_000_000 (* 30M reachable objs *)

  let max_k = 50_000_000_000

  let max_v = 400

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


module Old() = struct

  [@@@warning "-27"](* FIXME *)

  (** [calculate_extents ~src ~dst] takes {b sorted} [(off,len)] data from [src], combines
      adjacent extents, and outputs a minimal set of (sorted) extents to [dst]; the return
      value is the length of the part of [dst] that was filled *)
  let calculate_extents ~(src_is_sorted:unit) ~(src:int_bigarray) ~(dst:int_bigarray) = 
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


  (* NOTE as above, but dst is an out_channel *)
  (** [calculate_extents_oc ~src ~dst] takes {b sorted} [(off,len)] data from [src],
      combines adjacent extents, and outputs a minimal set of (sorted) extents to
      [dst:out_channel]; the return value is the length of the part of [dst] that was
      filled *)
  let calculate_extents_oc ~(src_is_sorted:unit) ~(src:int_bigarray) ~(dst:out_channel) : unit = 
    let src_sz = BA1.dim src in    
    let _ = 
      assert(src_sz >= 2);
      assert(src_sz mod step = 0);
      ()
    in    
    let output_dst ~off ~len = Util.Out_channel_extra.(
        output_int_ne dst off;
        output_int_ne dst len;
        ())
    in
    let (off,len) = src.{0},src.{1} in
    let regions_combined = ref 0 in
    let dst_off =
      (* iterate over entries in src, combining adjacent entries *)
      (2,off,len) |> iter_k (fun ~k (src_off,off,len) -> 
          match src_off >= src_sz with
          | true -> 
            (* write out "current" extent *)
            output_dst ~off ~len;
            ()
          | false ->
            (* check if we can combine the next region *)
            let off',len' = src.{src_off},src.{src_off+1} in
            assert(off <= off');
            match off' <= off+len with
            | false -> 
              (* we can't, so write out current extent and move to next *)
              output_dst ~off ~len;
              k (src_off+2,off',len')
            | true ->               
              (* we can combine *)
              incr regions_combined;
              (if false &&  off' < off+len then P.p "Offset %d occured within existing region (%d,%d)\n%!" off' off len);
              assert(off <= off'); (* offs are sorted *)
              let len = max len (off'+len' - off) in
              k (src_off+2,off,len))
    in
    P.p "Regions combined: %d\n%!" (!regions_combined);
    dst_off           

end
