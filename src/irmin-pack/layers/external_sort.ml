(** An implementation of "external sorting" (sorting on-disk data) and various other
    related routines. 

Most of these routines work with mmap-ed data, as a one dimensional array of integers,
where each pair of integers represents a [(key,value)] pair.

These routines exist to support the implementation of the sparse file. The documentation
in the sparse file should also be read.

Usage: We start with a file containing [(off,len)] pairs. These describe which regions of
a file contain data that we need when creating a sparse file. We first sort these by
offset, using {!sort}. We then combine adjacent extents using {!calculate_extents_oc}. For
example, a region [(10,10)] and a region [(20,10)] will be combined into the single extent
[(10,20)]. When combining extents, we also want to allow some flexibility if two regions
are "almost adjacent". For example, a region [(10,10)] and a region [(21,10)] will be
combined into the single extent [(10,21)], even though there is a single byte at offset 20
that we do not actually need. The parameter [gap_tolerance] defines how large this gap
between regions can be for them to be combined in this way. The reason for doing this is
that we want the sparse file to have a small map if possible, and we are happy to include
some unneeded data in the sparse data file if this will make the map smaller.
*)

open Util

module Private = struct
  (* each entry consists of [step] ints; there is the possibility to generalize to
     arbitrary step sizes, but the following code always works with (key,value) pairs, ie
     step size is 2 *)
  let step_2 = 2

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
            (* read in as a list; we may prefer to sort an array instead *)
            assert(sz mod step_2 = 0);
            let xs = List.init (sz / step_2) (fun i -> (arr.{off+2*i}, arr.{off+2*i +1})) in
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
      assert(step_2 = 2); 
      assert(chunk_sz mod step_2 = 0);
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
      in [dst] ([src] and [dst] must be disjoint); [chunk_sz] is the number of integers
      that are held in memory when sorting each chunk. *)
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
      assert(src_sz mod step_2 = 0);
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
              (* (if false &&  off' < off+len then P.p "Offset %d occured within existing region (%d,%d)\n%!" off' off len); *)
              assert(off <= off'); (* offs are sorted *)
              let len = max len (off'+len' - off) in
              k (src_off+2,off,len))
    in
    dst_off           

  (* NOTE using an [out_channel] is about twice as slow as using the mmap; but this
     probably doesn't make much difference to the overall time because
     [calculate_extents_oc] is such a small fraction compared to e.g. sorting *)
    
end

include (Private : sig                   
  val sort : chunk_sz:int -> src:int_bigarray -> dst:int_bigarray -> unit
  (** [sort ~chunk_sz ~src ~dst] sorts the [src] array of [(k,v)] pairs and places the
      result in [dst]. [src] and [dst] must be disjoint. [dst] must be large enough to
      hold the result. The data is sorted in chunks; [chunk_sz] is the number of ints that
      are kept in memory when sorting each chunk. *)

  val is_sorted : arr:int_bigarray -> bool
  (** [is_sorted ~arr] returns true iff the array of [(k,v)] pairs is sorted by [k]. This
      is used for testing, for example. *)

  val calculate_extents_oc :
    src_is_sorted:unit -> gap_tolerance:int -> src:int_bigarray -> dst:out_channel -> unit
    (** [calculate_extents_oc ~src_is_sorted ~gap_tolerance ~src ~dst] uses the sorted
        reachability data in [src] and outputs extent data on [dst]. [gap_tolerance]
        specifies how much gap between two extents is allowed for them to be combined into
        a single extent. *)
end)


