(** An implementation of "external sorting" (sorting on-disk data) and various
    other related routines.

    Most of these routines work with mmap-ed data, as a one dimensional array of
    integers, where each pair of integers represents a [(key,value)] pair.

    These routines exist to support the implementation of the sparse file. The
    documentation in the sparse file should also be read.

    Usage: We start with a file containing [(off,len)] pairs. These describe
    which regions of a file contain data that we need when creating a sparse
    file. We first sort these by offset, using {!sort}. We then combine adjacent
    extents using {!calculate_extents_oc}. For example, a region [(10,10)] and a
    region [(20,10)] will be combined into the single extent [(10,20)]. When
    combining extents, we also want to allow some flexibility if two regions are
    "almost adjacent". For example, a region [(10,10)] and a region [(21,10)]
    will be combined into the single extent [(10,21)], even though there is a
    single byte at offset 20 that we do not actually need. The parameter
    [gap_tolerance] defines how large this gap between regions can be for them
    to be combined in this way. The reason for doing this is that we want the
    sparse file to have a small map if possible, and we are happy to include
    some unneeded data in the sparse data file if this will make the map
    smaller. *)

open! Import

(* each entry consists of [step] ints; there is the possibility to generalize to
   arbitrary step sizes, but the following code always works with (key,value) pairs, ie
   step size is 2 *)
let step_2 = 2

(* Should be a multiple of 2 *)
let chunk_sz = 1_000_000 / 8

(* Set to 0 until we find decide what to do about sequential traversal of pack files *)
let gap_tolerance = 0

module BigArr1 = Bigarray.Array1

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Int_mmap : sig
  type t = private {
    fn : string;
    fd : Unix.file_descr;
    mutable arr : int_bigarray;
  }

  val create : fn:string -> sz:int -> t

  val open_ : fn:string -> sz:int -> t
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size
      of the underlying file *)

  val close : t -> unit
end = struct
  type int_bigarray =
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = { fn : string; fd : Unix.file_descr; mutable arr : int_bigarray }

  (* NOTE both following are shared *)
  let shared = true

  let create ~fn ~sz =
    assert (
      (not (Sys.file_exists fn))
      ||
      (Printf.printf "File exists: %s\n%!" fn;
       false));
    let fd =
      Unix.(openfile fn [ O_CREAT; O_RDWR; O_TRUNC; O_EXCL; O_CLOEXEC ] 0o660)
    in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ ~fn ~sz =
    assert (Sys.file_exists fn);
    let fd = Unix.(openfile fn [ O_RDWR ] 0o660) in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  let close t =
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int c_layout 0);
    ()
end

(** Essentially the Y combinator; useful for anonymous recursive functions. The
    k argument is the recursive callExample:

    {[
      iter_k (fun ~k n -> if n = 0 then 1 else n * k (n - 1))
    ]} *)
let iter_k f (x : 'a) =
  let rec k x = f ~k x in
  k x

(** [sort_chunks ~arr] sorts each chunk in the bigarray [arr].

    The [arr] should contain [(k,v)] integer pairs stored successively in the
    array. The last chunk may have size less than [chunk_sz] - we don't require
    the [arr] to be sized as a multiple of [chunk_sz].

    The implementation reads chunk-sized amounts of ints into memory as a list
    of tuples, sorts the list, and writes the list back out.

    [chunk_sz] is the number of ints that are kept in memory, and so the overall
    memory usage is something like [8 * chunk_sz] (with some overhead for the
    list.. FIXME perhaps an array would be better) *)
let sort_chunks ~(arr : int_bigarray) =
  let arr_sz = Bigarray.Array1.dim arr in
  0
  |> iter_k (fun ~k:kont1 off ->
         match off > arr_sz with
         | true -> ()
         | false ->
             let sz = min chunk_sz (arr_sz - off) in
             (* read in as a list; we may prefer to sort an array instead *)
             assert (sz mod step_2 = 0);
             let xs =
               List.init (sz / step_2) (fun i ->
                   (arr.{off + (2 * i)}, arr.{off + (2 * i) + 1}))
             in
             (* sort list *)
             let xs = List.sort (fun (k, _) (k', _) -> Int.compare k k') xs in
             (* write back out *)
             let _write_out =
               (xs, off)
               |> iter_k (fun ~k:kont2 (xs, off) ->
                      match xs with
                      | [] -> ()
                      | (k, v) :: rest ->
                          arr.{off} <- k;
                          arr.{off + 1} <- v;
                          kont2 (rest, off + 2))
             in
             (* do next chunk *)
             kont1 (off + chunk_sz));
  ()

(* [merge_chunks ~src ~dst] takes previously sorted chunks of [(k,v)] data in
   [src] and performs an n-way merge into [dst]. *)
let merge_chunks ~(src : int_bigarray) ~(dst : int_bigarray) =
  let src_sz, dst_sz = (BigArr1.dim src, BigArr1.dim dst) in
  let _initial_checks =
    assert (step_2 = 2);
    assert (chunk_sz mod step_2 = 0);
    assert (dst_sz >= src_sz);
    ()
  in
  (* form subarrays of size [chunk_sz] from [src] *)
  let xs =
    (0, [])
    |> iter_k (fun ~k (off, xs) ->
           match off < src_sz with
           | false -> xs
           | true ->
               let arr = BigArr1.sub src off (min chunk_sz (src_sz - off)) in
               k (off + chunk_sz, arr :: xs))
  in
  (* for each subarr, we start at position 0, and successively move through the array
     until the end; we keep the tuple (arr.{off}, off, arr) in a priority queue *)
  let open struct
    type pos_in_arr = { key : int; off : int; arr : int_bigarray }

    (* Q stands for "priority queue" *)
    module Q = Binary_heap.Make (struct
      type t = pos_in_arr

      let compare x y = compare x.key y.key
    end)
  end in
  let xs = xs |> List.map (fun arr -> { key = arr.{0}; off = 0; arr }) in
  (* form priority queue *)
  let q =
    let q =
      Q.create
        ~dummy:{ key = 0; off = 0; arr = BigArr1.sub src 0 0 }
        (List.length xs)
    in
    let _ = xs |> List.iter (fun x -> Q.add q x) in
    q
  in
  (* now repeatedly pull the min elt from q, put corresponding entry in dst, advance elt
     offset and put elt back in q *)
  let dst_off =
    0
    |> iter_k (fun ~k dst_off ->
           match Q.is_empty q with
           | true ->
               (* return so we can check it is what we think it should be *)
               dst_off
           | false -> (
               let { key; off; arr } = Q.pop_minimum q in
               let v = arr.{off + 1} in
               dst.{dst_off} <- key;
               dst.{dst_off + 1} <- v;
               match off + 2 < BigArr1.dim arr with
               | true ->
                   let off = off + 2 in
                   Q.add q { key = arr.{off}; off; arr };
                   k (dst_off + 2)
               | false ->
                   (* finished with this chunk *)
                   k (dst_off + 2)))
  in
  assert (dst_off = src_sz);
  ()

(** [sort ~src ~dst] sorts the [src] array of [(k,v)] pairs and places the
    result in [dst]. [src] and [dst] must be disjoint. [dst] must be large
    enough to hold the result. The data is sorted in chunks; [chunk_sz] is the
    number of ints that are kept in memory when sorting each chunk. *)

(** [sort ~src ~dst] sorts the (key,value) integer data in [src] and places it
    in [dst] ([src] and [dst] must be disjoint); [chunk_sz] is the number of
    integers that are held in memory when sorting each chunk. *)
let sort ~(src : int_bigarray) ~(dst : int_bigarray) =
  sort_chunks ~arr:src;
  merge_chunks ~src ~dst;
  ()

(** [calculate_extents_oc ~src_is_sorted ~src ~dst] uses the sorted reachability
    data in [src] and outputs extent data on [dst]. [gap_tolerance] specifies
    how much gap between two extents is allowed for them to be combined into a
    single extent. *)

(** [calculate_extents_oc ~src_is_sorted ~src ~dst] takes {b sorted} [(off,len)]
    data from [src], combines adjacent extents, and outputs a minimal set of
    (sorted) extents to [dst:out_channel]; the return value is the length of the
    part of [dst] that was filled. [gap_tolerance] is used to provide some
    looseness when combining extents: if the next extent starts within
    [gap_tolerance] of the end of the previous extent, then it is combined with
    the previous (the data in the gap, which is not originally part of an
    extent, will be counted as part of the resulting extent). This can reduce
    the number of extents significantly, at a cost of including gaps where the
    data is not actually needed. *)
let calculate_extents_oc ~(src_is_sorted : unit) ~(src : int_bigarray)
    ~(register_entry : off:int -> len:int -> unit) : unit =
  ignore src_is_sorted;
  let src_sz = BigArr1.dim src in
  let _ =
    assert (src_sz >= 2);
    assert (src_sz mod step_2 = 0);
    ()
  in
  let off, len = (src.{0}, src.{1}) in
  let regions_combined = ref 0 in
  let dst_off =
    (* iterate over entries in src, combining adjacent entries *)
    (2, off, len)
    |> iter_k (fun ~k (src_off, off, len) ->
           match src_off >= src_sz with
           | true ->
               (* write out "current" extent *)
               register_entry ~off ~len;
               ()
           | false -> (
               (* check if we can combine the next region *)
               let off', len' = (src.{src_off}, src.{src_off + 1}) in
               assert (off <= off');
               match off' <= off + len + gap_tolerance with
               | false ->
                   (* we can't, so write out current extent and move to next *)
                   register_entry ~off ~len;
                   k (src_off + 2, off', len')
               | true ->
                   (* we can combine *)
                   incr regions_combined;
                   assert (off <= off');
                   (* offs are sorted *)
                   let len = max len (off' + len' - off) in
                   k (src_off + 2, off, len)))
  in
  dst_off

(* Encoding of offset, length. An improvement would be to use varints to encode
   both. *)
type pair = int63 * int63 [@@deriving irmin ~encode_bin ~decode_bin]

module Make (Errs : Io_errors.S with module Io = Io.Unix) = struct
  module Ao = Append_only_file.Make (Io.Unix)

  let create ~root ~generation ~register_entries =
    let open Result_syntax in
    let path0 = Irmin_pack.Layout.V3.reachable ~generation ~root in
    let path1 = Irmin_pack.Layout.V3.sorted ~generation ~root in
    let path2 = Irmin_pack.Layout.V3.mapping ~generation ~root in

    let* () =
      if Sys.word_size <> 64 then Error `Gc_forbidden_on_32bit_platforms
      else Ok ()
    in

    (* Unlink the 3 files and ignore errors (typically no such file) *)
    Io.Unix.unlink path0 |> ignore;
    Io.Unix.unlink path1 |> ignore;
    Io.Unix.unlink path2 |> ignore;

    (* Create [file0] *)
    let file0_ref = ref None in
    let auto_flush_callback () =
      match !file0_ref with
      | None -> assert false
      | Some x -> Ao.flush x |> Errs.raise_if_error
    in
    let* file0 =
      Ao.create_rw ~path:path0 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_callback
    in
    file0_ref := Some file0;

    (* Fill and close [file0] *)
    let buffer = Bytes.create 16 in
    let register_entry ~off ~len =
      (* Write [off, len] in native-endian encoding because it will be read
         with mmap. *)
      (* if Int63.to_int off < 500 then
       *   Fmt.epr "register_entry < 500: %d %d\n%!" (Int63.to_int off) len; *)
      Bytes.set_int64_ne buffer 0 (Int63.to_int64 off);
      Bytes.set_int64_ne buffer 8 (Int64.of_int len);
      Ao.append_exn file0 (Bytes.unsafe_to_string buffer)
    in
    let* () = Errs.catch (fun () -> register_entries ~register_entry) in
    let* () = Ao.flush file0 in
    let* () = Ao.close file0 in

    (* Reopen [file0] but as an mmap, create [file1] and fill it. *)
    let file0 = Int_mmap.open_ ~fn:path0 ~sz:(-1) in
    let sz = BigArr1.dim file0.Int_mmap.arr in
    let file1 = Int_mmap.create ~fn:path1 ~sz in
    let* () = Errs.catch (fun () -> sort ~src:file0.arr ~dst:file1.arr) in

    (* Close and unlink [file0] *)
    Int_mmap.close file0;
    Io.Unix.unlink path0 |> ignore;

    (* Create [file2] *)
    let file2_ref = ref None in
    let auto_flush_callback () =
      match !file2_ref with
      | None -> assert false
      | Some x -> Ao.flush x |> Errs.raise_if_error
    in
    let* file2 =
      Ao.create_rw ~path:path2 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_callback
    in
    file2_ref := Some file2;

    (* Fill and close [file2]. *)
    let register_entry ~off ~len =
      (* Write [off, len] with repr because it will be read with repr. *)

      (* if off < 500 then *)
      (* Fmt.epr "\nregister_entry < 500: %d %d\n%!" off len; *)
      (* Fmt.epr "register_entry of middle file: %d %d\n%!" off len; *)
      let off = Int63.of_int off in
      let len = Int63.of_int len in
      encode_bin_pair (off, len) (Ao.append_exn file2)
    in
    let* () =
      Errs.catch (fun () ->
          calculate_extents_oc ~src_is_sorted:() ~src:file1.arr ~register_entry)
    in
    let* () = Ao.flush file2 in
    let* () = Ao.close file2 in

    (* Close and unlink [file1] *)
    Int_mmap.close file1;
    Io.Unix.unlink path1 |> ignore;

    Ok ()

  let iter io f =
    let buffer = Bytes.create (16 * 1000) in
    let buffer_off = ref 0 in

    let open Int63.Syntax in
    let open Int63 in
    let min a b = if a < b then a else b in
    let entry_bytes = of_int 16 in
    let max_entries_per_batch = of_int 1000 in

    (* let max_bytes_per_batch = max_entries_per_batch * entry_bytes in *)
    let open Result_syntax in
    let* byte_count = Io.Unix.read_size io in
    let entry_count = byte_count / entry_bytes in
    (* Fmt.epr "\nbyte_count:%#d entry_count:%#d\n%!" (to_int byte_count) (to_int entry_count); *)
    let* () =
      if entry_count * entry_bytes <> byte_count then
        Error (`Corrupted_mapping_file "unexpected file size")
      else Ok ()
    in

    let rec load_batch i last_yielded_end_offset =
      let entries_left = entry_count - i in
      (* Fmt.epr "load_batch i:%#d, entries_left:%#d \n%!" (to_int i) (to_int entries_left); *)
      if entries_left = zero then ()
      else
        let entries_in_batch = min entries_left max_entries_per_batch in
        let off = i * entry_bytes in
        let len = to_int (entries_in_batch * entry_bytes) in
        Io.Unix.read_exn io ~off ~len buffer;
        buffer_off := 0;
        yield_entries i (i + entries_in_batch) last_yielded_end_offset
    and yield_entries i end_i last_yielded_end_offset =
      if i = end_i then load_batch i last_yielded_end_offset
      else
        let off, len =
          (* Decoding a pair of int can't fail *)
          decode_bin_pair (Bytes.unsafe_to_string buffer) buffer_off
        in
        let () =
          if off < last_yielded_end_offset then
            let msg =
              Fmt.str "Found off:%a len:%a but the previous entry ends as at %a"
                Int63.pp off Int63.pp len Int63.pp last_yielded_end_offset
            in
            raise (Errors.Pack_error (`Corrupted_mapping_file msg))
        in
        f ~off ~len:(to_int len);
        let last_yielded_end_offset = off + len in
        yield_entries (succ i) end_i last_yielded_end_offset
    in
    Errs.catch (fun () -> load_batch zero zero)
end
