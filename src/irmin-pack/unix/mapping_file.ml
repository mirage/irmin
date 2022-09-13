(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
include Mapping_file_intf
module BigArr1 = Bigarray.Array1

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

type int64_bigarray =
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

(* each entry consists of [step] ints; there is the possibility to generalize to
   arbitrary step sizes, but the following code always works with (key,value) pairs, ie
   step size is 2 *)
let step_2 = 2

(* Should be a multiple of 2 *)
let chunk_sz = 1_000_000 / 8

(* Set to 0 until we find decide what to do about sequential traversal of pack files *)
let gap_tolerance = 0

module Int_mmap : sig
  type t = private {
    fn : string;
    fd : Unix.file_descr;
    mutable arr : int_bigarray;
  }

  val create : fn:string -> sz:int -> t

  val open_ro : fn:string -> sz:int -> t
  (** NOTE [open_ro ~fn ~sz] can use [sz=-1] to open with size based on the size
      of the underlying file *)

  val close : t -> unit
end = struct
  type t = { fn : string; fd : Unix.file_descr; mutable arr : int_bigarray }

  (* NOTE both following are shared *)

  let create ~fn ~sz =
    let shared = true in
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
  let open_ro ~fn ~sz =
    let shared = false in
    assert (Sys.file_exists fn);
    let fd = Unix.(openfile fn [ O_RDONLY ] 0o660) in
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

module Int64_mmap : sig
  type t = private {
    fn : string;
    fd : Unix.file_descr;
    mutable arr : int64_bigarray;
  }

  val open_ro : fn:string -> sz:int -> t
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size
      of the underlying file *)

  val close : t -> unit
end = struct
  type t = { fn : string; fd : Unix.file_descr; mutable arr : int64_bigarray }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ro ~fn ~sz =
    let shared = false in
    assert (Sys.file_exists fn);
    let fd = Unix.(openfile fn [ O_RDONLY ] 0o660) in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int64 c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  let close t =
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int64 c_layout 0);
    ()
end

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

let rev_calculate_extents_oc ~(src_is_revsorted : unit) ~(src : int_bigarray)
    ~(register_entry : off:int -> len:int -> unit) : unit =
  ignore src_is_revsorted;
  let src_sz = BigArr1.dim src in
  let _ =
    assert (src_sz >= 2);
    assert (src_sz mod step_2 = 0);
    ()
  in
  let n = src_sz - 2 in
  let off, len = (src.{n}, src.{n + 1}) in
  let regions_combined = ref 0 in
  let dst_off =
    (* iterate over entries in src, combining adjacent entries *)
    (n - 2, off, len)
    |> iter_k (fun ~k (src_off, off, len) ->
           match src_off < 0 with
           | true ->
               (* write out "current" extent *)
               register_entry ~off ~len;
               ()
           | false -> (
               (* check if we can combine the next region *)
               let off', len' = (src.{src_off}, src.{src_off + 1}) in
               assert (off' >= off + len);
               match off' <= off + len + gap_tolerance with
               | false ->
                   (* we can't, so write out current extent and move to next *)
                   register_entry ~off ~len;
                   k (src_off - 2, off', len')
               | true ->
                   (* we can combine *)
                   incr regions_combined;
                   assert (off <= off');
                   (* offs are sorted *)
                   let len = max len (off' + len' - off) in
                   k (src_off - 2, off, len)))
  in
  dst_off

module Make (Io : Io.S) = struct
  module Io = Io
  module Errs = Io_errors.Make (Io)
  module Ao = Append_only_file.Make (Io) (Errs)

  type t = { arr : int64_bigarray; root : string; generation : int }

  let open_map ~root ~generation =
    let path = Irmin_pack.Layout.V4.mapping ~generation ~root in
    match Io.classify_path path with
    | `File -> (
        let mmap = Int64_mmap.open_ro ~fn:path ~sz:(-1) in
        let arr = mmap.arr in
        let len = BigArr1.dim arr in
        match len > 0 && len mod 3 = 0 with
        | true ->
            Int64_mmap.close mmap;
            Ok { root; generation; arr }
        | false ->
            Error
              (`Corrupted_mapping_file
                (__FILE__ ^ ": mapping mmap size did not meet size requirements"))
        )
    | _ -> Error `No_such_file_or_directory

  let create ?report_file_sizes ~root ~generation ~register_entries () =
    assert (generation > 0);
    let open Result_syntax in
    let path0 = Irmin_pack.Layout.V4.reachable ~generation ~root in
    let path1 = Irmin_pack.Layout.V4.sorted ~generation ~root in
    let path2 = Irmin_pack.Layout.V4.mapping ~generation ~root in

    let* () =
      if Sys.word_size <> 64 then Error `Gc_forbidden_on_32bit_platforms
      else Ok ()
    in

    (* Unlink the 3 files and ignore errors (typically no such file) *)
    Io.unlink path0 |> ignore;
    Io.unlink path1 |> ignore;
    Io.unlink path2 |> ignore;

    (* Create [file0] *)
    let* file0 =
      Ao.create_rw ~path:path0 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal
    in

    (* Fill and close [file0] *)
    let register_entry ~off ~len =
      (* Write [off, len] in native-endian encoding because it will be read
         with mmap. *)
      let buffer = Bytes.create 16 in
      Bytes.set_int64_ne buffer 0 (Int63.to_int64 off);
      Bytes.set_int64_ne buffer 8 (Int64.of_int len);
      (* Bytes.unsafe_to_string usage: buffer is uniquely owned; we assume
         Bytes.set_int64_ne returns unique ownership; we give up ownership of buffer in
         conversion to string. This is safe. *)
      Ao.append_exn file0 (Bytes.unsafe_to_string buffer)
    in
    let* () = Errs.catch (fun () -> register_entries ~register_entry) in
    let* () = Ao.flush file0 in
    let* () = Ao.close file0 in

    (* Reopen [file0] but as an mmap, create [file1] and fill it. *)
    let file0 = Int_mmap.open_ro ~fn:path0 ~sz:(-1) in
    let sz = BigArr1.dim file0.Int_mmap.arr in
    let file1 = Int_mmap.create ~fn:path1 ~sz in
    let* () = Errs.catch (fun () -> sort ~src:file0.arr ~dst:file1.arr) in

    (* Close and unlink [file0] *)
    Int_mmap.close file0;
    let* reachable_size = Io.size_of_path path0 in
    Io.unlink path0 |> ignore;

    (* Create [file2] *)
    let* file2 =
      Ao.create_rw ~path:path2 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal
    in

    (* Fill and close [file2] *)
    let poff = ref 0 in
    let encode i =
      let buf = Bytes.create 8 in
      Bytes.set_int64_le buf 0 (Int64.of_int i);
      (* Bytes.unsafe_to_string is safe since [buf] will not be modified after
         this function returns. We give up ownership. *)
      Bytes.unsafe_to_string buf
    in
    let register_entry ~off ~len =
      Ao.append_exn file2 (encode off);
      Ao.append_exn file2 (encode !poff);
      Ao.append_exn file2 (encode len);
      poff := !poff + len
    in
    let* () =
      Errs.catch (fun () ->
          calculate_extents_oc ~src_is_sorted:() ~src:file1.arr ~register_entry)
    in
    let* () = Ao.flush file2 in
    let* () = Ao.fsync file2 in
    let mapping_size = Ao.end_poff file2 in
    let* () = Ao.close file2 in

    (* Close and unlink [file1] *)
    Int_mmap.close file1;
    let* sorted_size = Io.size_of_path path1 in
    Option.iter
      (fun f -> f (reachable_size, sorted_size, mapping_size))
      report_file_sizes;
    Io.unlink path1 |> ignore;

    (* Open created map *)
    open_map ~root ~generation

  let create_rev ?report_file_sizes ~root ~generation ~register_entries () =
    assert (generation > 0);
    let open Result_syntax in
    let path0 = Irmin_pack.Layout.V3.reachable ~generation ~root in
    let path2 = Irmin_pack.Layout.V3.mapping ~generation ~root in

    let* () =
      if Sys.word_size <> 64 then Error `Gc_forbidden_on_32bit_platforms
      else Ok ()
    in

    (* Unlink the 3 files and ignore errors (typically no such file) *)
    Io.unlink path0 |> ignore;
    Io.unlink path2 |> ignore;

    (* Create [file0] *)
    let* file0 =
      Ao.create_rw ~path:path0 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal
    in

    (* Fill and close [file0] *)
    let register_entry ~off ~len =
      (* Write [off, len] in native-endian encoding because it will be read
         with mmap. *)
      let buffer = Bytes.create 16 in
      Bytes.set_int64_ne buffer 0 (Int63.to_int64 off);
      Bytes.set_int64_ne buffer 8 (Int64.of_int len);
      (* Bytes.unsafe_to_string usage: buffer is uniquely owned; we assume
         Bytes.set_int64_ne returns unique ownership; we give up ownership of buffer in
         conversion to string. This is safe. *)
      Ao.append_exn file0 (Bytes.unsafe_to_string buffer)
    in
    let* () = Errs.catch (fun () -> register_entries ~register_entry) in
    let* () = Ao.flush file0 in
    let* () = Ao.close file0 in

    (* Reopen [file0] but as an mmap *)
    let file0 = Int_mmap.open_ro ~fn:path0 ~sz:(-1) in

    (* Create [file2] *)
    let* file2 =
      Ao.create_rw ~path:path2 ~overwrite:true ~auto_flush_threshold:1_000_000
        ~auto_flush_procedure:`Internal
    in

    (* Fill and close [file2] *)
    let poff = ref 0 in
    let encode i =
      let buf = Bytes.create 8 in
      Bytes.set_int64_le buf 0 (Int64.of_int i);
      (* Bytes.unsafe_to_string is safe since [buf] will not be modified after
         this function returns. We give up ownership. *)
      Bytes.unsafe_to_string buf
    in
    let register_entry ~off ~len =
      Ao.append_exn file2 (encode off);
      Ao.append_exn file2 (encode !poff);
      Ao.append_exn file2 (encode len);
      poff := !poff + len
    in
    let* () =
      Errs.catch (fun () ->
          rev_calculate_extents_oc ~src_is_revsorted:() ~src:file0.arr
            ~register_entry)
    in

    (* Close and unlink [file0] *)
    Int_mmap.close file0;
    Io.unlink path0 |> ignore;

    let* () = Ao.flush file2 in
    let* () = Ao.fsync file2 in
    let mapping_size = Ao.end_poff file2 in
    let* () = Ao.close file2 in

    Option.iter (fun f -> f mapping_size) report_file_sizes;

    (* Open created map *)
    open_map ~root ~generation

  let entry_count arr = BigArr1.dim arr / 3
  let entry_idx i = i * 3

  let conv_int64 : int64 -> int =
   fun i ->
    (if Sys.big_endian then (
     (* We are currently on a BE platform but the ints are encoded as LE in the
        file. We've just read a LE int using a BE decoding scheme. Let's fix
        this.

        The first step is to set [buf] to contain exactly what is stored on
        disk. Since the current platform is BE, we've interpreted what was
        written on disk using a BE decoding scheme. To do the opposite operation
        we must use a BE encoding scheme, hence [set_int64_be].

        Now that [buf] mimics what was on disk, the second step consist of
        decoding it using a LE function, hence [get_int64_le]. *)
     let buf = Bytes.create 8 in
     Bytes.set_int64_be buf 0 i;
     Bytes.get_int64_le buf 0)
    else i)
    |> Int64.to_int

  let entry_off arr i = arr.{entry_idx i} |> conv_int64 |> Int63.of_int
  let entry_poff arr i = arr.{entry_idx i + 1} |> conv_int64 |> Int63.of_int
  let entry_len arr i = arr.{entry_idx i + 2} |> conv_int64

  let iter_exn { arr; _ } f =
    for i = 0 to entry_count arr - 1 do
      f ~off:(entry_off arr i) ~len:(entry_len arr i)
    done

  let iter t f =
    Errs.catch (fun () ->
        iter_exn t f;
        ())

  type entry = { off : int63; poff : int63; len : int }

  let find_nearest_leq { arr; _ } off =
    let get arr i = arr.{entry_idx i} |> conv_int64 in
    match
      Utils.nearest_leq ~arr ~get ~lo:0
        ~hi:(entry_count arr - 1)
        ~key:(Int63.to_int off)
    with
    | `All_gt_key -> None
    | `Some i ->
        let off = entry_off arr i in
        let poff = entry_poff arr i in
        let len = entry_len arr i in
        Some { off; poff; len }
end
