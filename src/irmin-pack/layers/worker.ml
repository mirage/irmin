(** Worker process, responsible for calculating the reachable objects and constructing the
    next versions of the sparse file + suffix. 

FIXME worker logging probably shouldn't use the same mechanism as the main process -
worried about shared fds etc.

*)

[@@@warning "-33"]

open Util

[@@@warning "-27"]

(*
module type Pack_with_commit = sig
  module S : Irmin_pack.S
               
  val commit : S.commit
end

(** Calculate [(off,len)] data for reachable objects and write them to the file
    [reachable_fn] in the same format that is used by {!Int_mmap}. *)
let calculate_reachable_objects ~(pack_with_commit:(module Pack_with_commit)) ~reachable_fn = ()
(* NOTE at the moment I am not quite sure how to traverse the store - Repo.iter? Also, we
   need to grow the mmap as the reachable objects become more numerous... but we are only
   appending to fn so we can just buffer some data, then mmap and write out *)
*)

let _ = 
  (* 64 bit ints; FIXME are we still trying to maintain 32bit archs? *)
  assert(Sys.word_size = 8) 

(** [calculate_extents] takes the reachable data in [reachable_fn], sorts it, and
    calculates the extents; uses [working_dir] for intermediate results; returns the name
    of the file (which will be within [working_dir]) that holds the extent data; other
    intermediate files are deleted (FIXME not during testing) *)
let calculate_extents ~working_dir ~reachable_fn = 
  let reachable  = Int_mmap.open_ ~fn:reachable_fn ~sz:(-1) in
  let chunk_sz   = 1_000_000 / 8 in
  let _          = assert(chunk_sz mod 2 = 0) in (* needs to be a multiple of 2 *)
  let sorted_fn  = Filename.temp_file ~temp_dir:working_dir "sorted." ".tmp" in
  let sorted     = Int_mmap.create ~fn:sorted_fn ~sz:(BA1.dim reachable.Int_mmap.arr) in
  let _          = External_sort.sort ~chunk_sz ~src:reachable.arr ~dst:sorted.arr in
  let extents_fn = Filename.temp_file ~temp_dir:working_dir "extents." ".tmp" in
  (* NOTE we open extents with a size that is at least as big as the number of extents *)
  let extents    = Int_mmap.open_ ~fn:extents_fn ~sz:(BA1.dim sorted.Int_mmap.arr) in
  let n          = External_sort.calculate_extents ~src_is_sorted:() ~src:sorted.arr ~dst:extents.arr in
  (* NOTE ftruncate is apparently safe provided we don't try to access past the new end
     via the mmap *)
  Unix.ftruncate extents.fd (n * 8); (* 8 bytes per int *)
  Int_mmap.close reachable;
  Int_mmap.close sorted;
  Int_mmap.close extents;
  extents_fn


(* FIXME at the moment we ignore max_pos, which should be the pos at which we split the
   file *)  
let create_sparse_file ~(extents_fn:string) ~(src:Pread.t) ~fn : unit = 
  let _ = 
    assert(Sys.file_exists extents_fn);
    assert(File.size extents_fn mod 16 = 0);
    assert(not (Sys.file_exists fn));
  in
  let sparse = Sparse_file.create ~path:fn in
  let extents = Int_mmap.open_ ~fn:extents_fn ~sz:(File.size extents_fn / 8) in
  let arr = extents.arr in
  let arr_sz = BA1.dim arr in
  let _write_extents_to_sparse = 
    0 |> iter_k (fun ~k i -> 
        match i < arr_sz with
        | false -> ()
        | true -> 
          let off,len = arr.{i},arr.{i+1} in
          Sparse_file.append_region sparse ~src ~src_off:off ~len ~virt_off:off;
          k (i+2))
  in
  let _ = Int_mmap.close extents in
  let _ = Sparse_file.close sparse in
  ()


let create_suffix_file ~(src:Pread.t) ~src_off ~len ~dst_path : unit =
  (* create empty suffix *)
  let suff = Suffix.create ~root:dst_path ~suffix_offset:src_off in
  (* copy from src to suffix *)
  let dst = Pwrite.{pwrite=Suffix.pwrite ~worker_only:() suff} in  
  let _do_copy = copy ~src ~src_off ~dst ~dst_off:src_off ~len in
  let _ = Suffix.close suff in
  ()


(* for [run_worker], there are questions: 1) how does this get called? 2) does layers
   depend on irmin-pack, or vice versa? probably irmin-pack depends on layers, and we pass
   in [calculate_reachable_objects] *)
type calculate_reachable_objects_t = (reachable_fn:string -> unit)


type worker_args = {
  working_dir   :string; (** where we place temporary files; they end in ".tmp" *)
  src           :Pread.t; (** current sparse+suffix that we read from *)
  src_off       :int; (** where we split src for the next sparse+suffix *)
  src_len       :unit -> int; (** get the current length of src *)
  calc_rch_objs :calculate_reachable_objects_t;
  sparse_dir    :string; (** path to the next sparse dir *)
  suffix_dir    :string; (** path to the next suffix dir *)
}


let run_worker ~worker_args = 
  let {working_dir;src;src_off;src_len;calc_rch_objs;sparse_dir;suffix_dir} = worker_args in
  let reachable_fn = Filename.temp_file ~temp_dir:working_dir "reachable." ".tmp" in
  calc_rch_objs ~reachable_fn;
  let extents_fn = calculate_extents ~working_dir ~reachable_fn in
  let _ = create_sparse_file ~extents_fn ~src ~fn:sparse_dir in
  let _ = create_suffix_file ~src ~src_off ~len:(src_len()) ~dst_path:suffix_dir in
  Log.info (fun m -> m "Worker terminating");
  ()


let fork_worker ~worker_args = 
  Stdlib.flush_all ();
  let r = Unix.fork () in
  match r with 
  | 0 -> (run_worker ~worker_args; Unix._exit 0)
  | pid -> `Pid pid

