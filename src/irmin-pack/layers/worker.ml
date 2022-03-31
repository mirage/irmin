(** Worker process, responsible for calculating the reachable objects and constructing the
    next versions of the sparse and suffix.
*)

open Util

(** [irmin_layers_worker_debug] is the value of the environment variable
    [IRMIN_LAYERS_WORKER_DEBUG]; if set, this will aid debugging by e.g. not deleting
    temporary worker files during execution *)
let irmin_layers_worker_debug = Sys.getenv_opt "IRMIN_LAYERS_WORKER_DEBUG"

type create_reachable_t = (reachable_fn:string -> unit)

(** When the worker process is forked, it needs to know: 
- where it should place the new suffix and sparse files
- how to read the existing data in the pack store
- how to calculate the regions in the pack store that are reachable from a given commit
- the names of the new sparse and suffix
*)
type worker_args = {
  working_dir   : string; 
  (** where we place temporary files (ending in ".tmp") and the new sparse and suffix *)

  src           : string; 
  (** [src] is the path to the current IO instance; this will be opened readonly by the
      worker using {!Pre_io} *)

  create_reachable : create_reachable_t;
  (** function to invoke to produce the "reachable regions" data; the worker (and the IO
      layer as a whole) does not know about Irmin repositories etc; instead, we provide
      the worker with this function; this function takes a path (where to write the data)
      and likely calls [Repo.iter] with read logging enabled (or uses some other
      mechanism), to determine the reachable data *)

  sparse_dir    : string; 
  (** name (in [working_dir]) of the next sparse dir (a simple name, which will be located
      in [working_dir]) *)

  suffix_dir    : string; 
  (** name (in [working_dir]) of the next suffix dir *)
}

(** Signature of worker implementation *)
module type S = sig
  val fork_worker: worker_args:worker_args -> [`Pid of int]
end

(** Private implementation *)
module Private = struct

  let debug_mode = irmin_layers_worker_debug <> None

  let _ = 
    (* 64 bit ints; FIXME are we still trying to maintain 32bit archs? *)
    assert(Sys.word_size = 64) 

  let gap_tolerance = 0 
  (* FIXME config? although note that we can't just increase this and expect it to work with
     existing stores *)

  (** [calculate_extents] takes the reachable data in [reachable_fn], sorts it, and
      calculates the extents; uses [working_dir] for intermediate results *)
  let calculate_extents ~reachable_fn ~sorted_fn ~extents_fn = 
    let reachable  = Int_mmap.open_ ~fn:reachable_fn ~sz:(-1) in
    let chunk_sz   = 1_000_000 / 8 in
    let _          = assert(chunk_sz mod 2 = 0) in (* needs to be a multiple of 2 *)
    let sorted     = Int_mmap.open_ ~fn:sorted_fn ~sz:(BA1.dim reachable.Int_mmap.arr) in
    let _          = External_sort.sort ~chunk_sz ~src:reachable.arr ~dst:sorted.arr in
    let _create_extents = 
      let oc = Stdlib.open_out_bin extents_fn in
      External_sort.calculate_extents_oc ~src_is_sorted:() ~gap_tolerance ~src:sorted.arr ~dst:oc;
      Stdlib.close_out_noerr oc; (* FIXME maybe close and check for error *)
      ()
    in
    Int_mmap.close reachable;
    Int_mmap.close sorted;
    ()

  (** [create_sparse_file ~extents_fn ~src ~fn] creates a new sparse file [fn]; extent data
      [(off,len)] is read from [extents_fn]; [src] is the current pack store; each extent
      [(off,len)] in [src] is read and recorded in the new sparse file. *)
  let create_sparse_file ~(extents_fn:string) ~(src:Pread.t) ~fn : unit = 
    let _ = 
      assert(Sys.file_exists extents_fn);
      assert(File.size extents_fn mod 16 = 0);
      assert(not (Sys.file_exists fn));
    in
    let sparse = Sparse_file.create ~path:fn in
    let extents = Int_mmap.open_ ~fn:extents_fn ~sz:(-1) in
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

  (** [create_suffix_file ~src ~src_off ~len ~dst_path] creates a new suffix file
      [dst_path], starting at virtual offset [src_off]; to create the suffix file, [len]
      bytes of data is read from [src], starting from [src_off], and placed in the new
      suffix file. It is expected that [src_off + len] is the end of the existing [src]
      file, but because the [src] is continually being extended, there may actually be more
      than [len] bytes available at the time the copy is done. To account for this, the main
      process may further append to the new suffix file when the worker terminates (see
      function [handle_worker_termination], which may be located in [pack_store_IO.ml]). *)
  let create_suffix_file ~(src:Pread.t) ~src_off ~len ~dst_path : unit =
    (* create empty suffix *)
    let suff = Suffix.create ~root:dst_path ~suffix_offset:src_off in
    (* copy from src to suffix *)
    let dst = Pwrite.{pwrite=Suffix.pwrite ~worker_only:() suff} in  
    let _do_copy = File.copy ~src ~src_off ~dst ~dst_off:src_off ~len in
    let () = Suffix.close suff in
    ()


  (* debugging *)
  open struct
    let mark i = Printf.printf "Mark: %d\n%!" i
    (* let mark _ = () *)
  end

  let run_worker ~worker_args = 
    mark 1;
    let {working_dir;src;create_reachable;sparse_dir;suffix_dir} = worker_args in
    let _ = 
      Printf.printf 
        "(worker args: working_dir:%s; src: %s; sparse_dir: %s; suffix_dir: %s)\n%!" 
        working_dir src sparse_dir suffix_dir
    in
    let reachable_fn = Filename.temp_file ~temp_dir:working_dir "reachable." ".tmp" in
    mark 2;
    let () = create_reachable ~reachable_fn in
    mark 3;
    let sorted_fn = Filename.temp_file ~temp_dir:working_dir "sorted." ".tmp" in
    let extents_fn = Filename.temp_file ~temp_dir:working_dir "extents." ".tmp" in
    let _create_extents : unit = calculate_extents ~reachable_fn ~sorted_fn ~extents_fn in
    mark 4;
    let offset_of_last_extent = 
      let mmap = Int_mmap.open_ ~fn:extents_fn ~sz:(-1) in
      let sz = BA1.dim mmap.arr in
      assert(sz mod 2 = 0 && sz >= 2); (* FIXME ensure this is the case; perhaps bail if not *)
      let off = mmap.arr.{sz-2} in
      Int_mmap.close mmap;
      Printf.printf "%s: last extent offset is: %d\n%!" __FILE__ off;
      off
    in
    mark 5;
    let src_io = Pre_io.open_ ~readonly:true ~fn:src in  
    let src : Pread.t = { pread=(Pre_io.pread src_io) } in
    (* FIXME do we want to limit the sparse file to extents < offset_of_last_extent?
       probably yes *)
    mark 6;
    let _create_sparse : unit = create_sparse_file ~extents_fn ~src ~fn:Fn.(working_dir / sparse_dir) in
    mark 7;
    (* delete temporary files after they are not needed, but before creating the suffix;
       we want [create_suffix_file] to be the last thing that runs so that the main
       process has less to catch up with when switching; probably this doesn't make any
       difference in reality because these deletions will likely be very quick *)
    let _delete_tmp_files : unit = 
      match debug_mode with 
      | true -> ()
      | false -> 
        (* actually delete *)
        [reachable_fn;sorted_fn;extents_fn] |> List.iter (fun n -> Unix.unlink n)
    in
    let _create_suffix : unit = 
      let src_off = offset_of_last_extent in
      create_suffix_file ~src ~src_off ~len:(Pre_io.size src_io - src_off) ~dst_path:Fn.(working_dir / suffix_dir)
    in
    Log.info (fun m -> m "Worker terminating");
    ()

  let fork_worker ~worker_args = 
    Stdlib.flush_all ();
    let r = Unix.fork () in
    match r with 
    | 0 -> (run_worker ~worker_args; Unix._exit 0)
    | pid -> `Pid pid

  let _ = fork_worker

end

include (Private : S)
