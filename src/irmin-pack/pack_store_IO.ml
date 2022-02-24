open! Import

(* NOTE moved from pack_store_intf.ml, to break cycle with S.ml *)

open struct
  (** Printf abbreviations *)
  module P = struct
    include Printf
    let s = sprintf 
    let p = printf
  end

  module F = struct
    include Format
    let s = sprintf 
    let p = printf
  end

  
end[@@warning "-32"]

type trigger_gc_t = {
  commit_hash_s: string;
  (** The commit hash, as a string; used for debugging *)

  create_reachable: reachable_fn:string -> unit
  (** The function the IO layer should use to calculate the initial reachability data;
      this will be called in another process, so it should not use any file descriptors
      from the parent; instead, it should open the repository in readonly mode and then
      call Repo.iter *)
}

(* to calculate the reachable extents, we open the Store in readonly mode, with
   read_logger set to write to the given file *)


(** Setting this to Some will trigger GC on the next IO operation (this is just for
    initial testing) *)
let trigger_gc : trigger_gc_t option ref = ref None

module Private (* : Irmin_pack_layers.IO.S *) = struct
  (* include IO.Unix *)
  
  open struct 
    module IO_impl = Irmin_pack_layers.IO (* IO.Unix *)
  end
  open IO_impl

  type t = { 
    base:IO_impl.t; 
    mutable read_logger: out_channel option; 
    mutable worker_pid_and_args: (int * Irmin_pack_layers.Worker.worker_args) option 
  }
           
  (* now we need to lift all the funs to work with this new type; OO has an advantage here
     in that classes can be easily extended with additional fields, whereas here we have
     to lift the existing functions *)

  (* we may check an envvar to see whether we need to log reads; typically this is set to
     false after the first instance is created; so we hope this is the right instance to
     log *)
  let check_envvar = ref true 

  (* expose a reference so we can close when finished *)
  let read_logger = ref None

  let v ~version ~fresh ~readonly path = 
    Printf.printf "%s: v called\n%!" __FILE__;
    let read_logger =       
      match !check_envvar with 
      | true -> (
          match Sys.getenv_opt "IRMIN_PACK_LOG_READS" with 
          | None -> None | Some fn -> (check_envvar:=false; read_logger:=Some (open_out_bin fn); !read_logger))
      | false -> None
    in
    { base=v ~version ~fresh ~readonly path; read_logger; worker_pid_and_args=None}

  let truncate t = truncate t.base

  let readonly t = readonly t.base

  let flush t = flush t.base

  let close t = close t.base
      
  let offset t = offset t.base

  let read t ~off buf = 
    let len = read t.base ~off buf in
    let _maybe_log = 
      match t.read_logger with 
      | None -> ()
      | Some oc -> 
        Irmin_pack_layers.Util.Out_channel_extra.(
          output_int_ne oc (Int63.to_int off);
          output_int_ne oc len;
          ())
    in
    len

  open struct
    open Irmin_pack_layers
    open Irmin_pack_layers.Util
    (* open Irmin_pack_layers.Worker *)

    let check_trigger_maybe_fork_worker (t0:t) = 
      let t = t0.base in
      match !trigger_gc with
      | None -> ()
      | Some _ when Option.is_some t0.worker_pid_and_args -> 
        let (pid,_args) = Option.get t0.worker_pid_and_args in
        Printf.printf "%s: warning: GC triggered but a worker %d is already running for a \
                       previous GC; ignoring" __FILE__ pid;
        trigger_gc := None;
        ()        
      | Some { commit_hash_s=_; create_reachable } ->        
        assert(t0.worker_pid_and_args = None); (* is_some case above *)
        let _ = trigger_gc := None in 
        let generation = 1+Control.get_generation t.control in
        let worker_args : Worker.worker_args = {
          working_dir=t.root;
          src=t.fn;
          create_reachable;
          sparse_dir=Pre_io.sparse_name ~generation;
          suffix_dir=Pre_io.suffix_name ~generation;
        }
        in
        let `Pid pid = Worker.fork_worker ~worker_args in
        t0.worker_pid_and_args <- Some (pid,worker_args);
        ()

    let handle_worker_termination (t:t) = 
      let open struct
        module Util = Irmin_pack_layers.Util
        module File = Util.File
      end
      in
      (* switch to the new sparse+suffix *)
      assert(Option.is_some t.worker_pid_and_args);
      let _pid,args = Option.get t.worker_pid_and_args in
      (* reset worker_pid_and_args *)
      let _ = t.worker_pid_and_args <- None in
      (* if the current generation is 1234, then after worker termination, we expect
         "sparse.1235" and "suffix.1235" (the next versions of the sparse and suffix) to
         exist *)      
      assert(Sys.file_exists Fn.(args.working_dir / args.sparse_dir));
      assert(Sys.file_exists Fn.(args.working_dir / args.suffix_dir));
      let next_sparse = Sparse_file.open_ro ~dir:Fn.(args.working_dir / args.sparse_dir) in
      let next_suffix = Suffix.open_ ~root:Fn.(args.working_dir / args.suffix_dir) in
      (* between the termination of the worker, and the execution of this code in the
         parent, further data may have been written to the current suffix; we need to copy
         this across to the next suffix *)
      let _ = 
        let cur_len = Pre_io.size t.base in
        let nxt_len = Suffix.size next_suffix in
        match nxt_len < cur_len with 
        | false -> ()
        | true -> 
          (* next suffix lags current suffix; need to copy data from current to next *)
          let pread = Pread.{pread=Pre_io.pread t.base} in
          let pwrite = Pwrite.{pwrite=Suffix.pwrite ~worker_only:() next_suffix} in
          let len = cur_len - nxt_len in
          File.copy ~src:pread ~dst:pwrite ~src_off:(cur_len - len) ~len ~dst_off:nxt_len;
          ()
      in
      (* now we perform the switch to the next sparse and suffix *)
      let old_sparse, old_suffix = t.base.sparse,t.base.suffix in
      t.base.sparse <- next_sparse;
      t.base.suffix <- next_suffix;
      Sparse_file.close old_sparse; (* FIXME use Sparse_file and Suffix_file, with sparse and suffix as abbrevs *)
      Suffix.close old_suffix;
      (* increment generation *)
      let _ = 
        let c = t.base.control in
        Control.(set c generation_field (1+get_generation c));
        (* fsync, so that others can detect a generation change *)
        Control.(fsync c);
        ()
      in
      Printf.printf "%s: switched to new sparse+suffix\n%!" __FILE__;
      (* FIXME and remove old files *)
      ()

    let check_worker_status (t:t) =
      match t.worker_pid_and_args with 
      | None -> ()
      | Some (pid,_) -> 
        let pid0,status = Unix.(waitpid [WNOHANG] pid) in
        match pid0 with
        | 0 -> 
          (* worker still processing *)
          ()
        | _ when pid0=pid -> (
            (* worker has terminated *)
            match status with 
            | WEXITED 0 -> handle_worker_termination t
            | WEXITED n -> failwith (P.s "Worker terminated unsuccessfully with %d" n)
            | _ -> failwith "Worker terminated abnormally")
        | _ -> failwith (P.s "Unexpected pid0 value %d" pid0)
        
        
  end

  let append t s = 
    check_trigger_maybe_fork_worker t;
    check_worker_status t;
    append t.base s

  let version t = version t.base

  let set_version t = set_version t.base

  let name t = name t.base

  let force_offset t = force_offset t.base

  (* let set_read_logger t opt = t.read_logger <- opt *)

end

include (Private : Irmin_pack_layers.IO.S)
