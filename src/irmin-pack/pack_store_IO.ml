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
    mutable worker_pid:int option 
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
    { base=v ~version ~fresh ~readonly path; read_logger; worker_pid=None}

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

  include struct
    open Irmin_pack_layers
    open Irmin_pack_layers.Worker

    let check_trigger_maybe_fork_worker (t0:t) = 
      let t = t0.base in
      match !trigger_gc with
      | None -> ()
      | Some { commit_hash_s=_; create_reachable } ->       
        let _ = trigger_gc := None in 
        let generation = 1+Control.get_generation t.control in
        let worker_args : worker_args = {
          working_dir=t.root;
          src=t.fn;
          create_reachable;
          sparse_dir=Pre_io.sparse_name ~generation;
          suffix_dir=Pre_io.suffix_name ~generation;
        }
        in
        let `Pid pid = fork_worker ~worker_args in
        t0.worker_pid <- Some pid;
        ()

    let handle_worker_termination (_t:t) = 
      (* FIXME switch to the new sparse+suffix *)
      failwith "FIXME handle_worker_termination"

    let check_worker_status (t:t) =
      match t.worker_pid with 
      | None -> ()
      | Some pid -> 
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
    append t.base s

  let version t = version t.base

  let set_version t = set_version t.base

  let name t = name t.base

  let force_offset t = force_offset t.base

  (* let set_read_logger t opt = t.read_logger <- opt *)

end

include (Private : Irmin_pack_layers.IO.S)
