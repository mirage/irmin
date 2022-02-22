open! Import

(* NOTE moved from pack_store_intf.ml, to break cycle with S.ml *)

module Private (* : Irmin_pack_layers.IO.S *) = struct
  (* include IO.Unix *)
  
  module IO_impl = Irmin_pack_layers.IO (* IO.Unix *)
  include IO_impl

  type t = { base:IO_impl.t; mutable read_logger: out_channel option }
           
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
    { base=v ~version ~fresh ~readonly path; read_logger}

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

(* FIXME
  let check_trigger_maybe_fork_worker (t:IO_impl.t) = 
    match !trigger_gc with
    | None -> ()
    | Some commit_hash_s ->       
      let open Irmin_pack_layers.Worker in
      let worker_args : worker_args = {
        working_dir=IO_impl.get_working_dir t;
        src=failwith "";
        src_off=failwith "";
        src_len=failwith "";
        calc_rch_objs=failwith "";
        sparse_dir="";
        suffix_dir="";
      }
      in
      let `Pid pid = fork_worker ~worker_args
*)

  let append t s = 
    (* check_trigger_maybe_fork_worker t; *)
    append t.base s

  let version t = version t.base

  let set_version t = set_version t.base

  let name t = name t.base

  let force_offset t = force_offset t.base

  (* let set_read_logger t opt = t.read_logger <- opt *)

end

include (Private : Irmin_pack_layers.IO.S)
