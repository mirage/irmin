(** This implements the "layered" version of the pack store IO. It builds on package
    [irmin-pack.layers]. That package provides the clean implementations of individual
    modules. In this module, we integrate those parts with the rest of [irmin-pack]. *)

open! Import

(** This module defines a record type with fields that are used when triggering a GC from
    a particular commit. *)
module Trigger_gc = struct
  type t = {
    commit_hash_s: string;
    (** The commit hash, as a string; used for debugging *)

    create_reachable: reachable_fn:string -> unit
    (** The function the IO layer should use to calculate the initial reachability data;
        this will be called in another process, so it should not use any file descriptors
        from the parent; instead, it should open the repository in readonly mode and then
        call Repo.iter *)
  }

end


(** This is the original interface that the [Pack_store] IO needed to provide. It is based
    on {!IO_intf} but omits those functions that are not actually used by the pack
    store. NOTE The original {!IO_intf} was not fully documented; further documentation is
    in a pull request: https://github.com/mirage/irmin/pull/1758 *)
module type S = sig 
  type t

  val v : version:Version.t option -> fresh:bool -> readonly:bool -> string -> t
  (* NOTE Handling of version is a bit subtle in the existing implementation in IO.ml; eg
     opening a V2 with V1 fails. *)

  val readonly : t -> bool

  val flush : t -> unit
  val close : t -> unit
  val offset : t -> int63
  (** For readonly instances, offset is the last offset returned by force_offset; this is
      used to trigger updates to the dictionary and index *)

  val read : t -> off:int63 -> bytes -> int
  val append : t -> string -> unit
  (* NOTE this is an append-only file *)

  val truncate : t -> unit

  val version : t -> Version.t
  val set_version : t -> Version.t -> unit
  val name : t -> string
  (* This is just the "filename"/path used when opening the IO instance *)

  val force_offset : t -> int63 

end

module type S_with_trigger_gc = sig
  include S

  (** [gc_worker_status t] returns the status of the GC worker process for [t], if any *)
  val gc_worker_status: t -> [ `Running of int (* pid *) | `None ]

  (** [trigger_gc t args] triggers a layers-GC for [t]; [args] is of type type
      {!Trigger_gc.t}. 

      If the trigger is already set, but no GC has started, the trigger will be
      overwritten and GC will not occur for the previous value of the trigger.

      If there is already a GC running, then when the main process checks the trigger, it
      will simply unset it. So you should check that {!gc_worker_status} is [`None] before
      calling {!trigger_gc}. *)
  val trigger_gc: t -> Trigger_gc.t -> unit
end


(** {!Irmin_pack_layers.Pre_io} provides a basic implementation of {!S}, but without
    conforming exactly to the interface above: for example, some types such as
    {!Version.t} are not known in [layers]. Here we add a shim layer so that we conform to
    {!S} exactly. *)
module Private_io_impl = struct

  open Irmin_pack_layers
  include Irmin_pack_layers.Pre_io

  let default_version = `V2

  let set_version t (ver:Version.t) = 
    let i = 
      match ver with 
      | `V1 -> 1
      | `V2 -> 2
    in
    set_version t i

  let version t = 
    version t |> function
    | 1 -> `V1
    | 2 -> `V2
    | _ -> default_version

  (* FIXME is fresh=true ever used in the codebase? what about fresh=true, readonly=true?
     is this allowed?

     is fresh,version allowed?

     Documentation from IO_intf:

     If [path] exists: 
     if [fresh]:
     - version must be (Some _)
     - version meta is updated to match that supplied as argument (even if this results in a
       downgrade of the version from [`V2] to [`V1]; even in readonly mode)
     - the offset is positioned at zero (the intent is probably to make the file appear
       truncated, but this is not what actually happens)
       if [not fresh]:
     - meta version is loaded
     - if meta version is > than that supplied as argument, fail

     If [path] does not exist:
     - version must be (Some _)
     - instance is created with the supplied version (even if readonly is true)

  *)
  (* NOTE This function is complicated because it attempts to replicate precisely the
     current behaviour of the [IO.ml] implementation (but some of that may be unintended
     behaviour that we don't need to model so precisely) *)
  let v ~version:(ver0:Version.t option) ~fresh ~readonly path = 
    let exists = Sys.file_exists path in
    let ( --> ) a b = (not a) || b in
    assert(not exists --> Option.is_some ver0);
    assert(not (readonly && fresh)); (* FIXME this is allowed in the existing code *)
    match exists with 
    | false -> (
        assert(not exists);
        assert(Option.is_some ver0);
        ignore(fresh);
        match readonly with
        | true -> 
          (* FIXME in the existing code, opening readonly will create the file if it
             doesn't exist *)
          Fmt.failwith 
            "%s: Cannot open a non-existent file %s in readonly mode." __FILE__ path
        | false -> 
          assert(not exists);
          assert(not readonly);
          assert(Option.is_some ver0);
          let t = create ~fn:path in
          let ver0 = Option.get ver0 in
          let _ = set_version t ver0 in
          let _ = flush t in
          t)
    | true -> (
        assert(exists);
        assert(fresh --> Option.is_some ver0);        
        let t = open_ ~readonly ~fn:path in
        (* handle fresh and ver0 *)
        begin 
          match fresh with
          | false -> 
            assert(exists);
            assert(not fresh);
            let _check_versions =
              let version_lt v1 v2 = Version.compare v1 v2 < 0 in
              match ver0 with
              | None -> ()
              | Some ver0 -> 
                match version_lt ver0 (version t) with
                | true ->
                  Fmt.failwith 
                    "%s: attempt to open %s, version %a file, using older %a version" __FILE__ 
                    path 
                    Version.pp 
                    (version t)
                    Version.pp
                    (ver0)
                | false -> ()
            in
            ()
          | true -> 
            assert(exists);
            assert(fresh);
            assert(not readonly); (* FIXME *)
            assert(Option.is_some ver0);
            let ver0 = Option.get ver0 in
            (* if fresh, then we want to bump the generation number and switch to a new
               suffix/sparse *)
            let gen' = Control.get_generation t.control +1 in
            let sparse = Sparse_file.create ~path:(sparse_name ~generation:gen') in
            let suffix = 
              let suffix_offset = 0 in
              Suffix.create ~root:(suffix_name ~generation:gen') ~suffix_offset in
            let old_sparse,old_suffix = t.sparse,t.suffix in
            t.sparse <- sparse;
            t.suffix <- suffix;
            Control.(set t.control generation_field gen');
            (* FIXME potential problem if update to generation is seen without the update to
               last_synced_offset_field? *)
            Control.(set t.control last_synced_offset_field 0); 
            set_version t ver0; (* use the provided version, not any in the existing file *)
            Control.fsync t.control;
            Sparse_file.close old_sparse;
            Suffix.close old_suffix;
            (* FIXME delete old generation sparse+suffix here *)            
        end;
        t)
end

open struct
  (* Verify that IO_impl does have signature S; we don't want to seal IO_impl because we
       want to be able to access fields such as [t.control] etc *)
  module _ : S = Private_io_impl
end


(** This further wraps {!Private_io_impl}, in order to handle forking the worker, and
    dealing with the worker when it terminates. *)
module Private (* : S *) = struct
  open struct 
    module IO_impl = Private_io_impl (* was IO.Unix *)
  end
  open IO_impl

  (** Wrap {!IO_impl.t} with two additional fields: [read_logger] which, if set, logs all
      reads; and [worker_pid_and_args] for dealing with the worker. *)
  type t = { 
    base:IO_impl.t; 
    mutable trigger_gc: Trigger_gc.t option;
    mutable read_logger: out_channel option; 
    mutable worker_pid_and_args: (int * Irmin_pack_layers.Worker.worker_args) option 
  }
           
  (** NOTE most of the functions that follow simply lift the underlying functionality *)
 
  let truncate t = truncate t.base

  let readonly t = readonly t.base

  let flush t = flush t.base

  let close t = close t.base
      
  let offset t = offset t.base

  let version t = version t.base

  let set_version t = set_version t.base

  let name t = name t.base

  let force_offset t = force_offset t.base



  module For_create_reach_exe = struct

    let already_done = ref false

    (** Optional output channel for logging reads; normally this is [None]; only used for
        [create_reach.exe]; only one IO instance will be logged. We assume it is the first
        one that is opened by [create_reach.exe]. See [create_reach.ml],
        INV_CREATE_REACH *)
    let get_reach_oc () =
      match !already_done with 
      | true -> None
      | false -> 
        match !Irmin_pack_layers.running_create_reach_exe with 
        | None -> None
        | Some fn -> 
          (* NOTE only return [Some _] if [running_create_reach_exe] is set *)
          let it = open_out_bin fn in
          already_done := true;
          Some it
  end
      

  (** [v] is as {!Private_io_impl.v}, except that, if [create_reach.exe] is running, then
      we log all reads. *)
  let v ~version ~fresh ~readonly path = 
    [%log.info "%s: v called\n%!" __FILE__];
    let read_logger = For_create_reach_exe.get_reach_oc () in
    { base=v ~version ~fresh ~readonly path; trigger_gc=None; read_logger; worker_pid_and_args=None}

  (** [read] is as {!Private_io_impl.read}, except that, if [is_some t.read_logger] then
      we log reads. *)
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

  (** The following concern the worker. *)

  open struct
    open Irmin_pack_layers
    open Irmin_pack_layers.Util
    (* open Irmin_pack_layers.Worker *)

    let check_trigger_maybe_fork_worker (t0:t) = 
      [%log.info "%s: check_trigger_maybe_fork_worker called" __FILE__];
      let t = t0.base in
      match t0.trigger_gc with
      | None -> ()
      | Some _ when Option.is_some t0.worker_pid_and_args -> 
        let (pid,_args) = Option.get t0.worker_pid_and_args in
        [%log.warn "%s: warning: GC triggered but a worker %d is already running for a \
                    previous GC; ignoring" __FILE__ pid];
        t0.trigger_gc <- None;
        ()        
      | Some { commit_hash_s=_; create_reachable } ->        
        [%log.info "%s: check_trigger_maybe_fork_worker: trigger_gc is_some ..." __FILE__];
        assert(t0.worker_pid_and_args = None); (* is_some case above *)
        let _ = t0.trigger_gc <- None in 
        let generation = 1+Control.get_generation t.control in
        let worker_args : Worker.worker_args = {
          working_dir=t.root;
          src=t.fn;
          create_reachable;
          sparse_dir=Pre_io.sparse_name ~generation;
          suffix_dir=Pre_io.suffix_name ~generation;
        }
        in
        [%log.info "%s: ... calling Worker.fork_worker" __FILE__];
        let `Pid pid = Worker.fork_worker ~worker_args in
        t0.worker_pid_and_args <- Some (pid,worker_args);
        ()

    let handle_worker_termination (t:t) = 
      [%log.info "%s: handle_worker_termination called" __FILE__];
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
      [%log.info "%s: checking existence of next sparse+suffix" __FILE__];
      assert(Sys.file_exists Fn.(args.working_dir / args.sparse_dir));
      assert(Sys.file_exists Fn.(args.working_dir / args.suffix_dir));
      let next_sparse = Sparse_file.open_ro ~dir:Fn.(args.working_dir / args.sparse_dir) in
      let next_suffix = Suffix.open_ ~root:Fn.(args.working_dir / args.suffix_dir) in
      (* between the termination of the worker, and the execution of this code in the
         parent, further data may have been written to the current suffix; we need to copy
         this across to the next suffix *)
      [%log.info "%s: copy additional data from current suffix to next" __FILE__];
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
      [%log.info "%s: switching to next sparse+suffix" __FILE__];
      let old_sparse, old_suffix = t.base.sparse,t.base.suffix in
      t.base.sparse <- next_sparse;
      t.base.suffix <- next_suffix;
      Sparse_file.close old_sparse; 
      Suffix.close old_suffix;
      (* increment generation *)
      let c = t.base.control in
      let _increment_generation = 
        Control.(set c generation_field (1+get_generation c));
        (* fsync, so that others can detect a generation change, and to persist the change
           on disk *)
        Control.(fsync c);
        ()
      in
      [%log.info "%s: switched to new sparse+suffix" __FILE__];
      let _remove_old_sparse_and_suffix = 
        [%log.info "%s: removing old files" __FILE__];
        match Irmin_pack_layers.debug_mode with 
        | true -> () (* don't delete *)
        | false -> 
          let (* old *) generation = (Control.get_generation c) -1 in
          let old_sparse_fn,old_suffix_fn = 
            Pre_io.sparse_name ~generation, Pre_io.suffix_name ~generation 
          in
          let root = t.base.root in
          [old_sparse_fn;old_suffix_fn] |> List.iter (fun n -> Util.rm_rf Fn.(root / n))
      in
      ()

    let check_worker_status (t:t) =
      [%log.info "%s: check_worker_status called" __FILE__];                
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
            | WEXITED 0 -> (
                [%log.info "%s: check_worker_status: worker terminated" __FILE__];                
                handle_worker_termination t)
            | WEXITED n -> 
              (* FIXME we should handle this case by, for example, issuing a warning to
                 the node owner *)
              failwith (P.s "Worker terminated unsuccessfully with %d" n)
            | _ -> 
              (* FIXME we should handle this case by, for example, issuing a warning to
                 the node owner *)
              failwith "Worker terminated abnormally")
        | _ -> failwith (P.s "Unexpected pid0 value %d" pid0)        
  end
      

  (** [append] is as {!Private_io_impl.append}, except that we use this point to 1) fork a
      worker if required; 2) handle a terminated worker, if any. *)
  let append t s = 
    [%log.info "%s: append called" __FILE__];
    check_trigger_maybe_fork_worker t;
    check_worker_status t;
    append t.base s

  let gc_worker_status t = 
    match t.worker_pid_and_args with
    | None -> `None
    | Some (pid,_) -> `Running pid

  let trigger_gc t trig =
    [%log.info "%s: trigger_gc called" __FILE__];
    t.trigger_gc <- Some trig

end

include (Private : S_with_trigger_gc)
