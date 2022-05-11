(** [Pre_io] is like {!Irmin_pack.Pack_store_IO}, but only deals with the
    sparse+suffix+control; there is no dependency on the worker. This means that the
    worker can use the functions here to interact with an existing store instance, without
    introducing a circularity IO<->Worker. We also {b do not} include functionality in
    [Pre_io] that is really something to do with irmin-pack. *)

open! Import
open Util


(** Private implementation *)
open struct
  module Sparse = Sparse_file
end

(** We want to store the control file, and all other files and directories related to
    layers, in a subdirectory; however, the existing code expects to be working with a
    single file; so we have a single file that points to the directory that holds all
    the underlying files. *)
module File_containing_pointer_to_subdir = struct
  module T = struct
    open Sexplib.Std
    type t = { 
      subdir_name:string;
      (** The name of the subdir. *)
    }[@@deriving sexp]
  end
  include T
  include Add_load_save_funs(T)
end

type ro_fields = {
  mutable readonly_offset: int;
  (** This offset is maintained by readonly instances; when the RW instance appends data
      to the pack file, the RO instance detects the new data (via force_offset) which
      triggers the RO instance to resync the index and the dictionary *)
  mutable readonly_generation: int;
  (** For a readonly instance, the generation that corresponds to the sparse+suffix in
      use. This can be checked against the control, and if it lags, the readonly instance
      can reload a new sparse+suffix. *)
}

type t = {
  fn      : string; (** file containing a pointer to a subdir *)
  root    : string; (** subdirectory where we store the files *)
  (* objects : Obj_store.t; *)
  mutable sparse  : Sparse.t;
  mutable suffix  : Suffix.t;
  control : Control.t;
  readonly_fields: ro_fields option;
}

(* FIXME need to examine the force_offset function and its affect on readonly
   instances *)

let suffix_name ~generation = "suffix."^(generation |> string_of_int)
(** Default name for suffix subdir; "suffix.nnnn" where nnnn is the generation number *)

let sparse_name ~generation = "sparse."^(generation |> string_of_int)
(** Default name for sparse subdir; "sparse.nnnn" where nnnn is the generation number *)


(** [create ~fn] create the IO instance using [fn] as the "pointer file", which points
    to a subdirectory containing various other files.

    If [fn] is [/path/to/some/file], then we expect to create a "root" subdirectory
    [/path/to/some/layers.nnnn], where nnnn is some integer to make the root name
    fresh.

    Within the root, there will be a control file, sparse file and suffix file.
*) 
let create ~fn:fn0 =
  assert(not (Sys.file_exists fn0));
  let dir,base = Filename.dirname fn0,Filename.basename fn0 in    
  (* now we need a subdirectory to work within; we check for a free name of the form
     "layers.nnnn"; this is relative to [dir] *)
  let layers_dot_nnnn = 
    0 |> iter_k (fun ~k n -> 
        let subdir = "layers." ^ (string_of_int n) in
        match Sys.file_exists Fn.(dir / subdir) with
        | false -> subdir
        | true -> k (n+1))
  in    
  (* need to create this directory *)
  let _ = mkdir ~path:Fn.(dir / layers_dot_nnnn) in
  (* now create the initial contents of the layers directory *)
  let root = Fn.(dir / layers_dot_nnnn) in
  (* use 1234 as initial generation, just for testing *)
  let init_gen = 1234 in
  let control = 
    let t = Control.create ~root ~name:control_s in
    Control.(set t generation_field init_gen);
    Control.(set t last_synced_offset_field 0);
    Control.fsync t;
    t
  in
  let sparse = Sparse.create ~path:Fn.(root / sparse_name ~generation:init_gen) in
  let suffix = 
    let init_suffix_offset = 0 in
    Suffix.create 
      ~root:Fn.(root / suffix_name ~generation:init_gen) 
      ~suffix_offset:init_suffix_offset
  in
  (* FIXME make sure to sync all above *)
  (* finally create the pointer to the subdir *)
  File_containing_pointer_to_subdir.save {subdir_name=layers_dot_nnnn} Fn.(dir/base);
  { fn=fn0; root; sparse; suffix; control; readonly_fields=None }

(** [cleanup ~root ~allowed_names] cleans the directory [root] by deleting all entries
    which are not in [allowed_names]; typically [allowed_names] contains the current names
    of the control, sparse and suffix. NOTE implementation not particularly efficient:
    [allowed_names] is expected to be small; the number of entries in [root] should be
    small *)
let cleanup ~root ~allowed_names = 
  let names = Util.read_small_dir root in
  let to_delete = names |> List.filter (fun x -> not (List.mem x allowed_names)) in
  to_delete |> List.iter (fun n -> Util.rm_rf Fn.(root / n))

let open_ ~readonly ~fn:fn0 = 
  assert(Sys.file_exists fn0);
  let dir,base = Filename.dirname fn0,Filename.basename fn0 in
  let layers_dot_nnnn = 
    File_containing_pointer_to_subdir.load Fn.(dir/base) |> fun x -> x.subdir_name 
  in
  let root = Fn.(dir / layers_dot_nnnn) in
  let control = Control.open_ ~root ~name:control_s in
  let generation = Control.get_generation control in
  (* remove any old files left over from a crashed worker; only if RW; otherwise we hit a
     bug where create_reach.exe deletes it's own reach.tmp file; also, it makes sense to
     only delete from the RW process *)
  let _remove_old_files =
    if not readonly then 
      cleanup ~root 
        ~allowed_names:[control_s; sparse_name ~generation; suffix_name ~generation ]
  in
  let sparse = Sparse.open_ro ~dir:Fn.(root / sparse_name ~generation) in
  (* FIXME probably want to take into account the "last_synced_offset" for the suffix, eg
     by truncating the suffix to this *)
  let suffix = Suffix.open_ ~root:Fn.(root / suffix_name ~generation) in
  let readonly_fields = 
    match readonly with
    | false -> None
    | true -> Some {
        readonly_offset=Control.(get control last_synced_offset_field);
        readonly_generation=generation
      }
  in
  { fn=fn0; root; sparse; suffix; control; readonly_fields }

let readonly t = Option.is_some t.readonly_fields

let readwrite t = not (readonly t)

(* FIXME the existing IO implementation uses a buffer for writes, and flush is used to
   actually flush the data to disk (with an fsync? FIXME); this presumably improves
   performance; do we want to do that here? an alternative is to use OCaml's native
   channels *)
let flush t = 
  assert(readwrite t);
  (* Suffix.fsync t.suffix; NOTE the existing pack store IO did not issue fsync on flush;
     it is safe to do so, but renders performance of eg snapshot import unusable; see
     https://github.com/mirage/irmin/issues/1840 *)
  (* NOTE the last_synced_offset_field is the "virtual" size of the suffix FIXME shouldn't
     Suffix.fsync update this field?; NOTE last_synced_offset_field is only updated here
     in flush; used to control WHEN RO instances sync? don't want them to sync on each
     write? *)
  Control.(set t.control last_synced_offset_field (Suffix.size t.suffix));
  (* FIXME may want to fsync control here *)
  ()

let close t = 
  (if readwrite t then flush t);
  Suffix.close t.suffix;
  Sparse.close t.sparse;
  Control.close t.control;
  ()

let offset t = 
  match t.readonly_fields with
  | None -> 
    Suffix.size t.suffix |> Int63.of_int
  | Some ro_fields -> 
    (* FIXME this is only used to trigger RO updates to dict and index? *)
    ro_fields.readonly_offset |> Int63.of_int

(** [maybe_reload_if_readonly t] reloads the sparse+suffix, if the generation has changed *)
let maybe_reload_if_readonly t =
  match t.readonly_fields with
  | None -> ()
  | Some ro_fields ->     
    match ro_fields.readonly_generation = Control.get_generation t.control with
    | true -> () (* no need to reload *)
    | false ->       
      (* generations differ; need to reload *)
      Sparse.close t.sparse;
      Suffix.close t.suffix;
      let root = t.root in
      let generation = Control.get_generation t.control in
      [%log.info "%s: RO instance for %s reloading for generation %d" __FILE__ t.fn generation];
      (* FIXME race condition here if generation gets bumped meanwhile *)
      let sparse = Sparse.open_ro ~dir:Fn.(root / sparse_name ~generation) in
      let suffix = Suffix.open_ ~root:Fn.(root / suffix_name ~generation) in
      t.sparse <- sparse;
      t.suffix <- suffix;
      [%log.info "%s: RO instance for %s reloaded; updating ro_fields" __FILE__ t.fn];
      (* NOTE following is only place where readonly_generation is changed *)
      ro_fields.readonly_generation <- generation;
      ro_fields.readonly_offset <- 
        (* FIXME not sure about readonly_offset; needs checking *)
        Control.(get t.control last_synced_offset_field);
      ()

let pread t ~off ~len ~buf = 
  maybe_reload_if_readonly t;
  match !off >= Suffix.private_suffix_offset t.suffix with
  | true -> 
    (* read from suffix *)
    Suffix.pread t.suffix ~off ~len ~buf
  | false -> 
    (* read from sparse *)
    Sparse.pread t.sparse ~off ~len ~buf

let size t = Suffix.size t.suffix

let read t ~off buf =
  let off = Int63.to_int off in
  let len = Bytes.length buf in
  pread t ~off:(ref off) ~len ~buf

let append t s = 
  assert(readwrite t);
  Suffix.append t.suffix s

let version t : int = 
  Control.(get t.control version_field) 

let set_version t (ver:int) = 
  assert(readwrite t);
  Control.(set t.control version_field ver)

let name t = t.fn

(* The semantics of [force_offset] in the original implementation is a bit unclear. Here
   we assume that it is used only for readonly instances. It accesses the
   "last_synced_offset" of the underlying RW instance, and updates [readonly_offset]
   from that. This is then used by the RO instance to trigger a reload of dictionary and
   index. *)
let force_offset t =   
  assert(readonly t);
  maybe_reload_if_readonly t;
  match t.readonly_fields with
  | None -> failwith "impossible"
  | Some ro_fields -> 
    ro_fields.readonly_offset <- Control.(get t.control last_synced_offset_field);
    ro_fields.readonly_offset |> Int63.of_int

let truncate t = 
  assert(readwrite t);
    (*
    Fmt.failwith "%s: truncate not supported (the only user in pack_store.ml should be \
                  clear, which is also not supported) FIXME except that [v ~fresh] \
                  probably uses truncate? " __FILE__
*)
  () (* FIXME at the moment we assume we are called on an empty file anyway FIXME but we need to support this apparently *)

(* FIXME truncate: this apparently needs to be supported:

   make -f Makefile.local run_m 
   eval $(opam env) && dune build ./bench/irmin-pack/tree.exe
   cp -f _build/default/bench/irmin-pack/*.exe /tmp
   /tmp/tree.exe --mode=trace --keep-stat-trace --keep-store --ncommits-trace=100000  ../medium_data4_100066commits.repr
   +000us  application Will check commit hashes against reference.
   src/irmin-pack/pack_store_IO.ml: v called
   +000us  application Store kept at /tmp/l/github/irmin-worktree-layered/_artefacts/581d2b1c-7c59-4de1-ac68-1cdd8fb4e70a/store
   tree: internal error, uncaught exception:
      (Failure
        "src/irmin-pack/layers/IO.ml: truncate not supported (the only user in pack_store.ml should be clear, which is also not supported)")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Irmin_pack__IO.Cache.memoize.cached_constructor in file "src/irmin-pack/IO.ml", line 248, characters 22-29
      Called from Irmin_pack__Pack_store.Maker.Make_without_close_checks.unsafe_v_no_cache in file "src/irmin-pack/pack_store.ml", line 192, characters 17-72
      Called from Irmin_pack__Pack_store.Maker.Make_without_close_checks.unsafe_v in file "src/irmin-pack/pack_store.ml", line 209, characters 10-97
      Called from Irmin_pack__Pack_store.Maker.Make_without_close_checks.v in file "src/irmin-pack/pack_store.ml", line 218, characters 8-74
      Called from Irmin_pack__Pack_store.Maker.Make.v in file "src/irmin-pack/pack_store.ml", line 568, characters 6-71
      Called from Irmin_pack__Ext.Maker.Make.X.Repo.unsafe_v in file "src/irmin-pack/ext.ml", line 154, characters 12-97
      Called from Lwt.Sequential_composition.catch in file "src/core/lwt.ml", line 2019, characters 16-20
      Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3068, characters 20-29
      Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 31, characters 10-20
      Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 118, characters 8-13
      Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 124, characters 4-13
      Called from Dune__exe__Tree.main in file "bench/irmin-pack/tree.ml", line 343, characters 4-677
      Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
      Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
   make: *** [Makefile.local:75: run_m] Error 125
*)

