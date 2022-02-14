(** Alternative IO interface for pack_store.

This uses the object store, suffix file, control and meta to implement the normal
irmin-pack [IO] interface. 

6GB snapshot size, 30M objects, 6000/30 = 200 bytes average object size; 




*)

open! Import
open Util

(** NOTE this interface is documented also in https://github.com/mirage/irmin/pull/1758 *)
module type S = sig 
  type t
    
  val v : version:Version.t option -> fresh:bool -> readonly:bool -> string -> t
  (** Handling of version is a bit subtle in the existing implementation in IO.ml; eg
      opening a V2 with V1 fails! *)

  (* NOTE that there is a kind of caching of IO.t instances in irmin-pack, and this may
     assume that an IO.t has a name that refers to a non-directory file; for this reason,
     we might want to implement our layers via a control file, rather than storing
     everything in a subdir; but since a subdir is so much cleaner, let's just patch up
     the caching if it is broken *)

  (* following are file-like *)
  val readonly : t -> bool


  val flush : t -> unit
  val close : t -> unit
  val offset : t -> int63
  val read : t -> off:int63 -> bytes -> int
  val append : t -> string -> unit
  (* NOTE this is an append-only file *)

  val truncate : t -> unit
  (* FIXME not clear that we can implement this using layers; removing for time being as
     probably not needed? Although if we allow "fresh" as an open option, we presumably do
     have to implement it; OK; just start from a fresh instance *)

  (* These are not file-like; some doc added in {!IO_intf}. *)
  val version : t -> Version.t
  val set_version : t -> Version.t -> unit
  val name : t -> string
  (* This is just the "filename"/path used when opening the IO instance *)

  val force_offset : t -> int63 
  (* 
I think this is for readonly instances, to allow them to detect that there is more data to
read.

     
See doc in ../IO_intf.ml We probably have something like this for the layers: the
     suffix file likely contains metadata for the last synced position. NOTE there are
     various bits of metadata, some of which we consult more often than others; for
     example, the layered store has a "generation" incremented on each GC; but we also
     have "version" which changes rarely, and "max_flushed_offset" which probably changes
     quite a lot. If these are all in the same file, then potentially changes to eg
     "max_flushed_offset" are detected as "some change to metadata" and RO instances then
     reload the entire metadata. This is a bit inefficient. We really want to detect
     changes to one piece of metadata independently of another piece. The best way to do
     this is with an mmap'ed file for the per-file changes (version, max_flushed_offset,
     etc) and only change the control file when the generation changes. *)
end

(** Private implementation *)
module Private = struct

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

  type t = {
    fn      : string; (** file containing a pointer to a subdir *)
    root    : string; (** subdirectory where we store the files *)
    (* objects : Obj_store.t; *)
    sparse  : Sparse.t;
    suffix  : Suffix.t;
    control : Control.t;
    readonly: bool;
  }

  let suffix_name ~generation = "suffix."^(generation |> string_of_int)
  (** Default name for suffix subdir; "suffix.nnnn" where nnnn is the generation number *)

(*
  let objects_name ~generation = "objects."^(generation |> string_of_int)
  (** Default name for objects subdir; "objects.nnnn" where nnnn is the generation number *)
*)

  let sparse_name ~generation = "objects."^(generation |> string_of_int)
  (** Default name for sparse subdir; "sparse.nnnn" where nnnn is the generation number *)



(*
FIXME are meta and control the same? we want some cheap way to indicate that RO instances
should switch, but we don't want them repeatedly reading the control file; and we also
want to record some metadata which should also be easily accessible; so the control file
maybe just points to the relevant files, and is updated by atomic rename; then the
metadata is stored elsewhere, in meta.nnnn
*)

  (** [create ~fn] create the IO instance using [fn] as the "pointer file", which points
      to a subdirectory containing various other files.

      If [fn] is [/path/to/some/file], then we expect to create a "root" subdirectory
      [/path/to/some/layers.nnnn], where nnnn is some integer to make the root name
      fresh.
      
      Within the root, there will be a control file, object store, suffix, and meta.
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
          | true -> subdir
          | false -> k (n+1))
    in    
    (* now create the initial contents of the layers directory *)
    let root = Fn.(dir / layers_dot_nnnn) in
    let init_gen = 1234 (* FIXME *) in
    let control = 
      let t = Control.create ~root ~name:control_s in
      Control.(set t generation_field init_gen);
      Control.(set t last_synced_offset_field 0);
      Control.fsync t;
      t
    in
    (*
    let objects = 
      Obj_store.create ~root:Fn.(root / objects_name ~generation:init_gen) in
    *)
    let sparse = Sparse.create ~path:Fn.(root / sparse_name ~generation:init_gen) in
    let suffix = 
      (* NOTE in the following, the initial suffix_offset is 0 *)
      let suffix_offset = 0 in
      Suffix.create ~root:Fn.(root / suffix_name ~generation:init_gen) ~suffix_offset 
    in
    (* FIXME make sure to sync all above *)
    (* finally create the pointer to the subdir *)
    File_containing_pointer_to_subdir.save {subdir_name=layers_dot_nnnn} Fn.(dir/base);
    { fn=fn0; root; sparse; suffix; control; readonly=false }
    

  let open_ ~readonly ~fn:fn0 = 
    assert(Sys.file_exists fn0);
    let dir,base = Filename.dirname fn0,Filename.basename fn0 in
    let layers_dot_nnnn = 
      File_containing_pointer_to_subdir.load Fn.(dir/base) |> fun x -> x.subdir_name 
    in
    let root = Fn.(dir / layers_dot_nnnn) in
    let control = Control.open_ ~root ~name:control_s in
    let generation = Control.get_generation control in
    let sparse = Sparse.open_ro ~dir:Fn.(root / sparse_name ~generation) in
    (* let objects = Obj_store.open_ro ~root:Fn.(root / objects_name ~generation) in *)
    (* FIXME probably want to take into account the "last_synced_offset" for the suffix *)
    let suffix = Suffix.open_ ~root:Fn.(root / suffix_name ~generation) in
    (* FIXME when we open, we should take into account meta.last_synced_offset *)
    { fn=fn0; root; sparse; suffix; control; readonly }
    
  let readonly t = t.readonly

  (* FIXME the existing IO implementation uses a buffer for writes, and flush is used to
     actually flush the data to disk (with an fsync? FIXME); this presumably improves
     performance; do we want to do that here? an alternative is to use OCaml's native
     channels *)
  let flush t = 
    Suffix.fsync t.suffix;
    (* NOTE the last_synced_offset_field is the "virtual" size of the suffix *)
    Control.(set t.control last_synced_offset_field (Suffix.size t.suffix));
    (* FIXME may want to flush here *)
    ()


end
