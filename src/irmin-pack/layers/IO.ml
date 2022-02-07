(** Alternative IO interface for pack_store.

This uses the object store, suffix file, control and meta to implement the normal
irmin-pack [IO] interface. 
*)

open! Import
open Util

module type S = sig 
  type t
    
  (* What is the meaning of [version]? if it is set, does it have to agree with the
     underlying impl? At the moment, files are always opened with [version:`V2] (see
     selected_version below), and an existing `V1 file will be upgraded silently when
     things are written to it. So, we should have version metadata with the file and
     simulate this behaviour. *)
  val v : version:Version.t option -> fresh:bool -> readonly:bool -> string -> t

  (* FIXME note that there is a kind of caching of IO.t instances in irmin-pack, and this
     may assume that an IO.t has a name that refers to a non-directory file; for this
     reason, we might want to implement our layers via a control file, rather than storing
     everything in a subdir; but since a subdir is so much cleaner, let's just patch up
     the caching if it is broken *)

  (* following are file-like *)
  val truncate : t -> unit
  (* FIXME not clear that we can implement this using layers; removing for time being as
     probably not needed? Although if we allow "fresh" as an open option, we presumably do
     have to implement it; OK; just start from a fresh instance *)

  val readonly : t -> bool
  val flush : t -> unit
  val close : t -> unit
  val offset : t -> int63
  val read : t -> off:int63 -> bytes -> int
  val append : t -> string -> unit
  (* NOTE this is an append-only file *)

  (* These are not file-like; some doc added in {!IO_intf}. *)
  val version : t -> Version.t
  (* We can probably support a "version" in our metadata *)
  val set_version : t -> Version.t -> unit
  val name : t -> string
  (* This is just the "filename"/path used when opening the IO instance *)
  val force_offset : t -> int63 
  (* See doc in ../IO_intf.ml We probably have something like this for the layers: the
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

  let create_object_store ~root:_ : unit = failwith "FIXME"
  let create_suffix_file ~root:_ : unit = failwith "FIXME"

  type t = {
    fn      : string; (** file containing a pointer to a subdir *)
    root    : string; (** subdirectory where we store the files *)
    objects : Obj_store.t;
    suffix  : Suffix.t;
    control : Control.t;
    readonly: bool;
  }

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
    let control = 
      let t = Control.create ~root ~name:control_s in
      Control.(set t generation_field 1234);
      Control.(set t last_synced_offset_field 0);
      Control.fsync t;
      t
    in
    let objects = Obj_store.create ~root:Fn.(root / Control.(objects_name control)) in
    (* NOTE in the following, the initial suffix_offset is 0 *)
    let suffix = Suffix.create ~root:Fn.(root / Control.(suffix_name control)) ~suffix_offset:0 in
    (* FIXME make sure to sync all above *)
    (* finally create the pointer to the subdir *)
    File_containing_pointer_to_subdir.save {subdir_name=layers_dot_nnnn} Fn.(dir/base);
    { fn=fn0; root; objects; suffix; control; readonly=false }
    

  let open_ ~readonly ~fn:fn0 = 
    assert(Sys.file_exists fn0);
    let dir,base = Filename.dirname fn0,Filename.basename fn0 in
    let layers_dot_nnnn = 
      File_containing_pointer_to_subdir.load Fn.(dir/base) |> fun x -> x.subdir_name 
    in
    let root = Fn.(dir / layers_dot_nnnn) in
    let control = Control.open_ ~root ~name:control_s in
    let objects = Obj_store.open_ro ~root:Fn.(root / Control.(objects_name control)) in
    let suffix = Suffix.open_ ~root:Fn.(root / Control.(suffix_name control)) in
    (* FIXME when we open, we should take into account meta.last_synced_offset *)
    { fn=fn0; root; objects; suffix; control; readonly }
    


end
