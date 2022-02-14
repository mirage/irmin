(** Alternative IO interface for pack_store.

This uses the object store, suffix file, control and meta to implement the normal
irmin-pack [IO] interface. 

6GB snapshot size, 30M objects, 6000/30 = 200 bytes average object size; 




*)

[@@@warning "-27"]

open! Import
open Util

(** NOTE this interface is documented also in https://github.com/mirage/irmin/pull/1758 *)
module type S = sig 
  type t
    
  val v : version:Lyr_version.t option -> fresh:bool -> readonly:bool -> string -> t
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
  (** For readonly instances, offset is the last offset returned by force_offset; this is
      used to trigger updates to the dictionary and index *)

  val read : t -> off:int63 -> bytes -> int
  val append : t -> string -> unit
  (* NOTE this is an append-only file *)

  val truncate : t -> unit
  (* FIXME not clear that we can implement this using layers; removing for time being as
     probably not needed? Although if we allow "fresh" as an open option, we presumably do
     have to implement it; OK; just start from a fresh instance *)

  (* These are not file-like; some doc added in {!IO_intf}. *)
  val version : t -> Lyr_version.t
  val set_version : t -> Lyr_version.t -> unit
  val name : t -> string
  (* This is just the "filename"/path used when opening the IO instance *)

  val force_offset : t -> int63 
  (* 
I think this is for readonly instances, to allow them to detect that there is more data to
read... except that RO instances only read from particular offsets - there is no need for
them to "keep up with" a log file, for example. Instead, it is used to indicate to the RO instance that it needs to resync the dict and index

     
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
    mutable sparse  : Sparse.t;
    mutable suffix  : Suffix.t;
    control : Control.t;
    readonly: bool;
    mutable readonly_offset: int;
    (** This offset is maintained by readonly instances; when the RW instance appends data
        to the pack file, the RO instance detects the new data (via force_offset) which
        triggers the RO instance to resync the index and the dictionary *)
  }

  (* FIXME need to examine the force_offset function and its affect on readonly
     instances *)

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
    let readonly_offset = 0 in
    { fn=fn0; root; sparse; suffix; control; readonly=false; readonly_offset; }
    

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
    let readonly_offset = Control.(get control last_synced_offset_field) in
    (* FIXME when we open, we should take into account meta.last_synced_offset *)
    { fn=fn0; root; sparse; suffix; control; readonly; readonly_offset }
    
  let readonly t = t.readonly

  (* FIXME the existing IO implementation uses a buffer for writes, and flush is used to
     actually flush the data to disk (with an fsync? FIXME); this presumably improves
     performance; do we want to do that here? an alternative is to use OCaml's native
     channels *)
  let flush t = 
    assert(not t.readonly);
    Suffix.fsync t.suffix;
    (* NOTE the last_synced_offset_field is the "virtual" size of the suffix FIXME
       shouldn't Suffix.fsync update this field? *)
    Control.(set t.control last_synced_offset_field (Suffix.size t.suffix));
    (* FIXME may want to flush here *)
    ()
    
  let close t = 
    (if not t.readonly then flush t);
    Suffix.close t.suffix;
    Sparse.close t.sparse;
    Control.close t.control;
    ()

  let offset t = 
    match t.readonly with
    | false -> 
      Suffix.size t.suffix
    | true -> 
      (* FIXME this is only used to trigger RO updates to dict and index? *)
      t.readonly_offset 
  
  let read t ~off buf =
    match off >= Suffix.private_suffix_offset t.suffix with
    | true -> 
      (* read from suffix *)
      Suffix.pread t.suffix ~off:(ref off) ~len:(Bytes.length buf) ~buf
    | false -> 
      (* read from sparse *)
      Sparse.pread t.sparse ~off:(ref off) ~len:(Bytes.length buf) ~buf

  let append t s = Suffix.append t.suffix s

  (* [version] and [set_version] need to be changed when more versions are added. If a new
     version is added, we will get a type mismatch for these functions, which will trigger
     the programmer to rewrite them. *)
  let version t : Lyr_version.t = Control.(get t.control version_field) |> function
    | 1 -> `V1
    | 2 -> `V2
    | ver -> 
      Log.err (fun m -> m "Unrecognized version: %d" ver);
      Fmt.(failwith "Unrecognized version: %d" ver)

  let set_version t (ver:Lyr_version.t) = 
    assert(not t.readonly);
    let ver_i = match ver with `V1 -> 1 | `V2 -> 2 in
    Control.(set t.control version_field ver_i)

  let name t = t.fn

  (* The semantics of [force_offset] in the original implementation is a bit unclear. Here
     we assume that it is used only for readonly instances. It accesses the
     "last_synced_offset" of the underlying RW instance, and updates [readonly_offset]
     from that. This is then used by the RO instance to trigger a reload of dictionary and
     index. *)
  let force_offset t = 
    assert(t.readonly);
    t.readonly_offset <- Control.(get t.control last_synced_offset_field);
    t.readonly_offset

  let truncate t = 
    assert(not t.readonly);    
    Fmt.failwith "%s: truncate not supported (the only user in pack_store.ml should be \
                  clear, which is also not supported)" __FILE__

  
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
  let v ~version:(ver0:Lyr_version.t option) ~fresh ~readonly path t = 
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
            assert(List.mem (version t) [`V1;`V2]);
            let _check_versions =
              let version_lt v1 v2 = Lyr_version.compare v1 v2 < 0 in
              match ver0 with
              | None -> ()
              | Some ver0 -> 
                match version_lt ver0 (version t) with
                | true ->
                  Fmt.failwith 
                    "%s: attempt to open %s, V%d file, using older V%d version" __FILE__ 
                    path 
                    (version t |> Lyr_version.to_int) 
                    (ver0 |> Lyr_version.to_int)
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
            let sparse = Sparse.create ~path:(sparse_name ~generation:gen') in
            let suffix = 
              let suffix_offset = 0 in
              Suffix.create ~root:(suffix_name ~generation:gen') ~suffix_offset in
            t.sparse <- sparse;
            t.suffix <- suffix;
            Control.(set t.control generation_field gen');
            (* FIXME potential problem if update to generation is seen without the update to
               last_synced_offset_field? *)
            Control.(set t.control last_synced_offset_field 0); 
            set_version t ver0; (* use the provided version, not any in the existing file *)
            Control.fsync t.control;
            (* FIXME delete old generation sparse+suffix here *)
        end;
        t)

end (* Private *)
