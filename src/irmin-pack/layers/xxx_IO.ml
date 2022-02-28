(*
(** Alternative IO interface for pack_store.

This uses the object store, suffix file, control and meta to implement the normal
irmin-pack [IO] interface. 

6GB snapshot size, 30M objects, 6000/30 = 200 bytes average object size; 




*)

[@@@warning "-27"]

open! Import
(* open Util *)

open struct
  module Sparse = Sparse_file
end


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

  (* val set_read_logger: t -> out_channel option -> unit   *)

end


(** Private implementation *)
module Private = struct

  include Pre_io


end (* Private *)

module _  = (Private : S) (* check it matches the intf *)

include (Private (*: S *)) (* expose the underlying impl for now *)
*)
