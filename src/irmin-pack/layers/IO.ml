(** Alternative IO interface for pack_store *)

open! Import
(* open Util *)

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
  (* We probably have something like this for the layers: the suffix file likely contains
     metadata for the last synced position. NOTE there are various bits of metadata, some
     of which we consult more often than others; for example, the layered store has a
     "generation" incremented on each GC; but we also have "version" which changes rarely,
     and "max_flushed_offset" which probably changes quite a lot. If these are all in the
     same file, then potentially changes to eg "max_flushed_offset" are detected as "some
     change to metadata" and RO instances then reload the entire metadata. This is a bit
     inefficient. We really want to detect changes to one piece of metadata independently
     of another piece. The best way to do this is with an mmap'ed file for the per-file
     changes (version, max_flushed_offset, etc) and only change the control file when the
     generation changes. *)
end

(** Private implementation *)
module Private = struct

end
