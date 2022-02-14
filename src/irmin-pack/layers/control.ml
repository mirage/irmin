(** The control file records which are the "current" implementation files (objects,
    suffix), as well as some metadata: generation, version, last flushed offset.


The control file uses an mmap. RO instances should check generation and resync if required.

NOTE 
*)

(** 

NOTE: structure of layers directory:

Typically the "generation" is some number 1234 say. Then we expect these files to exist:

- control (contains the generation number, and pointers to the rest of the data; RO
  instances should check the generation to see when it changes, and if so
  resync)
- objects.1234/ \{objects.data,objects.map\}
- suffix.1234/ \{suffix.data,suffix.offset\}
*)

open Util

module Private = struct

  (* FIXME fragile - when versions change/ the default version changes, we need to adjust
     this code *)
  let default_version = 2

  type t  = Int_mmap.t

  (** Metadata fields are identifed by their index in the array; use the predefined fields,
      don't create your own *)
  type field = int

  let generation_field : field = 0

  let version_field : field = 1

  let last_synced_offset_field : field = 2

  let max_field = last_synced_offset_field

  let set t index v = t.Int_mmap.arr.{ index } <- v

  let get t index = t.Int_mmap.arr.{ index }

  let create ~root ~name = 
    let ok = not (Sys.file_exists Fn.(root / name)) in
    assert(ok);
    let t = Int_mmap.create ~fn:Fn.(root / name) ~sz:(max_field+1) in
    set t version_field default_version;
    t

  let open_ ~root ~name = 
    let ok = Sys.file_exists Fn.(root / name) in
    assert(ok);
    let t = Int_mmap.open_ ~fn:Fn.(root / name) ~sz:(max_field+1) in
    t

  let close t = Int_mmap.close t

  (* NOTE it is believed that under Linux this implies that the mmap is flushed as if via
     msync FIXME perhaps use msync instead *)
  let fsync (t:t) = Unix.fsync t.fd

  let get_generation t = get t generation_field

end

include (Private : sig
  type t
  type field
  val generation_field : field
  val version_field : field
  val last_synced_offset_field : field
  val set : t -> field -> int -> unit
  val get : t -> field -> int
  val create : root:string -> name:string -> t
  val open_ : root:string -> name:string -> t
  val fsync : t -> unit
  val close : t -> unit
  val get_generation : t -> int
  (** Convenience; just [get t generation] *)
end)
