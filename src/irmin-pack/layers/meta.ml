(** We record file metadata -- version, last flushed offset, etc. -- in a separate file 

FIXME perhaps we want two ints: generation, current_generation; when we switch, we create
new files, and bump the current_generaiton in teh old meta file; this triggers RO
instances to resync; but then we lose the idea that control is a single point of truth; so
perhaps just stick with control
*)

open Util

type t  = {
  fd: Unix.file_descr;
  arr: int_bigarray;
}

(** Metadata fields are identifed by their index in the array; use the predefined fields,
    don't create your own *)
type field = {index:int}

let last_synced_offset : field = {index=0}

let version : field = {index=1}

(** [length] is the number of metadata fields, and hence the size of the array *)
let length = 2


(** When creating, we first write to a tmp file, then rename, then open, to avoid partial
    creation errors *)
let create ~root:_ ~name:_ = failwith ""
