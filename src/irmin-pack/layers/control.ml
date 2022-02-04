(** A control file, which records various bits of information about the IO state; updated
    atomically via rename *)

(** 

NOTE: structure of layers directory:

Typically the "generation" is some number 1234 say. Then we expect these files to exist:

- control (contains the generation number, and pointers to the rest of the data; RO
  instances should periodically check (via Unix.stat) whether this has changed, and if so
  resync)
- meta.1234 (the metadata file; does not contain the generation number)
- objects.1234/{sparse.data,sparse.map}
- suffix.1234/{upper.data,upper.offset}
*)

open Sexplib.Std
open Util

module T = struct
  type t = {
    generation : int;
    (** generation is incremented on every GC completion, to signal that RO instances
        should reload *)
    objects_dir : string;
    (** subdir which contains the obj store *)
    suffix_dir : string;
    (** subdir which contains the suffix data file, and offset file *)
    meta_fn : string
    (** filename for the meta data; does not include the generation number *)
  }
  [@@deriving sexp]
end

include T
include Add_load_save_funs (T)

let make ~generation = 
  let suff = string_of_int generation in
  {
    generation;
    objects_dir="objects."^suff;
    suffix_dir="suffix."^suff;
    meta_fn="meta."^suff
  }
