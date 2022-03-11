(** Top-level interface to library [irmin_pack_layers]. 

This library provides support for "layers" or "garbage collection" of an irmin-pack
store. 

The existing [store.pack] file is a simple file. With this library, we replace a
single [store.pack] file with a combination of a "sparse file" and a "suffix file" (and an
additional "control" file). The idea is that, for some offset [off] in the original
[store.pack] file, all subsequent bytes are contained in the suffix file; and before [off]
we retain only some of the data (the data that is "live" from the original [store.pack]
file) in the sparse file. 

Even if the original [store.pack] file is very large, we expect that the live data is
relatively small, thus, splitting the [store.pack] file into a sparse+suffix requires much
less disk space (since the sparse file only has to store the {b live} data before the
offset).

The main implementation modules are:
- {!Suffix} for the suffix file
- {!Sparse_file} for the sparse file
- {!Control} for the control file
- {!Pre_io} which combines the above 3 modules and presents a file-like interface

GC is triggered by the main process, which forks a worker process to do most of the
work. The worker calculates the live regions of the file before the offset, then
constructs the next versions of the sparse+suffix. When the worker terminates, the main
process then switches to use these next versions.

*)

(** {1 Globals *)

(** [running_create_reach_exe] is a [string option ref]; by default this is None;
    this should only be set to [Some fn] by the [create_reach.exe] executable.

    When set, it disables various bits of caching in {!Irmin.Object_graph} and
    {!Irmin_pack.Pack_store}, in order that [Repo.iter] visits every object reachable from
    a particular commit. This is necessary to accurately compute the layers reachability
    data for the commit. 

    The flag also controls whether {!Irmin_pack.Pack_store_IO} logs reads to a file: If
    the ref is [Some fn] then reads will be logged to file [fn].

    This is only needed for [create_reach.exe]; as such, the ugliness is viewed as
    acceptable since it only affects the codebase when running this one executable, and
    can otherwise be ignored.
*)
let running_create_reach_exe : string option ref = ref None  


(** {1 Utils *)

module Util = Util

module External_sort = External_sort

(** {1 Suffix file, sparse file, and control file} *)

module Suffix = Suffix

module Sparse_file = Sparse_file

module Control = Control


(** {1 IO replacement, using above} *)

module Pre_io = Pre_io

(** {1 Worker, for computing reachability and constructing next versions of suffix and
    sparse} *)

module Worker = Worker


(** {1 Testing} *)

module Test = Test


