(** Top-level interface to library [irmin_pack_layers]. 

FIXME We want this to be accessible from irmin-pack; so either it is a library before
irmin-pack (but then we can't depend on eg Version.t from irmin-pack), or it is in the
irmin-pack library itself; this is probably the thing to do, but then we have to avoid
name clashes
*)

(** {1 Globals *)

(** [running_create_reach_exe] is a [string option ref]; by default this is None;
    this should only be set to [Some fn] by the [create_reach.exe] executable.

    When set, it disables various bits of caching in {!Irmin_pack.Object_graph} and
    {!Irmin_pack.Pack_store}, in order that [Repo.iter] visits every object reachable from
    a particular commit. This is necessary to accurately compute the layers reachability
    data for the commit. 

    The flag also controls whether the {!Pack_store_IO} logs reads to a file. If the ref
    is [Some fn] then reads will be logged to file [fn].

    This is only needed for [create_reach.exe]; as such, the ugliness is viewed as
    acceptable since it only affects the codebase when running this one executable, and
    can otherwise be ignored.
*)
let running_create_reach_exe : string option ref = ref None  


(** {1 Utils *)

module Util = Util

module External_sort = External_sort

(** {1 Suffix file, object store, and control file} *)

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


