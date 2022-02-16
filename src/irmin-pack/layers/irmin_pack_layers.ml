(** Top-level interface to library [irmin_pack_layers]. 

FIXME We want this to be accessible from irmin-pack; so either it is a library before
irmin-pack (but then we can't depend on eg Version.t from irmin-pack), or it is in the
irmin-pack library itself; this is probably the thing to do, but then we have to avoid
name clashes
*)

(** {1 Utils *)

module Util = Util

module External_sort = External_sort

(** {1 Suffix file, object store, and control file} *)

module Suffix = Suffix

module Sparse_file = Sparse_file

module Control = Control


(** {1 IO replacement, using above} *)

module IO = IO



module Private = struct
  module Obj_store = Obj_store
  module Worker = Worker
end
