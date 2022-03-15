open! Import
(* include module type of Irmin_pack.Pack_store *)

val selected_version : Version.t

(* NOTE we want to specify that Maker is like Irmin_pack.Pack_store_intf.Maker, but with
   some additional functionality in the final result signature S *)
module Maker
    (Index : Pack_index.S)
    (Hash : Irmin.Hash.S with type t = Index.key) :
sig
  (* This is like Pack_store_intf.Maker, but with S extended *)
  type hash = Hash.t
  type index := Index.t

  module Make
      (V : Pack_value.Persistent
       with type hash := hash
        and type key := hash Pack_key.t) :
    sig 
      include Irmin_pack.Pack_store_intf.S
      (** Layers operates at the IO level; we expose this here in order to provide access to
          the [Pack_store_IO.t] instance, and hence to GC functions etc. *)
      val get_pack_store_io: 'a t -> Pack_store_IO.t
    end
      with type key = hash Pack_key.t
       and type hash = hash
       and type value = V.t
       and type index := index
       (* and type indexing_strategy := indexing_strategy *)
end

val clear_all_caches: unit -> unit
