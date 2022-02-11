(** Worker process, responsible for calculating the reachable objects and constructing the
    next versions of the sparse file + suffix. 

*)

[@@@warning "-33"]

open Util

[@@@warning "-27"]

module type Pack_with_commit = sig
  module S : Irmin_pack.S
               
  val commit : S.commit
end

(** Calculate [(off,len)] data for reachable objects and write them to the file
    [reachable_fn] in the same format that is used by {!Int_mmap}. *)
let calculate_reachable_objects ~(pack_with_commit:(module Pack_with_commit)) ~reachable_fn = ()
(* NOTE at the moment I am not quite sure how to traverse the store - Repo.iter? Also, we
   need to grow the mmap as the reachable objects become more numerous... but we are only
   appending to fn so we can just buffer some data, then mmap and write out *)

(** [sort_reachable ~src ~dst] sorts the [(off,len)] data in [src] and places it in [dst];
    [dst] should be at least as big as [src] *)
let sort_reachable ~src ~dst = ()

let create_sparse_file ~reachable_fn = ()
