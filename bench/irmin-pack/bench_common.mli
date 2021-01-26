val reporter : ?prefix:string -> unit -> Logs.reporter
val reset_stats : unit -> unit
val with_timer : (unit -> 'a Lwt.t) -> (float * 'a) Lwt.t
val info : unit -> Irmin.Info.t
val random_blob : unit -> string

module Conf : sig
  val entries : int
  val stable_hash : int
end

module FSHelper : sig
  val rm_dir : string -> unit
  val get_size : string -> int
  val print_size_layers : string -> unit
end

module Generate_trees
    (Store : Irmin.S with type contents = string and type key = string list) : sig
  val add_chain_trees : int -> int -> Store.tree -> Store.tree Lwt.t
  (** [add_chain_trees depth nb tree] adds [nb] random contents to [tree],
      depthwise. *)

  val add_large_trees : int -> int -> Store.tree -> Store.tree Lwt.t
  (** [add_large_trees width nb tree] adds [nb] random contents to [tree],
      breadthwise. *)
end
