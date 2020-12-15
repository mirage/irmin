type empty = |

module type S = sig
  module Stat : sig
    val run : root:string -> unit Lwt.t
    (** Reads basic metrics from an existing store and prints them to stdout. *)

    val term : (unit -> unit) Cmdliner.Term.t
    (** A pre-packaged [Cmdliner] term for executing {!run}. *)
  end

  module Check_self_contained : sig
    val run : root:string -> unit Lwt.t
    (** Ensure that the upper layer of the store is self-contained.*)

    val term : (unit -> unit) Cmdliner.Term.t
    (** A pre-packaged [Cmdliner] term for executing {!run}. *)
  end

  module Reconstruct_index : sig
    val run : root:string -> output:string option -> unit
    (** Rebuilds an index for an existing pack file *)

    val term : (unit -> unit) Cmdliner.Term.t
    (** A pre-packaged [Cmdliner] term for executing {!run}. *)
  end

  module Integrity_check : sig
    val run : root:string -> auto_repair:bool -> unit Lwt.t
    (** Checks the integrity of a store *)

    val term : (unit -> unit) Cmdliner.Term.t
    (** A pre-packaged [Cmdliner] term for executing {!run}. *)
  end

  val cli : unit -> empty
  (** Run a [Cmdliner] binary containing tools for running offline checks. *)
end

module type Checks = sig
  type nonrec empty = empty

  module type S = S

  module Make
      (Conf : Config.S)
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S
                with type metadata = M.t
                 and type hash = H.t
                 and type step = P.step)
      (Commit : Irmin.Private.Commit.S with type hash = H.t) : S
end
