type empty = |

module type Subcommand = sig
  type run

  val run : run

  val term_internal : (unit -> unit) Cmdliner.Term.t
  (** A pre-packaged [Cmdliner] term for executing {!run}. *)

  val term : unit Cmdliner.Term.t * Cmdliner.Term.info
  (** [term] is {!term_internal} plus documentation and logs initialisation *)
end

module type S = sig
  (** Reads basic metrics from an existing store and prints them to stdout. *)
  module Stat : sig
    include Subcommand with type run := root:string -> unit Lwt.t

    (** Internal implementation utilities exposed for use in other integrity
        checks. *)

    type size = Bytes of int [@@deriving irmin]

    type io = { size : size; offset : int64; generation : int64 }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    val v : root:string -> files
  end

  module Reconstruct_index :
    Subcommand with type run := root:string -> output:string option -> unit
  (** Rebuilds an index for an existing pack file *)

  (** Checks the integrity of a store *)
  module Integrity_check : sig
    include
      Subcommand with type run := root:string -> auto_repair:bool -> unit Lwt.t

    val handle_result :
      ?name:string ->
      ( [< `Fixed of int | `No_error ],
        [< `Cannot_fix of string | `Corrupted of int ] )
      result ->
      unit
  end

  val cli :
    ?terms:(unit Cmdliner.Term.t * Cmdliner.Term.info) list -> unit -> empty
  (** Run a [Cmdliner] binary containing tools for running offline checks.
      [terms] defaults to the set of checks in this module. *)
end

module type Make_args = sig
  module Hash : Irmin.Hash.S

  module Store : sig
    include Irmin.S with type hash = Hash.t
    include Store.S with type repo := repo

    (* TODO(craigfe): avoid redefining this extension to [Store] repeatedly *)
    val reconstruct_index : ?output:string -> Irmin.config -> unit
  end
end

module type Checks = sig
  type nonrec empty = empty

  val setup_log : unit Cmdliner.Term.t
  val path : string Cmdliner.Term.t

  module type Subcommand = Subcommand
  module type S = S
  module type Make_args = Make_args

  module Make (_ : Make_args) : S
end
