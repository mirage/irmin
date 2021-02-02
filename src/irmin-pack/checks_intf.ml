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
    type version = [ `V1 | `V2 ] [@@deriving irmin]

    type io = {
      size : size;
      offset : int64;
      generation : int64;
      version : version;
    }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    val v : root:string -> version:IO.version -> files
    val detect_version : root:string -> IO.version
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

  (** Checks the integrity of inodes in a store *)
  module Integrity_check_inodes : sig
    include
      Subcommand
        with type run := root:string -> heads:string list option -> unit Lwt.t
  end

  val cli :
    ?terms:(unit Cmdliner.Term.t * Cmdliner.Term.info) list -> unit -> empty
  (** Run a [Cmdliner] binary containing tools for running offline checks.
      [terms] defaults to the set of checks in this module. *)
end

module type Versioned_store = sig
  include Irmin.S
  include Store.S with type repo := repo

  (* TODO(craigfe): avoid redefining this extension to [Store] repeatedly *)
  val reconstruct_index : ?output:string -> Irmin.config -> unit

  val integrity_check_inodes :
    ?heads:commit list ->
    repo ->
    ([> `Msg of string ], [> `Msg of string ]) result Lwt.t
end

module type MAKER = functor (_ : IO.VERSION) -> Versioned_store

module type Checks = sig
  type nonrec empty = empty

  val setup_log : unit Cmdliner.Term.t
  val path : string Cmdliner.Term.t

  module type Subcommand = Subcommand
  module type S = S
  module type Versioned_store = Versioned_store
  module type MAKER = MAKER

  module Make (_ : MAKER) : S
end
