(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import

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
      offset : int63;
      generation : int63;
      version : version;
    }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    type objects = { nb_commits : int; nb_nodes : int; nb_contents : int }
    [@@deriving irmin]

    val v : root:string -> version:Version.t -> files
    val detect_version : root:string -> Version.t
    val traverse_index : root:string -> int -> objects
  end

  module Reconstruct_index :
    Subcommand
      with type run :=
            root:string ->
            output:string option ->
            ?index_log_size:int ->
            unit ->
            unit
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
  include S.S with type repo := repo and type commit := commit
end

module type Maker = functor (_ : Version.S) -> Versioned_store

type integrity_error = [ `Wrong_hash | `Absent_value ]

module type Sigs = sig
  type integrity_error = [ `Wrong_hash | `Absent_value ]
  type nonrec empty = empty

  val setup_log : unit Cmdliner.Term.t
  val path : string Cmdliner.Term.t

  module type Subcommand = Subcommand
  module type S = S
  module type Versioned_store = Versioned_store
  module type Maker = Maker

  module Make (_ : Maker) : S

  module Index (Index : Pack_index.S) : sig
    val integrity_check :
      ?ppf:Format.formatter ->
      auto_repair:bool ->
      check:
        (kind:[> `Commit | `Contents | `Node ] ->
        offset:int63 ->
        length:int ->
        Index.key ->
        (unit, [< `Absent_value | `Wrong_hash ]) result) ->
      Index.t ->
      ( [> `Fixed of int | `No_error ],
        [> `Cannot_fix of string | `Corrupted of int ] )
      result
  end
end
