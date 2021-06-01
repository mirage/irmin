(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  (** {1 Commit values} *)

  type t [@@deriving irmin]
  (** The type for commit values. *)

  type hash [@@deriving irmin]
  (** Type for keys. *)

  module Info : Info.S
  (** The type for commit info. *)

  val v : info:Info.t -> node:hash -> parents:hash list -> t
  (** Create a commit. *)

  val node : t -> hash
  (** The underlying node. *)

  val parents : t -> hash list
  (** The commit parents. *)

  val info : t -> Info.t
  (** The commit info. *)
end

module type Maker = sig
  module Info : Info.S
  module Make (H : Type.S) : S with type hash = H.t and module Info = Info
end

module type Store = sig
  (** {1 Commit Store} *)

  include Content_addressable.S

  module Info : Info.S
  (** Commit info. *)

  (** [Key] provides base functions for commit keys. *)
  module Key : Hash.Typed with type t = key and type value = value

  (** [Val] provides functions for commit values. *)
  module Val : S with type t = value and type hash = key and module Info := Info

  module Node : Node.Store with type key = Val.hash
  (** [Node] is the underlying node store. *)

  val merge : [> read_write ] t -> info:Info.f -> key option Merge.t
  (** [merge] is the 3-way merge function for commit keys. *)
end

module type History = sig
  (** {1 Commit History} *)

  type 'a t
  (** The type for store handles. *)

  type node [@@deriving irmin]
  (** The type for node values. *)

  type commit [@@deriving irmin]
  (** The type for commit values. *)

  type v [@@deriving irmin]
  (** The type for commit objects. *)

  type info [@@deriving irmin]
  (** The type for commit info. *)

  val v :
    [> write ] t ->
    node:node ->
    parents:commit list ->
    info:info ->
    (commit * v) Lwt.t
  (** Create a new commit. *)

  val parents : [> read ] t -> commit -> commit list Lwt.t
  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its immediate
      predecessors. *)

  val merge : [> read_write ] t -> info:(unit -> info) -> commit Merge.t
  (** [merge t] is the 3-way merge function for commit. *)

  val lcas :
    [> read ] t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t
  (** Find the lowest common ancestors
      {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor} lca} between two
      commits. *)

  val lca :
    [> read_write ] t ->
    info:(unit -> info) ->
    ?max_depth:int ->
    ?n:int ->
    commit list ->
    (commit option, Merge.conflict) result Lwt.t
  (** Compute the lowest common ancestors ancestor of a list of commits by
      recursively calling {!lcas} and merging the results.

      If one of the merges results in a conflict, or if a call to {!lcas}
      returns either [Error `Max_depth_reached] or [Error `Too_many_lcas] then
      the function returns the same error. *)

  val three_way_merge :
    [> read_write ] t ->
    info:(unit -> info) ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit, Merge.conflict) result Lwt.t
  (** Compute the {!lcas} of the two commit and 3-way merge the result. *)

  val closure :
    [> read ] t -> min:commit list -> max:commit list -> commit list Lwt.t
  (** Same as {{!Node.Graph.closure} Node.Graph.closure} but for the history
      graph. *)

  val iter :
    [> read ] t ->
    min:commit list ->
    max:commit list ->
    ?commit:(commit -> unit Lwt.t) ->
    ?edge:(commit -> node -> unit Lwt.t) ->
    ?skip:(commit -> bool Lwt.t) ->
    ?rev:bool ->
    unit ->
    unit Lwt.t
  (** Same as {{!Node.Graph.iter} Node.Graph.iter} but for traversing the
      history graph. *)
end

module type Sigs = sig
  module type S = S
  module type Maker = Maker

  (** [Maker] provides a simple implementation of commit values, parameterized
      by commit info. *)
  module Maker (I : Info.S) : Maker with module Info = I

  (** V1 serialisation. *)
  module V1 : sig
    module Info : Info.S with type t = Info.Default.t
    (** Serialisation format for V1 info. *)

    module Make (C : S with module Info := Info) : sig
      include S with module Info = Info and type hash = C.hash

      val import : C.t -> t
      val export : t -> C.t
    end
  end

  module type Store = Store
  (** [Store] specifies the signature for commit stores. *)

  (** [Store] creates a new commit store. *)
  module Store
      (I : Info.S)
      (N : Node.Store)
      (S : Content_addressable.S with type key = N.key)
      (K : Hash.S with type t = S.key)
      (V : S with type hash = S.key and type t = S.value and module Info := I) :
    Store
      with type 'a t = 'a N.t * 'a S.t
       and type key = S.key
       and type value = S.value
       and module Info = I
       and module Val = V

  module type History = History
  (** [History] specifies the signature for commit history. The history is
      represented as a partial-order of commits and basic functions to search
      through that history are provided.

      Every commit can point to an entry point in a node graph, where
      user-defined contents are stored. *)

  (** Build a commit history. *)
  module History (C : Store) :
    History
      with type 'a t = 'a C.t
       and type v = C.Val.t
       and type node = C.Node.key
       and type commit = C.key
       and type info = C.Info.t

  include Maker with module Info = Info.Default
end
