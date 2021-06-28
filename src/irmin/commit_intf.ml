(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open S
open! Import

module type S = sig
  (** {1 Commit values} *)

  type t
  (** The type for commit values. *)

  type hash
  (** Type for keys. *)

  val v : info:Info.t -> node:hash -> parents:hash list -> t
  (** Create a commit. *)

  val node : t -> hash
  (** The underlying node. *)

  val parents : t -> hash list
  (** The commit parents. *)

  val info : t -> Info.t
  (** The commit info. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val hash_t : hash Type.t
  (** [hash_t] is the value type for {!hash}. *)
end

module type Maker = functor (H : Type.S) -> S with type hash = H.t

module type STORE = sig
  (** {1 Commit Store} *)

  include CONTENT_ADDRESSABLE_STORE

  val merge : [> read_write ] t -> info:Info.f -> key option Merge.t
  (** [merge] is the 3-way merge function for commit keys. *)

  (** [Key] provides base functions for commit keys. *)
  module Key : Hash.TYPED with type t = key and type value = value

  (** [Val] provides functions for commit values. *)
  module Val : S with type t = value and type hash = key

  module Node : Node.STORE with type key = Val.hash
  (** [Node] is the underlying node store. *)
end

module type HISTORY = sig
  (** {1 Commit History} *)

  type 'a t
  (** The type for store handles. *)

  type node
  (** The type for node values. *)

  type commit
  (** The type for commit values. *)

  type v
  (** The type for commit objects. *)

  val v :
    [> write ] t ->
    node:node ->
    parents:commit list ->
    info:Info.t ->
    (commit * v) Lwt.t
  (** Create a new commit. *)

  val parents : [> read ] t -> commit -> commit list Lwt.t
  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its immediate
      predecessors. *)

  val merge : [> read_write ] t -> info:Info.f -> commit Merge.t
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
    info:Info.f ->
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
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit, Merge.conflict) result Lwt.t
  (** Compute the {!lcas} of the two commit and 3-way merge the result. *)

  val closure :
    [> read ] t -> min:commit list -> max:commit list -> commit list Lwt.t
  (** Same as {{!NODE_GRAPH.closure} NODE_GRAPH.closure} but for the history
      graph. *)

  val iter :
    [> read ] t ->
    min:node list ->
    max:node list ->
    ?commit:(commit -> unit Lwt.t) ->
    ?edge:(node -> node -> unit Lwt.t) ->
    ?skip:(node -> bool Lwt.t) ->
    ?rev:bool ->
    unit ->
    unit Lwt.t
  (** Same as {{!NODE_GRAPH.iter} NODE_GRAPH.iter} but for traversing the
      history graph. *)

  (** {1 Value Types} *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)
end

module type Commit = sig
  module type S = S
  module type Maker = Maker

  module Make : Maker
  (** [Make] provides a simple implementation of commit values, parameterized by
      the commit and node keys [K]. *)

  (** V1 serialisation. *)
  module V1 (C : S) : sig
    include S with type hash = C.hash

    val import : C.t -> t
    val export : t -> C.t
  end

  module type STORE = STORE
  (** [STORE] specifies the signature for commit stores. *)

  (** [Store] creates a new commit store. *)
  module Store
      (N : Node.STORE) (C : sig
        include CONTENT_ADDRESSABLE_STORE with type key = N.key
        module Key : Hash.S with type t = key
        module Val : S with type t = value and type hash = key
      end) :
    STORE
      with type 'a t = 'a N.t * 'a C.t
       and type key = C.key
       and type value = C.value
       and type Key.t = C.Key.t
       and module Val = C.Val

  module type HISTORY = HISTORY
  (** [History] specifies the signature for commit history. The history is
      represented as a partial-order of commits and basic functions to search
      through that history are provided.

      Every commit can point to an entry point in a node graph, where
      user-defined contents are stored. *)

  (** Build a commit history. *)
  module History (C : STORE) :
    HISTORY
      with type 'a t = 'a C.t
       and type v = C.Val.t
       and type node = C.Node.key
       and type commit = C.key
end
