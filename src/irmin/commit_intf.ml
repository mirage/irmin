(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S_generic_key = sig
  (** {1 Commit values} *)

  type t [@@deriving irmin]
  (** The type for commit values. *)

  type node_key [@@deriving irmin]
  (** Type for node keys. *)

  type commit_key [@@deriving irmin]
  (** Type for commit keys. *)

  module Info : Info.S
  (** The type for commit info. *)

  val v : info:Info.t -> node:node_key -> parents:commit_key list -> t
  (** Create a commit. *)

  val node : t -> node_key
  (** The underlying node key. *)

  val parents : t -> commit_key list
  (** The commit parents. *)

  val info : t -> Info.t
  (** The commit info. *)
end

module type S = sig
  type hash [@@deriving irmin]

  (** @inline *)
  include S_generic_key with type node_key = hash and type commit_key = hash
end

module type Portable = sig
  include S

  type commit

  val of_commit : commit -> t
end

open struct
  module S_is_a_generic_key (X : S) : S_generic_key = X
end

module type Maker_generic_key = sig
  module Info : Info.S

  module Make
      (H : Type.S)
      (N : Key.S with type hash = H.t)
      (C : Key.S with type hash = H.t) : sig
    include
      S_generic_key
        with type node_key = N.t
         and type commit_key = C.t
         and module Info = Info

    module Portable :
      Portable with type commit := t and type hash := H.t and module Info = Info
  end

  module Make_v2
      (H : Type.S)
      (N : Key.S with type hash = H.t)
      (C : Key.S with type hash = H.t) : sig
    include
      S_generic_key
        with type node_key = N.t
         and type commit_key = C.t
         and module Info = Info

    module Portable :
      Portable with type commit := t and type hash := H.t and module Info = Info
  end
end

module type Maker = sig
  module Info : Info.S
  module Make (H : Type.S) : S with type hash = H.t and module Info = Info
end

module type Store = sig
  (** {1 Commit Store} *)

  include Indexable.S

  module Info : Info.S
  (** Commit info. *)

  (** [Val] provides functions for commit values. *)
  module Val :
    S_generic_key
      with type t = value
       and type commit_key = key
       and module Info := Info

  module Hash : Hash.Typed with type t = hash and type value = value

  module Node : Node.Store with type key = Val.node_key
  (** [Node] is the underlying node store. *)

  val merge : [> read_write ] t -> info:Info.f -> key option Merge.t
  (** [merge] is the 3-way merge function for commit keys. *)
end

module type History = sig
  (** {1 Commit History} *)

  type 'a t
  (** The type for store handles. *)

  type node_key [@@deriving irmin]
  (** The type for node keys. *)

  type commit_key [@@deriving irmin]
  (** The type for commit keys. *)

  type v [@@deriving irmin]
  (** The type for commit objects. *)

  type info [@@deriving irmin]
  (** The type for commit info. *)

  val v :
    [> write ] t ->
    node:node_key ->
    parents:commit_key list ->
    info:info ->
    (commit_key * v) Lwt.t
  (** Create a new commit. *)

  val parents : [> read ] t -> commit_key -> commit_key list Lwt.t
  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its immediate
      predecessors. *)

  val merge : [> read_write ] t -> info:(unit -> info) -> commit_key Merge.t
  (** [merge t] is the 3-way merge function for commit. *)

  val lcas :
    [> read ] t ->
    ?max_depth:int ->
    ?n:int ->
    commit_key ->
    commit_key ->
    (commit_key list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t
  (** Find the lowest common ancestors
      {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor} lca} between two
      commits. *)

  val lca :
    [> read_write ] t ->
    info:(unit -> info) ->
    ?max_depth:int ->
    ?n:int ->
    commit_key list ->
    (commit_key option, Merge.conflict) result Lwt.t
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
    commit_key ->
    commit_key ->
    (commit_key, Merge.conflict) result Lwt.t
  (** Compute the {!lcas} of the two commit and 3-way merge the result. *)

  val closure :
    [> read ] t ->
    min:commit_key list ->
    max:commit_key list ->
    commit_key list Lwt.t
  (** Same as {{!Node.Graph.closure} Node.Graph.closure} but for the history
      graph. *)

  val iter :
    [> read ] t ->
    min:commit_key list ->
    max:commit_key list ->
    ?commit:(commit_key -> unit Lwt.t) ->
    ?edge:(commit_key -> commit_key -> unit Lwt.t) ->
    ?skip:(commit_key -> bool Lwt.t) ->
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

  (** [Generic_key] generalises the concept of "commit" to one that supports
      object keys that are not strictly equal to hashes. *)
  module Generic_key : sig
    module type S = S_generic_key
    module type Maker = Maker_generic_key

    module Maker (I : Info.S) : Maker with module Info = I

    module Store
        (I : Info.S)
        (N : Node.Store)
        (S : Indexable.S)
        (H : Hash.S with type t = S.hash)
        (V : S
               with type node_key = N.key
                and type commit_key = S.key
                and type t = S.value
                and module Info := I) :
      Store
        with type 'a t = 'a N.t * 'a S.t
         and type key = S.key
         and type value = S.value
         and module Info = I
         and type hash = S.hash
         and module Val = V

    include Maker with module Info = Info.Default
  end

  (** V1 serialisation. *)
  module V1 : sig
    module Info : Info.S with type t = Info.Default.t
    (** Serialisation format for V1 info. *)

    module Make (Hash : Hash.S) (C : Generic_key.S with module Info := Info) : sig
      include
        Generic_key.S
          with module Info = Info
           and type node_key = C.node_key
           and type commit_key = C.commit_key

      val import : C.t -> t
      val export : t -> C.t
    end
  end

  module Portable : sig
    (** Portable form of a commit implementation that can be constructed from a
        concrete representation and used in computing hashes. Conceptually, a
        [Commit.Portable.t] is a [Commit.t] in which all internal keys have been
        replaced with the hashes of the values they point to.

        As with {!Node.Portable}, computations over portable values must commute
        with those over [t]s. *)

    (** A node implementation with hashes for keys is trivially portable: *)
    module Of_commit (S : S) :
      Portable
        with type commit := S.t
         and type t = S.t
         and type hash = S.hash
         and module Info = S.Info

    module type S = Portable
  end

  module type Store = Store
  (** [Store] specifies the signature for commit stores. *)

  (** [Store] creates a new commit store. *)
  module Store
      (I : Info.S)
      (N : Node.Store)
      (S : Content_addressable.S with type key = N.key)
      (H : Hash.S with type t = S.key)
      (V : S with type hash = S.key and type t = S.value and module Info := I) :
    Store
      with type 'a t = 'a N.t * 'a S.t
       and type key = S.key
       and type hash = S.key
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
       and type node_key = C.Node.key
       and type commit_key = C.key
       and type info = C.Info.t

  include Maker with module Info = Info.Default
end
