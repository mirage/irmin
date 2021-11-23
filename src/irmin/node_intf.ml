(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

open! Import
open S

module Proof = struct
  type ('hash, 'step, 'value) t =
    | Blinded of 'hash
    | Values of ('step * 'value) list
    | Inode of { length : int; proofs : (int * ('hash, 'step, 'value) t) list }

  (* TODO(craigfe): fix [ppx_irmin] for recursive types with type parameters. *)
  let t hash_t step_t value_t =
    let open Type in
    mu (fun t ->
        variant "proof" (fun blinded values inode -> function
          | Blinded x1 -> blinded x1
          | Values x1 -> values x1
          | Inode { length; proofs } -> inode (length, proofs))
        |~ case1 "Blinded" hash_t (fun x1 -> Blinded x1)
        |~ case1 "Values" [%typ: (step * value) list] (fun x1 -> Values x1)
        |~ case1 "Inode" [%typ: int * (int * t) list] (fun (length, proofs) ->
               Inode { length; proofs })
        |> sealv)
end

module type S = sig
  (** {1 Node values} *)

  type t [@@deriving irmin]
  (** The type for node values. *)

  type metadata [@@deriving irmin]
  (** The type for node metadata. *)

  type hash [@@deriving irmin]
  (** The type for keys. *)

  type step [@@deriving irmin]
  (** The type for steps between nodes. *)

  type value = [ `Node of hash | `Contents of hash * metadata ]
  [@@deriving irmin]
  (** The type for either (node) keys or (contents) keys combined with their
      metadata. *)

  val of_list : (step * value) list -> t
  (** [of_list l] is the node [n] such that [list n = l]. *)

  val list :
    ?offset:int -> ?length:int -> ?cache:bool -> t -> (step * value) list
  (** [list t] is the contents of [t]. [offset] and [length] are used to
      paginate results.

      {2 caching}

      [cache] regulates the caching behaviour regarding the node's internal data
      which may be lazily loaded from the backend, depending on the node
      implementation.

      [cache] defaults to [true] which may greatly reduce the IOs and the
      runtime but may also increase the memory consumption.

      [cache = false] doesn't replace a call to [clear], it only prevents the
      storing of new data, it doesn't discard the existing one. *)

  val of_seq : (step * value) Seq.t -> t
  (** [of_seq s] is the node [n] such that [seq n = s]. *)

  val seq :
    ?offset:int -> ?length:int -> ?cache:bool -> t -> (step * value) Seq.t
  (** [seq t] is the contents of [t]. [offset] and [length] are used to paginate
      results.

      See {!caching} for an explanation of the [cache] parameter *)

  val empty : t
  (** [empty] is the empty node. *)

  val is_empty : t -> bool
  (** [is_empty t] is true iff [t] is {!empty}. *)

  val length : t -> int
  (** [length t] is the number of entries in [t]. *)

  val clear : t -> unit
  (** Cleanup internal caches. *)

  val find : ?cache:bool -> t -> step -> value option
  (** [find t s] is the value associated with [s] in [t].

      A node can point to user-defined {{!Node.S.contents} contents}. The edge
      between the node and the contents is labeled by a {{!Node.S.step} step}.

      See {!caching} for an explanation of the [cache] parameter *)

  val add : t -> step -> value -> t
  (** [add t s v] is the node where [find t v] is [Some s] but is similar to [t]
      otherwise. *)

  val remove : t -> step -> t
  (** [remove t s] is the node where [find t s] is [None] but is similar to [t]
      otherwise. *)

  val default : metadata
  (** [default] is the default metadata value. *)

  (** {1 Proofs} *)

  type proof = (hash, step, value) Proof.t [@@deriving irmin]
  (** The type for proof trees. *)

  val to_proof : t -> proof
  val of_proof : proof -> t
end

module type Maker = functor
  (H : Hash.S)
  (P : sig
     type step [@@deriving irmin]
   end)
  (M : METADATA)
  -> S with type metadata = M.t and type hash = H.t and type step = P.step

module type STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  module Path : Path.S
  (** [Path] provides base functions on node paths. *)

  val merge : [> read_write ] t -> key option Merge.t
  (** [merge] is the 3-way merge function for nodes keys. *)

  (** [Key] provides base functions for node keys. *)
  module Key : Hash.TYPED with type t = key and type value = value

  module Metadata : METADATA
  (** [Metadata] provides base functions for node metadata. *)

  (** [Val] provides base functions for node values. *)
  module Val :
    S
      with type t = value
       and type hash = key
       and type metadata = Metadata.t
       and type step = Path.step

  module Contents : Contents.STORE with type key = Val.hash
  (** [Contents] is the underlying contents store. *)
end

module type GRAPH = sig
  (** {1 Node Graphs} *)

  type 'a t
  (** The type for store handles. *)

  type metadata
  (** The type for node metadata. *)

  type contents
  (** The type of user-defined contents. *)

  type node
  (** The type for node values. *)

  type step
  (** The type of steps. A step is used to pass from one node to another. *)

  type path
  (** The type of store paths. A path is composed of {{!step} steps}. *)

  type value = [ `Node of node | `Contents of contents * metadata ]
  (** The type for store values. *)

  val empty : [> write ] t -> node Lwt.t
  (** The empty node. *)

  val v : [> write ] t -> (step * value) list -> node Lwt.t
  (** [v t n] is a new node containing [n]. *)

  val list : [> read ] t -> node -> (step * value) list Lwt.t
  (** [list t n] is the contents of the node [n]. *)

  val find : [> read ] t -> node -> path -> value option Lwt.t
  (** [find t n p] is the contents of the path [p] starting form [n]. *)

  val add : [> read_write ] t -> node -> path -> value -> node Lwt.t
  (** [add t n p v] is the node [x] such that [find t x p] is [Some v] and it
      behaves the same [n] for other operations. *)

  val remove : [> read_write ] t -> node -> path -> node Lwt.t
  (** [remove t n path] is the node [x] such that [find t x] is [None] and it
      behhaves then same as [n] for other operations. *)

  val closure : [> read ] t -> min:node list -> max:node list -> node list Lwt.t
  (** [closure t min max] is the unordered list of nodes [n] reachable from a
      node of [max] along a path which: (i) either contains no [min] or (ii) it
      ends with a [min].

      {b Note:} Both [min] and [max] are subsets of [n]. *)

  val iter :
    [> read ] t ->
    min:node list ->
    max:node list ->
    ?node:(node -> unit Lwt.t) ->
    ?contents:(contents -> unit Lwt.t) ->
    ?edge:(node -> node -> unit Lwt.t) ->
    ?skip_node:(node -> bool Lwt.t) ->
    ?skip_contents:(contents -> bool Lwt.t) ->
    ?rev:bool ->
    unit ->
    unit Lwt.t
  (** [iter t min max node edge skip rev ()] iterates in topological order over
      the closure of [t].

      It applies the following functions while traversing the graph: [node] on
      the nodes; [edge n predecessor_of_n] on the directed edges; [skip_node n]
      to not include a node [n], its predecessors and the outgoing edges of [n]
      and [skip_contents c] to not include content [c].

      If [rev] is true (the default) then the graph is traversed in the reverse
      order: [node n] is applied only after it was applied on all its
      predecessors; [edge n p] is applied after [node n]. Note that [edge n p]
      is applied even if [p] is skipped. *)

  (** {1 Value Types} *)

  val metadata_t : metadata Type.t
  (** [metadat_t] is the value type for {!metadata}. *)

  val contents_t : contents Type.t
  (** [contents_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val path_t : path Type.t
  (** [path_t] is the value type for {!path}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type Node = sig
  module Proof = Proof

  module type S = S
  module type Maker = Maker

  module Make : Maker
  (** [Make] provides a simple node implementation, parameterized by the
      contents and notes keys [K], paths [P] and metadata [M]. *)

  (** v1 serialisation *)
  module V1 (N : S with type step = string) : sig
    include
      S
        with type hash = N.hash
         and type step = N.step
         and type metadata = N.metadata

    val import : N.t -> t
    val export : t -> N.t
  end

  module type STORE = STORE
  (** [STORE] specifies the signature for node stores. *)

  (** [Store] creates node stores. *)
  module Store
      (C : Contents.STORE)
      (P : Path.S)
      (M : METADATA) (N : sig
        include CONTENT_ADDRESSABLE_STORE with type key = C.key
        module Key : Hash.S with type t = key

        module Val :
          S
            with type t = value
             and type hash = key
             and type metadata = M.t
             and type step = P.step
      end) :
    STORE
      with type 'a t = 'a C.t * 'a N.t
       and type key = N.key
       and type value = N.value
       and module Path = P
       and module Metadata = M
       and type Key.t = N.key
       and module Val = N.Val

  module type GRAPH = GRAPH
  (** [Graph] specifies the signature for node graphs. A node graph is a
      deterministic DAG, labeled by steps. *)

  module Graph (N : STORE) :
    GRAPH
      with type 'a t = 'a N.t
       and type contents = N.Contents.key
       and type metadata = N.Metadata.t
       and type node = N.key
       and type step = N.Path.step
       and type path = N.Path.t

  module No_metadata : METADATA with type t = unit
end
