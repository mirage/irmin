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

module type S = sig
  type contents
  type hash
  type step
  type metadata

  (** Proofs are compact representations of Irmin [trees] which can be shared
      between an Irmin node and a client.

      The protocol is the following:

      - The Irmin node runs a function [f] over a tree [t]. While performing
        this computation, the node records: the hash of [t] (called [before]
        below), the hash of [f t] (called [after] below) and a subset of [t]
        which is needed to replay [f] without any access to the node's storage.
        Once done, the node packs this into a proof [p] and sends this to the
        client.

      - The client generates an initial tree [t'] from [p] and computes [f t'].
        Once done, it compares [t']'s hash and [f t']'s hash to [before] and
        [after]. If they match, they know that the result state [f t'] is a
        valid state of Irmin, without having to have access to the full node's
        storage. *)

  type 'a inode = { length : int; proofs : (int list * 'a) list }
  [@@deriving irmin]
  (** The type for (internal) inode proofs.

      These proofs encode large directories into a more efficient tree-like
      structure.

      Invariant are dependent on the backend.

      [length] is the total number of entries in the chidren of the inode. E.g.
      the size of the "flattened" version of that inode. This is used by some
      backend (like [irmin-pack]) to efficiently implements paginated lists.

      Paths of singleton inodes are compacted into a single inode addressed by
      that path (hence the [int list] indexing).

      {e For [irmin-pack]}: [proofs] have a length of at most [Conf.entries]
      entries. This list can be sparse so every proof is indexed by their
      position between [0 ... (Conf.entries-1)]. For binary trees, this boolean
      index is a step of the left-right sequence / decision proof corresponding
      to the path in that binary tree. *)

  (** The type for compressed and partial Merkle tree proofs.

      Tree proofs do not provide any guarantee with the ordering of
      computations. For instance, if two effects commute, they won't be
      distinguishable by this kind of proofs.

      [Blinded_node h] is a shallow pointer to a node having hash [h].

      [Node ls] is a "flat" node containing the list of files [ls]. The length
      of [ls] depends on the backend. For instance, it can be unbounded for most
      of the backends, while it is at most [Conf.stable_hash] entries for
      [irmin-pack].

      [Inode i] is an optimized representation of a node as a tree. Pointers in
      that trees would refer to blinded nodes, nodes or to other inodes. E.g.
      Blinded content nor contents is not expected to appear directly in an
      inodes.

      [Blinded_contents (h, m)] is a shallow pointer to contents having hash [h]
      and metadata [m].

      [Contents c] is the contents [c]. *)
  type tree =
    | Blinded_node of hash
    | Node of (step * tree) list
    | Inode of inode_tree inode
    | Blinded_contents of hash * metadata
    | Contents of contents * metadata
  [@@deriving irmin]

  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (step * tree) list
    | Inode_tree of inode_tree inode
  [@@deriving irmin]

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]
  (** The type for kinded hashes. *)

  type elt =
    | Node of (step * kinded_hash) list
    | Inode of hash inode
    | Contents of contents
  [@@deriving irmin]

  type stream = elt Seq.t [@@deriving irmin]
  (** The type for stream proofs. Stream poofs provides stronger ordering
      guarantees as the read effects have to happen in the exact same order and
      they are easier to verify. *)

  type 'a t [@@deriving irmin]
  (** The type for proofs. *)

  val v : before:kinded_hash -> after:kinded_hash -> 'a -> 'a t
  (** [v ~before ~after p] proves that the state advanced from [before] to
      [after]. [p]'s hash is [before], and [p] contains the minimal information
      for the computation to reach [after]. *)

  val before : 'a t -> kinded_hash
  (** [before t] it the state's hash at the beginning of the computation. *)

  val after : 'a t -> kinded_hash
  (** [after t] is the state's hash at the end of the computation. *)

  val state : 'a t -> 'a
  (** [proof t] is a subset of the initial state needed to prove that the proven
      computation could run without performing any I/O. *)
end

(** Environment that tracks side effects during the production/consumption of
    proofs.

    {1 The merkle proof construction algorithm}

    This description assumes that the large nodes are represented by the backend
    as a tree structure (i.e. inodes). There are 4 distinct phases when working
    with Irmin's merkle proofs: [Produce | Serialise | Deserialise | Consume].

    {2 [Produce]}

    In this phase the Irmin user builds an [after] tree from a [before] tree
    that has been setup with an [Env] that records every backend reads into two
    hash tables.

    During the next phase (i.e. [Serialise]) the cleared [before] tree will be
    traversed from root to stems only following the paths that are referenced in
    [Env].

    In practice [Env] doesn't exactly records the reads, it keeps track of all
    the [hash -> backend node] and [hash -> backend contents] mappings that are
    directly output of the backend stores through [P.Node.find] and
    [P.Contents.find]. This is obviously enough to remember the contents, the
    nodes and the inodes tips, but the inner inodes are not directly referenced
    in the hash tables.

    The inner inodes are in fact referenced in their inode tip which is itself
    referenced in [Env]'s hash tables. Since an inode shares its lazy pointers
    with the inodes derived from it, even the inner inodes that are loaded from
    the derived tips will be available from the original inode tip.

    {2 [Serialise]}

    In this phase the [Env] contains everything necessary for the computation of
    a merkle proof from a cleared [before]. The [Env] now affects
    [Node.cached_value] and [Contents.cached_value] allowing for the discovery
    of the cached closure.

    {2 [Deserialise]}

    In this phase the [Env] is filled by recursively destructing the proof and
    filling it before the [Consume] phase.

    {2 [Consume]}

    In this last phase the [Env] is again made accessible through
    [Node.cached_value] and [Contents.cached_value], making it possible for the
    user to reference by [hash] everything that was contained in the proof. *)
module type Env = sig
  type kind = Set | Stream
  type mode = Produce | Serialise | Deserialise | Consume
  type v
  type t
  type hash
  type node
  type contents
  type stream

  val t : t Type.ty
  val is_empty : t -> bool

  (** {2 Construction of envs} *)

  val empty : unit -> t
  val copy : into:t -> t -> unit

  (** {2 Modes} *)

  val mode : t -> mode option
  val set_mode : t -> kind -> mode -> unit

  val with_set_produce :
    (t -> start_serialise:(unit -> unit) -> 'a Lwt.t) -> 'a Lwt.t

  val with_set_consume :
    (t -> stop_deserialise:(unit -> unit) -> 'a Lwt.t) -> 'a Lwt.t

  val with_stream_produce :
    (t -> to_stream:(unit -> stream) -> 'a Lwt.t) -> 'a Lwt.t

  val with_stream_consume :
    stream -> (t -> is_empty:(unit -> bool) -> 'a Lwt.t) -> 'a Lwt.t

  (** {2 In/out backend objects with [Tree]} *)

  val add_contents_from_store : t -> hash -> contents -> unit

  val add_node_from_store : t -> hash -> node -> node
  (** [add_node_from_store] returns a [node] and not [unit] because [Env] may
      take the opportunity to wrap the input node in [Node.Val.with_handler]. *)

  val add_contents_from_proof : t -> hash -> contents -> unit
  val add_node_from_proof : t -> hash -> node -> unit
  val find_contents : t -> hash -> contents option
  val find_node : t -> hash -> node option
end

module type Proof = sig
  module type S = S
  module type Env = Env

  exception Bad_proof of { context : string }
  exception Bad_stream of { context : string; reason : string }

  val bad_proof_exn : string -> 'a
  val bad_stream_exn : string -> string -> 'a

  module Make
      (C : Type.S)
      (H : Hash.S) (P : sig
        type step [@@deriving irmin]
      end)
      (M : Type.S) : sig
    include
      S
        with type contents := C.t
         and type hash := H.t
         and type step := P.step
         and type metadata := M.t
  end

  module Env
      (H : Hash.S)
      (C : Contents.S)
      (N : Node.S with type hash = H.t)
      (P : S
             with type contents := C.t
              and type hash := H.t
              and type step := N.step
              and type metadata := N.metadata) :
    Env
      with type hash := H.t
       and type contents := C.t
       and type node := N.t
       and type stream := P.stream
end
