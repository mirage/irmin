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

module type S = sig
  (** Proofs are compact representations of trees which can be shared between
      peers.

      This is expected to be used as follows:

      - A first peer runs a function [f] over a tree [t]. While performing this
        computation, it records: the hash of [t] (called [before] below), the
        hash of [f t] (called [after] below) and a subset of [t] which is needed
        to replay [f] without any access to the first peer's storage. Once done,
        all these informations are packed into a proof of type [t] that is sent
        to the second peer.

      - The second peer generates an initial tree [t'] from [p] and computes
        [f t']. Once done, it compares [t']'s hash and [f t']'s hash to [before]
        and [after]. If they match, they know that the result state [f t'] is a
        valid context state, without having to have access to the full storage
        of the first peer. *)

  type contents
  type hash
  type step
  type metadata

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]

  type 'a inode = { length : int; proofs : (int * 'a) list } [@@deriving irmin]
  (** The type for (internal) inode proofs.

      These proofs encode large directories into a tree-like structure.

      Invariants are dependent on the backend.

      [length] is the total number of entries in the children of the inode. It's
      the size of the "flattened" version of that inode. [length] can be used to
      prove the correctness of operations such as [Tree.length] and
      [Tree.list ~offset ~length] in an efficient way.

      [proofs] contains the children proofs. It is a sparse list of ['a] values.
      These values are associated to their index in the list, and the list is
      kept sorted in increasing order of indices. ['a] can be a concrete proof
      or a hash of that proof.

      {e For [irmin-pack]}: [proofs] have a length of at most [Conf.entries]
      entries. For binary trees, this boolean index is a step of the left-right
      sequence / decision proof corresponding to the path in that binary tree. *)

  type 'a inode_extender = { length : int; segments : int list; proof : 'a }
  [@@deriving irmin]
  (** The type for inode extenders.

      An extender is a compact representation of a sequence of [inode] which
      contain only one child. As for inodes, the ['a] parameter can be a
      concrete proof or a hash of that proof.

      If an inode proof contains singleton children [i_0, ..., i_n] such as:
      [{length=l; proofs = \[ (i_0, {proofs = ... { proofs = \[ (i_n, p) \] }})\]}],
      then it is compressed into the inode extender
      [{length=l; segment = \[i_0;..;i_n\]; proof=p}] sharing the same length
      [l] and final proof [p]. *)

  (** The type for compressed and partial Merkle tree proofs.

      Tree proofs do not provide any guarantee with the ordering of
      computations. For instance, if two effects commute, they won't be
      distinguishable by this kind of proof.

      [Value v] proves that a value [v] exists in the store.

      [Blinded_value h] proves a value with hash [h] exists in the store.

      [Node ls] proves that a a "flat" node containing the list of files [ls]
      exists in the store. {e For [irmin-pack]}: the length of [ls] is at most
      [Conf.stable_hash];

      [Blinded_node h] proves that a node with hash [h] exists in the store.

      [Inode i] proves that an inode [i] exists in the store.

      [Extender e] proves that an inode extender [e] exist in the store. *)
  type tree =
    | Contents of contents * metadata
    | Blinded_contents of hash * metadata
    | Node of (step * tree) list
    | Blinded_node of hash
    | Inode of inode_tree inode
    | Extender of inode_tree inode_extender
  [@@deriving irmin]

  (** The type for inode trees. It is a subset of [tree], limited to nodes.

      [Blinded_inode h] proves that an inode with hash [h] exists in the store.

      [Inode_values ls] is simliar to trees' [Node].

      [Inode_tree i] is similar to tree's [Inode].

      [Inode_extender e] is similar to trees' [Extender]. *)
  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (step * tree) list
    | Inode_tree of inode_tree inode
    | Inode_extender of inode_tree inode_extender
  [@@deriving irmin]

  (** Stream proofs represent an explicit traversal of a Merle tree proof. Every
      element (a node, a value, or a shallow pointer) met is first "compressed"
      by shallowing its children and then recorded in the proof.

      As stream proofs directly encode the recursive construction of the Merkle
      root hash is slightly simpler to implement: the verifier simply needs to
      hash the compressed elements lazily, without any memory or choice.

      Moreover, the minimality of stream proofs is trivial to check. Once the
      computation has consumed the compressed elements required, it is
      sufficient to check that no more compressed elements remain in the proof.

      However, as the compressed elements contain all the hashes of their
      shallow children, the size of stream proofs is larger (at least double in
      size in practice) than tree proofs, which only contains the hash for
      intermediate shallow pointers. *)

  (** The type for elements of stream proofs.

      [Value v] is a proof that the next element read in the store is the value
      [v].

      [Node n] is a proof that the next element read in the store is the node
      [n].

      [Inode i] is a proof that the next element read in the store is the inode
      [i].

      [Inode_extender e] is a proof that the next element read in the store is
      the node extender [e]. *)
  type elt =
    | Contents of contents
    | Node of (step * kinded_hash) list
    | Inode of hash inode
    | Inode_extender of hash inode_extender
  [@@deriving irmin]

  type stream = elt Seq.t [@@deriving irmin]
  (** The type for stream proofs.

      The sequance [e_1 ... e_n] proves that the [e_1], ..., [e_n] are read in
      the store in sequence. *)

  type 'a t [@@deriving irmin]
  (** The type for proofs of kind ['a] (i.e. [stream] or [proof]).

      A proof [p] proves that the state advanced from [before p] to [after p].
      [state p]'s hash is [before p], and [state p] contains the minimal
      information for the computation to reach [after p]. *)

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

    {1 The Merkle Proof Construction Algorithm}

    This description stands for [Set] proofs and assumes that the large nodes
    are represented by the backend as a tree structure (i.e. inodes).

    There are 4 distinct phases when working with Irmin's merkle proofs:
    [Produce | Serialise | Deserialise | Consume].

    {2 [Produce]}

    This phase runs the [f] function provided by the Irmin user. It builds an
    [after] tree from a [before] tree that has been setup with an [Env] that
    records every backend reads into two hash tables.

    During the next phase (i.e. [Serialise]) the cleared [before] tree will be
    traversed from root to stems only following the paths that are referenced in
    [Env].

    In practice [Env] doesn't exactly record the reads, it keeps track of all
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

    In this phase, the [Env] contains everything necessary for the computation
    of a Merkle proof from a cleared [before]. The [Env] now affects
    [Node.cached_value] and [Contents.cached_value] allowing for the discovery
    of the cached closure.

    {2 [Deserialise]}

    In this phase the [Env] is filled by recursively destructing the proof and
    filling it before the [Consume] phase.

    {2 [Consume]}

    In this last phase the [Env] is again made accessible through
    [Node.cached_pvalue] and [Contents.cached_pvalue], making it possible for
    the user to reference by [hash] everything that was contained in the proof.

    {1 Nodes and Portable Nodes}

    While the [Produce] phase must be connected to the backend to records reads,
    the [Consume] phase must be disconnected from the backend.

    [Produce] manipulates backend nodes of type [Backend.Node.Val.t] (the ones
    enriched with backend keys)

    [Consume] is restricted to manipulating nodes of type
    [Backend.Node_portable.t].

    {1 Hashing of Backend Nodes with Streamed Proofs}

    Hashing a backend node or calling [head] on it may trigger IOs in order to
    load inner inodes (this is the case in irmin-pack).

    In various places, [Env] requires calling [head] or [hash_exn] on nodes.

    [Env] must be very careful that these two facts do not lead to chaos during
    the recording of IOs' order.

    Two tricks are in place to prevent problems:

    - The [Node.of_proof] functions return nodes that don't require IOs to
      produce their hash (i.e. they use caching if necessary).
    - The [Node.head] function that is called on a node during
      [dehydrate_stream_node] is also called just after [rehydrate_stream_node]. *)
module type Env = sig
  type kind = Set | Stream
  type mode = Produce | Serialise | Deserialise | Consume
  type t [@@deriving irmin]
  type hash
  type node
  type pnode
  type contents
  type stream

  val is_empty : t -> bool
  val empty : unit -> t
  val copy : into:t -> t -> unit

  (** {2 Modes} *)

  val set_mode : t -> kind -> mode -> unit

  val with_set_produce :
    (t -> start_serialise:(unit -> unit) -> 'a Lwt.t) -> 'a Lwt.t

  val with_set_consume :
    (t -> stop_deserialise:(unit -> unit) -> 'a Lwt.t) -> 'a Lwt.t

  val with_stream_produce :
    (t -> to_stream:(unit -> stream) -> 'a Lwt.t) -> 'a Lwt.t

  val with_stream_consume :
    stream -> (t -> is_empty:(unit -> bool) -> 'a Lwt.t) -> 'a Lwt.t

  (** {2 Interactions With [Tree]} *)

  val add_contents_from_store : t -> hash -> contents -> unit

  val add_node_from_store : t -> hash -> node -> node
  (** [add_node_from_store] returns a [node] and not [unit] because [Env] may
      take the opportunity to wrap the input node in [Node.Val.with_handler]. *)

  val add_contents_from_proof : t -> hash -> contents -> unit
  val add_pnode_from_proof : t -> hash -> pnode -> unit
  val find_contents : t -> hash -> contents option
  val find_node : t -> hash -> node option
  val find_pnode : t -> hash -> pnode option
end

module type Proof = sig
  module type S = S
  module type Env = Env

  exception Bad_proof of { context : string }

  type bad_stream_exn =
    | Stream_too_long of { context : string; reason : string }
    | Stream_too_short of { context : string; reason : string }
    | Proof_mismatch of { context : string; reason : string }

  exception Bad_stream of bad_stream_exn

  val bad_proof_exn : string -> 'a
  val bad_stream_exn : string -> string -> 'a
  val bad_stream_too_long : string -> string -> 'a
  val bad_stream_too_short : string -> string -> 'a

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
      (B : Backend.S)
      (P : S
             with type contents := B.Contents.Val.t
              and type hash := B.Hash.t
              and type step := B.Node.Val.step
              and type metadata := B.Node.Val.metadata) :
    Env
      with type hash := B.Hash.t
       and type contents := B.Contents.Val.t
       and type node := B.Node.Val.t
       and type pnode := B.Node_portable.t
       and type stream := P.stream
end
