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

      This list can be sparse so every proof is indexed by their position
      between [0 ... (Conf.entries-1)]. For binary trees, this boolean index is
      a step of the left-right sequence / decision proof corresponding to the
      path in that binary tree.

      Paths of singleton inodes are compacted into a single inode addressed by
      that path (hence the [int list] indexing).

      {e For [irmin-pack]}: [proofs] have a length of at most [Conf.entries]
      entries. *)

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
      Blinded content is not expected to appear directly in an inodes.

      [Blinded_contents (h, m)] is a shallow pointer to contents having hash [h]
      and metadata [m].

      [Contents c] is the contents [c]. *)
  type tree =
    | Blinded_node of hash
    | Node of (step * tree) list
    | Inode of tree inode
    | Blinded_contents of hash * metadata
    | Contents of contents * metadata
  [@@deriving irmin]

  type t [@@deriving irmin]
  (** The type for proofs. *)

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]
  (** The type for kinded hashes. *)

  val v : before:kinded_hash -> after:kinded_hash -> tree -> t
  (** [v ~before ~after p] proves that the state advanced from [before] to
      [after]. [p]'s hash is [before], and [p] contains the minimal information
      for the computation to reach [after]. *)

  val before : t -> kinded_hash
  (** [before t] it the state's hash at the beginning of the computation. *)

  val after : t -> kinded_hash
  (** [after t] is the state's hash at the end of the computation. *)

  val state : t -> tree
  (** [proof t] is a subset of the initial state needed to prove that the proven
      computation could run without performing any I/O. *)
end

module type Env = sig
  (** Environment that tracks side effects during the production/consumption of
      proofs. *)

  type hash
  type contents
  type node
  type mode = Produce | Consume [@@deriving irmin]
  type t [@@deriving irmin]

  val is_empty : t -> bool
  val merge : t -> t -> unit
  val empty : unit -> t
  val copy : into:t -> t -> unit
  val mode : t -> mode option

  (** {2 Construct and destruct envs}

      Since env is implemented as a [ref], all its occurences are collected on
      clear. *)

  val track_reads_as_sets : mode -> (t -> 'a) -> 'a
  val track_reads_as_sets_lwt : mode -> (t -> 'a Lwt.t) -> 'a Lwt.t

  (** {2 Register a backend content to env} *)

  val add_contents : t -> hash -> contents -> unit
  val add_contents_opt : t -> hash -> contents option -> unit

  (** {2 Register a backend node to env}

      Also setup its hook in case it is internally represented as a recusive
      objects. *)

  val add_node : t -> hash -> node -> node
  val add_node_opt : t -> hash -> node option -> node option

  (** {2 Fetch from env} *)

  val find_contents_opt : t -> hash option -> contents option
  val find_node_opt : t -> hash option -> node option
end

module type Proof = sig
  module type S = S
  module type Env = Env

  exception Bad_proof of { context : string }

  val bad_proof_exn : string -> 'a

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

  module Env (H : Hash.S) (C : Contents.S) (N : Node.S with type hash = H.t) :
    Env with type hash := H.t and type contents := C.t and type node := N.t
end
