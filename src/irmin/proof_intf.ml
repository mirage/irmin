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

module type Def = sig
  type 'a inode = { length : int; proofs : (int * 'a) list } [@@deriving irmin]
  (** The type for (internal) inode proofs. These proofs encode large
      directories into a more efficient tree-like structure.

      Invariant are dependent on the backend.

      [len] is the total number of entries in the chidren of the inode. E.g. the
      size of the "flattened" version of that inode. This is used by some
      backend (like [irmin-pack]) to efficiently implements paginated lists.

      {e For [irmin-pack]}: [proofs] have a length of at most [Conf.entries]
      entries. This list can be sparsed so every proof is indexed by their
      position between [0 ... (Conf.entries-1)].*)

  (** The type for tree proofs.

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
  type ('contents, 'hash, 'step, 'metadata) tree =
    | Blinded_node of 'hash
    | Node of ('step * ('contents, 'hash, 'step, 'metadata) tree) list
    | Inode of ('contents, 'hash, 'step, 'metadata) tree inode
    | Blinded_contents of 'hash * 'metadata
    | Contents of 'contents * 'metadata
  [@@deriving irmin]
end

module type S = sig
  type contents
  type hash
  type step
  type metadata
  type tree_proof [@@deriving irmin]

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]

  type t [@@deriving irmin]
  (** The type for proofs. *)

  val v : before:kinded_hash -> after:kinded_hash -> tree_proof -> t
  (** [v ~before ~after p] proves that the state advanced from [before] to
      [after]. [p]'s hash is [before], and [p] contains the minimal information
      for the computation to reach [after]. *)

  val before : t -> kinded_hash
  (** [before t] it the state's hash at the beginning of the computation. *)

  val after : t -> kinded_hash
  (** [after t] is the state's hash at the end of the computation. *)

  val proof : t -> tree_proof
  (** [proof t] is the tree proof needed to prove that the proven computation
      could run without performing without I/O.

      Note: proofs do not provide any guarantee with the ordering of
      computations. For instance, if two effects commute, they won't be
      distinguishable by this kind of proofs. *)
end

module type Proof = sig
  include Def
  (** @inline *)

  module type S = sig
    include S
    (** @inline *)
  end

  val bad_proof_exn : string -> 'a

  exception Bad_proof of { context : string }

  module Make
      (C : Type.S)
      (H : Hash.S) (P : sig
        type step [@@deriving irmin]
      end)
      (M : Type.S) :
    S
      with type contents := C.t
       and type hash := H.t
       and type step := P.step
       and type metadata := M.t
       and type tree_proof = (C.t, H.t, P.step, M.t) tree
end
