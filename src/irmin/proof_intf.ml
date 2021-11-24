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

  (** The type for proofs.

      [Blinded_node h] is a shallow pointer to a node having hash [h].

      [Blinded_contents (h, m)] is a shallow pointer to contents having hash [h]
      and metadata [m].

      [Node ls] is a "flat" node containing the list of files [ls]. The length
      of [ls] depends on the backend. For instance, it can be unbounded for most
      of the backends, while it is at most [Conf.stable_hash] entries for
      [irmin-pack].

      [Inode i] is an optimized representation of a node as a tree. Pointers in
      that trees would refer to blinded nodes, nodes or to other inodes. E.g.
      Blinded content is not expected to appear directly in an inodes. *)
  type ('hash, 'step, 'metadata) t =
    | Blinded_node of 'hash
    | Blinded_contents of 'hash * 'metadata
    | Node of ('step * ('hash, 'step, 'metadata) t) list
    | Inode of ('hash, 'step, 'metadata) t inode
  [@@deriving irmin]
end

module type Proof = sig
  include S
  (** @inline *)

  val bad_proof_exn : string -> 'a
end
