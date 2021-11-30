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

include Proof_intf

type 'a inode = { length : int; proofs : (int * 'a) list } [@@deriving irmin]

type ('contents, 'hash, 'step, 'metadata) tree =
  | Blinded_node of 'hash
  | Node of ('step * ('contents, 'hash, 'step, 'metadata) tree) list
  | Inode of ('contents, 'hash, 'step, 'metadata) tree inode
  | Blinded_contents of 'hash * 'metadata
  | Contents of 'contents * 'metadata

(* TODO(craigfe): fix [ppx_irmin] for inline parameters. *)
let tree_t contents_t hash_t step_t metadata_t =
  let open Type in
  mu (fun t ->
      variant "proof" (fun blinded_node node inode blinded_contents contents ->
        function
        | Blinded_node x1 -> blinded_node x1
        | Node x1 -> node x1
        | Inode i -> inode i
        | Blinded_contents (x1, x2) -> blinded_contents (x1, x2)
        | Contents (c, m) -> contents (c, m))
      |~ case1 "Blinded_node" hash_t (fun x1 -> Blinded_node x1)
      |~ case1 "Node" [%typ: (step * t) list] (fun x1 -> Node x1)
      |~ case1 "Inode" [%typ: t inode] (fun i -> Inode i)
      |~ case1 "Blinded_contents" [%typ: hash * metadata] (fun (x1, x2) ->
             Blinded_contents (x1, x2))
      |~ case1 "Contents" [%typ: contents * metadata] (fun (x1, x2) ->
             Contents (x1, x2))
      |> Type.sealv)

module Make
    (C : Type.S)
    (H : Type.S) (S : sig
      type step [@@deriving irmin]
    end)
    (M : Type.S) =
struct
  type contents = C.t [@@deriving irmin]
  type hash = H.t [@@deriving irmin]
  type step = S.step [@@deriving irmin]
  type metadata = M.t [@@deriving irmin]

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]

  type 'a inode = { length : int; proofs : (int * 'a) list } [@@deriving irmin]
  type tree_proof = (contents, hash, step, metadata) tree [@@deriving irmin]

  type t = { before : kinded_hash; after : kinded_hash; proof : tree_proof }
  [@@deriving irmin]

  let before t = t.before
  let after t = t.after
  let proof t = t.proof
  let v ~before ~after proof = { after; before; proof }
end

exception Bad_proof of { context : string }

let bad_proof_exn context = raise (Bad_proof { context })
