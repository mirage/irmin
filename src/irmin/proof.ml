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
include Proof_intf

module Make
    (C : Type.S)
    (H : Type.S)
    (S : sig
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

  type 'a inode_extender = { length : int; segments : int list; proof : 'a }
  [@@deriving irmin]

  type tree =
    | Contents of contents * metadata
    | Blinded_contents of hash * metadata
    | Node of (step * tree) list
    | Blinded_node of hash
    | Inode of inode_tree inode
    | Extender of inode_tree inode_extender
  [@@deriving irmin]

  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (step * tree) list
    | Inode_tree of inode_tree inode
    | Inode_extender of inode_tree inode_extender
  [@@deriving irmin]

  type elt =
    | Contents of contents
    | Node of (step * kinded_hash) list
    | Inode of hash inode
    | Inode_extender of hash inode_extender
  [@@deriving irmin]

  type stream = elt Seq.t [@@deriving irmin]

  type t = { before : kinded_hash; after : kinded_hash; state : tree }
  [@@deriving irmin]

  let before t = t.before
  let after t = t.after
  let state t = t.state
  let v ~before ~after state = { after; before; state }
end

exception Bad_proof of { context : string }

let bad_proof_exn context = raise (Bad_proof { context })

module Env
    (B : Backend.S)
    (P : S
           with type contents := B.Contents.Val.t
            and type hash := B.Hash.t
            and type step := B.Node.Val.step
            and type metadata := B.Node.Val.metadata) =
struct
  module H = B.Hash

  module Hashes = struct
    include Hashtbl.Make (struct
      type t = H.t

      let hash = H.short_hash
      let equal = Type.(unstage (equal H.t))
    end)

    let of_list l = of_seq (List.to_seq l)
    let to_list t = List.of_seq (to_seq t)
    let t elt_t = Type.map [%typ: (H.t * elt) list] of_list to_list
  end

  type mode = Produce | Serialise | Deserialise | Consume [@@deriving irmin]

  module Set = struct
    type produce = {
      nodes : B.Node.Val.t Hashes.t;
      contents : B.Contents.Val.t Hashes.t;
    }
    [@@deriving irmin]

    type deserialise = {
      nodes : B.Node_portable.t Hashes.t;
      contents : B.Contents.Val.t Hashes.t;
    }
    [@@deriving irmin]

    type t =
      | Produce of produce
      | Serialise of produce
      | Deserialise of deserialise
      | Consume of deserialise
    [@@deriving irmin]

    let producer () =
      Produce { contents = Hashes.create 13; nodes = Hashes.create 13 }

    let deserialiser () =
      Deserialise { contents = Hashes.create 13; nodes = Hashes.create 13 }
  end

  type v = Empty | Set of Set.t [@@deriving irmin]
  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty
  let copy ~into t = into := !t

  type hash = H.t [@@deriving irmin ~equal ~pp]

  let set_mode t mode =
    match (!t, mode) with
    | Empty, Produce -> t := Set Set.(producer ())
    | Empty, Deserialise -> t := Set Set.(deserialiser ())
    | Set (Produce set), Serialise -> t := Set Set.(Serialise set)
    | Set (Deserialise set), Consume -> t := Set Set.(Consume set)
    | _ -> assert false

  let with_consume f =
    let t = ref Empty in
    set_mode t Deserialise;
    let stop_deserialise () = set_mode t Consume in
    let res = f t ~stop_deserialise in
    t := Empty;
    res

  let with_produce f =
    let t = ref Empty in
    set_mode t Produce;
    let start_serialise () = set_mode t Serialise in
    let res = f t ~start_serialise in
    t := Empty;
    res

  module Contents_hash = Hash.Typed (H) (B.Contents.Val)

  let find_contents t h =
    match !t with
    | Empty -> None
    | Set (Produce set) ->
        (* Sharing of contents is not strictly needed during this phase. It
           could be disabled. *)
        Hashes.find_opt set.contents h
    | Set (Serialise set) ->
        (* This is needed in order to differenciate between blinded contents
           from others. *)
        Hashes.find_opt set.contents h
    | Set (Deserialise _) ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set (Consume set) ->
        (* Use the Env to feed the values during consume *)
        Hashes.find_opt set.contents h

  let add_contents_from_store t h v =
    match !t with
    | Empty -> ()
    | Set (Produce set) ->
        (* Registering in [set] for traversal during [Serialise]. *)
        assert (not (Hashes.mem set.contents h));
        Hashes.add set.contents h v
    | Set (Serialise _) ->
        (* There shouldn't be new contents during this phase *)
        assert false
    | Set (Deserialise _) ->
        (* This phase has no repo pointer *)
        assert false
    | Set (Consume _) ->
        (* This phase has no repo pointer *)
        assert false

  let add_contents_from_proof t h v =
    match !t with
    | Set (Deserialise set) ->
        (* Using [replace] because there could be several instances of this
           contents in the proof, we will not share as this is not strictly
           needed. *)
        Hashes.replace set.contents h v
    | Empty ->
        (* Happens during [hash_of_proof_state] *)
        ()
    | _ -> assert false

  let find_node t h =
    match !t with
    | Empty -> None
    | Set (Produce set) ->
        (* This is needed in order to achieve sharing on inode's pointers. In
           other words, each node present in the [before] tree should have a
           single [P.Node.Val.t] representative that will witness all the lazy
           inode loadings. *)
        Hashes.find_opt set.nodes h
    | Set (Serialise set) ->
        (* This is needed in order to follow loaded paths in the [before]
           tree. *)
        Hashes.find_opt set.nodes h
    | Set (Deserialise _) ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set (Consume _) ->
        (* This phase looks for portable nodes *)
        None

  let find_pnode t h =
    match !t with
    | Set (Consume set) ->
        (* [set] has been filled during deserialise. Using it to provide values
            during consume. *)
        Hashes.find_opt set.nodes h
    | _ -> None

  let add_node_from_store t h v =
    match !t with
    | Empty -> v
    | Set (Produce set) ->
        (* Registering in [set] for sharing during [Produce] and traversal
           during [Serialise]. This assertion is guarenteed because
           [add_node_from_store] is guarded by a call to [find_node] in tree. *)
        assert (not (Hashes.mem set.nodes h));
        Hashes.add set.nodes h v;
        v
    | Set (Serialise _) ->
        (* There shouldn't be new nodes during this phase *)
        assert false
    | Set (Deserialise _) ->
        (* This phase has no repo pointer *)
        assert false
    | Set (Consume _) ->
        (* This phase has no repo pointer *)
        assert false

  let add_pnode_from_proof t h v =
    match !t with
    | Set (Deserialise set) ->
        (* Using [replace] because there could be several instances of this
           node in the proof, we will not share as this is not strictly
           needed.
           All the occurences of this node in the proof are expected to have
           the same blinded/visible coverage (i.e. the same node proof). *)
        Hashes.replace set.nodes h v
    | Empty ->
        (* Happens during [hash_of_proof_state] *)
        ()
    | _ -> assert false
end
