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

open! Import
include Proof_intf

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

  type 'a inode = { length : int; proofs : (int list * 'a) list }
  [@@deriving irmin]

  type kinded_hash = [ `Node of hash | `Contents of hash * metadata ]
  [@@deriving irmin]

  type tree =
    | Blinded_node of hash
    | Node of (step * tree) list
    | Inode of tree inode
    | Blinded_contents of hash * metadata
    | Contents of contents * metadata
  [@@deriving irmin]

  type t = { before : kinded_hash; after : kinded_hash; state : tree }
  [@@deriving irmin]

  let before t = t.before
  let after t = t.after
  let state t = t.state
  let v ~before ~after state = { after; before; state }
end

exception Bad_proof of { context : string }

let bad_proof_exn context = raise (Bad_proof { context })

module Env (H : Hash.S) (C : Contents.S) (N : Node.S with type hash = H.t) =
struct
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

  (* Keep track of read effects happening during a computation using
     sets. This does not keep track of the ordering of the reads. *)
  type read_set = { nodes : N.t Hashes.t; contents : C.t Hashes.t }
  [@@deriving irmin]

  type mode = Produce | Serialise | Deserialise | Consume [@@deriving irmin]
  type set_effects = { mode : mode; set : read_set } [@@deriving irmin]
  type v = Empty | Set of set_effects [@@deriving irmin]
  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty
  let empty_set () = { contents = Hashes.create 13; nodes = Hashes.create 13 }
  let copy ~into t = into := !t
  let mode t = match !t with Empty -> None | Set { mode; _ } -> Some mode

  let to_mode t mode =
    match (!t, mode) with
    | Empty, Produce | Empty, Deserialise ->
        let set = empty_set () in
        t := Set { mode; set }
    | Set { mode = Produce; set }, Serialise
    | Set { mode = Deserialise; set }, Consume ->
        t := Set { mode; set }
    | _ -> assert false

  let with_mode t mode f =
    let before = !t in
    to_mode t mode;
    let+ res = f () in
    t := before;
    res

  let find_contents t h =
    match !t with
    | Empty -> None
    | Set { mode = Produce; set } ->
        (* Sharing of contents is not strictly needed during this phase. It
           could be disabled. *)
        Hashes.find_opt set.contents h
    | Set { mode = Serialise; set } ->
        (* This is needed in order to differenciate between blinded contents
           from others. *)
        Hashes.find_opt set.contents h
    | Set { mode = Deserialise; _ } ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set { mode = Consume; set } ->
        (* This is needed in order to read non-blinded contents. *)
        Hashes.find_opt set.contents h

  let add_contents_from_store t h v =
    match !t with
    | Empty -> ()
    | Set { mode = Produce; set } ->
        (* Registering in [set] for traversal during [Serialise]. *)
        assert (not (Hashes.mem set.contents h));
        Hashes.add set.contents h v
    | Set { mode = Serialise; _ } ->
        (* There shouldn't be new contents during this phase *)
        assert false
    | Set { mode = Deserialise; _ } ->
        (* This phase has no repo pointer *)
        assert false
    | Set { mode = Consume; _ } ->
        (* This phase has no repo pointer *)
        assert false

  let find_node t h =
    match !t with
    | Empty -> None
    | Set { mode = Produce; set } ->
        (* This is needed in order to achieve sharing on inode's pointers. In
           other words, each node present in the [before] tree should have a
           single [P.Node.Val.t] representative that will witness all the lazy
           inode loadings. *)
        Hashes.find_opt set.nodes h
    | Set { mode = Serialise; set } ->
        (* This is needed in order to follow loaded paths in the [before]
           tree. *)
        Hashes.find_opt set.nodes h
    | Set { mode = Deserialise; _ } ->
        (* This phase only fills the env, it should search for anything *)
        assert false
    | Set { mode = Consume; set } ->
        (* This is needed in order to read non-blinded nodes. *)
        Hashes.find_opt set.nodes h

  let add_contents_from_proof t h v =
    match !t with
    | Set { mode = Deserialise; set } ->
        (* Using [replace] because there could be several instances of this
           contents in the proof, we will not share as this is not strictly
           needed. *)
        Hashes.replace set.contents h v
    | _ -> assert false

  let add_node_from_store t h v =
    match !t with
    | Empty -> ()
    | Set { mode = Produce; set } ->
        (* Registering in [set] for sharing during [Produce] and traversal
           during [Serialise]. *)
        assert (not (Hashes.mem set.nodes h));
        Hashes.add set.nodes h v
    | Set { mode = Serialise; _ } ->
        (* There shouldn't be new nodes during this phase *)
        assert false
    | Set { mode = Deserialise; _ } ->
        (* This phase has no repo pointer *)
        assert false
    | Set { mode = Consume; _ } ->
        (* This phase has no repo pointer *)
        assert false

  let add_node_from_proof t h v =
    match !t with
    | Set { mode = Deserialise; set } ->
        (* Using [replace] because there could be several instances of this
           node in the proof, we will not share as this is not strictly
           needed.
           All the occurences of this node in the proof are expected to have
           the same blinded/visible coverage (i.e. the same node proof). *)
        Hashes.replace set.nodes h v
    | _ -> assert false

  (* x' = y' <- x union y *)
  let merge (x : t) (y : t) =
    match (!x, !y) with
    | Empty, Empty -> ()
    | Empty, y -> x := y
    | x, Empty -> y := x
    | Set _, Set _ ->
        failwith "Merging two non-empty [Proof.Env.t] is forbidden"
end
