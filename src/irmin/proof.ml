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
    | Inode of inode_tree inode
    | Blinded_contents of hash * metadata
    | Contents of contents * metadata
  [@@deriving irmin]

  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (step * tree) list
    | Inode_tree of inode_tree inode
  [@@deriving irmin]

  type elt =
    | Node of (step * kinded_hash) list
    | Inode of hash inode
    | Contents of contents
  [@@deriving irmin]

  type stream = elt Seq.t [@@deriving irmin]

  type 'a t = { before : kinded_hash; after : kinded_hash; state : 'a }
  [@@deriving irmin]

  let before t = t.before
  let after t = t.after
  let state t = t.state
  let v ~before ~after state = { after; before; state }
end

exception Bad_proof of { context : string }
exception Bad_stream of { context : string; reason : string }

let bad_proof_exn context = raise (Bad_proof { context })
let bad_stream_exn context reason = raise (Bad_stream { context; reason })

module Env
    (H : Hash.S)
    (C : Contents.S)
    (N : Node.S with type hash = H.t)
    (P : S
           with type contents := C.t
            and type hash := H.t
            and type step := N.step
            and type metadata := N.metadata) =
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

  type mode = Produce | Serialise | Deserialise | Consume [@@deriving irmin]
  type kind = Set | Stream

  module Set = struct
    (* Keep track of read effects happening during a computation using
       sets. This does not keep track of the ordering of the reads. *)
    type read_set = { nodes : N.t Hashes.t; contents : C.t Hashes.t }
    [@@deriving irmin]

    type t = { mode : mode; set : read_set } [@@deriving irmin]

    let empty () = { contents = Hashes.create 13; nodes = Hashes.create 13 }
  end

  module Stream = struct
    let ref_t v = Type.map v ref ( ! )

    type produce = { set : unit Hashes.t; rev_elts : P.elt list ref }
    [@@deriving irmin]

    type consume = {
      nodes : N.t Hashes.t;
      contents : C.t Hashes.t;
      stream : P.elt Seq.t ref;
    }
    [@@deriving irmin]

    type t = Produce of produce | Consume of consume [@@deriving irmin]
  end

  type v = Empty | Set of Set.t | Stream of Stream.t [@@deriving irmin]
  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty

  let to_stream t =
    match !t with
    | Stream (Produce { rev_elts; _ }) -> List.rev !rev_elts |> List.to_seq
    | _ -> assert false

  let is_empty_stream t =
    match !t with
    | Stream (Consume { stream; _ }) -> (
        (* Peek the sequence but do not advance the ref *)
        match !stream () with Seq.Nil -> true | _ -> false)
    | _ -> false

  let copy ~into t = into := !t

  let mode t =
    match !t with
    | Empty -> None
    | Set { mode; _ } -> Some mode
    | Stream (Produce _) -> Some Produce
    | Stream (Consume _) -> Some Consume

  let set_mode t (kind : kind) mode =
    match kind with
    | Set -> (
        match (!t, mode) with
        | Empty, Produce | Empty, Deserialise ->
            let set = Set.empty () in
            t := Set { mode; set }
        | Set { mode = Produce; set }, Serialise
        | Set { mode = Deserialise; set }, Consume ->
            t := Set { mode; set }
        | _ -> assert false)
    | Stream -> (
        match (!t, mode) with
        | Empty, Produce ->
            t := Stream (Produce { set = Hashes.create 13; rev_elts = ref [] })
        | _ -> assert false)

  let with_set_consume f =
    let t = ref Empty in
    set_mode t Set Deserialise;
    let stop_deserialise () = set_mode t Set Consume in
    let+ res = f t ~stop_deserialise in
    t := Empty;
    res

  let with_set_produce f =
    let t = ref Empty in
    set_mode t Set Produce;
    let start_serialise () = set_mode t Set Serialise in
    let+ res = f t ~start_serialise in
    t := Empty;
    res

  let with_stream_produce f =
    let t = ref Empty in
    set_mode t Stream Produce;
    let to_stream () = to_stream t in
    let+ res = f t ~to_stream in
    t := Empty;
    res

  let with_stream_consume stream f =
    let nodes = Hashes.create 13 in
    let contents = Hashes.create 13 in
    let stream = ref stream in
    let t = Stream (Consume { nodes; contents; stream }) |> ref in
    let is_empty () = is_empty_stream t in
    let+ res = f t ~is_empty in
    t := Empty;
    res

  type hash = H.t [@@deriving irmin ~equal ~pp]

  module Contents_hash = Hash.Typed (H) (C)
  module Node_hash = Hash.Typed (H) (N)

  let bad_stream_exn s = bad_stream_exn ("Proof.Env." ^ s)

  let check_contents_integrity v h =
    let h' = Contents_hash.hash v in
    if not (equal_hash h' h) then
      bad_stream_exn "check_contents_integrity"
        (Fmt.str "got %a expected %a" pp_hash h' pp_hash h)

  let check_node_integrity v h =
    let h' = Node_hash.hash v in
    if not (equal_hash h' h) then
      bad_stream_exn "check_node_integrity"
        (Fmt.str "got %a expected %a" pp_hash h' pp_hash h)

  let dehydrate_stream_node v =
    match N.to_elt v with
    | `Node l -> P.Node l
    | `Inode (length, proofs) ->
        let proofs = List.map (fun (index, k) -> ([ index ], k)) proofs in
        P.Inode { length; proofs }

  let rehydrate_stream_node ~depth (elt : P.elt) h =
    match elt with
    | Contents _ ->
        bad_stream_exn "rehydrate_stream_node"
          (Fmt.str
             "found contents at depth %d when looking for node with hash %a"
             depth pp_hash h)
    | Node l -> (
        match N.of_values ~depth l with
        | None ->
            bad_stream_exn "rehydrate_stream_node"
              (Fmt.str
                 "could not deserialise Node at depth %d when looking for hash \
                  %a"
                 depth pp_hash h)
        | Some v -> v)
    | Inode { length; proofs } ->
        let proofs =
          List.map
            (fun (index, k) ->
              let index =
                match index with
                | [ i ] -> i
                | _ ->
                    bad_stream_exn "rehydrate_stream_node"
                      (Fmt.str
                         "extender problem at depth %d when looking for hash %a"
                         depth pp_hash h)
              in
              (index, k))
            proofs
        in
        let v =
          match N.of_inode ~depth ~length proofs with
          | None ->
              bad_stream_exn "rehydrate_stream_node"
                (Fmt.str
                   "could not deserialise Inode at depth %d when looking for \
                    hash %a"
                   depth pp_hash h)
          | Some v -> v
        in
        v

  let rehydrate_stream_contents (elt : P.elt) h =
    match elt with
    | Node _ ->
        bad_stream_exn "find_contents"
          (Fmt.str "found Node when looking Contents with hash %a" pp_hash h)
    | Inode _ ->
        bad_stream_exn "find_contents"
          (Fmt.str "found Inode when looking Contents with hash %a" pp_hash h)
    | Contents v -> v

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
        (* Use the Env to feed the values during consume *)
        Hashes.find_opt set.contents h
    | Stream (Produce _) ->
        (* There is no need for sharing with stream proofs *)
        None
    | Stream (Consume { contents; stream; _ }) -> (
        (* Use the Env to feed the values during consume *)
        match Hashes.find_opt contents h with
        | Some v -> Some v
        | None -> (
            match !stream () with
            | Seq.Nil ->
                bad_stream_exn "find_contents"
                  (Fmt.str "empty stream when looking for hash %a" pp_hash h)
            | Cons (elt, rest) ->
                let v = rehydrate_stream_contents elt h in
                check_contents_integrity v h;
                stream := rest;
                Hashes.add contents h v;
                Some v))

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
    | Stream (Produce { set; rev_elts }) ->
        (* Registering when seen for the first time *)
        if not @@ Hashes.mem set h then (
          Hashes.add set h ();
          let elt : P.elt = Contents v in
          rev_elts := elt :: !rev_elts)
    | Stream (Consume _) ->
        (* This phase has no repo pointer *)
        assert false

  let add_contents_from_proof t h v =
    match !t with
    | Set { mode = Deserialise; set } ->
        (* Using [replace] because there could be several instances of this
           contents in the proof, we will not share as this is not strictly
           needed. *)
        Hashes.replace set.contents h v
    | _ -> assert false

  let find_recnode t _find ~expected_depth h =
    assert (expected_depth > 0);
    match !t with
    | Stream (Consume { nodes; stream; _ }) -> (
        (* Use the Env to feed the values during consume *)
        match Hashes.find_opt nodes h with
        | Some v -> Some v
        | None -> (
            match !stream () with
            | Seq.Nil ->
                bad_stream_exn "find_recnode"
                  (Fmt.str "empty stream when looking for hash %a" pp_hash h)
            | Cons (v, rest) ->
                let v = rehydrate_stream_node ~depth:expected_depth v h in
                (* There is no need to apply [with_handler] here because there
                   is no repo pointer in this inode. *)
                check_node_integrity v h;
                stream := rest;
                Hashes.add nodes h v;
                Some v))
    | _ -> assert false

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
        (* Use the Env to feed the values during consume *)
        Hashes.find_opt set.nodes h
    | Stream (Produce _) ->
        (* There is no need for sharing with stream proofs *)
        None
    | Stream (Consume { nodes; stream; _ }) -> (
        (* Use the Env to feed the values during consume *)
        match Hashes.find_opt nodes h with
        | Some v -> Some v
        | None -> (
            match !stream () with
            | Seq.Nil ->
                bad_stream_exn "find_node"
                  (Fmt.str "empty stream when looking for hash %a" pp_hash h)
            | Cons (v, rest) ->
                (* [depth] is 0 because this context deals with root nodes *)
                let v = rehydrate_stream_node ~depth:0 v h in
                let v = N.with_handler (find_recnode t) v in
                check_node_integrity v h;
                stream := rest;
                Hashes.add nodes h v;
                Some v))

  let add_recnode_from_store t find ~expected_depth h =
    assert (expected_depth > 0);
    match !t with
    | Stream (Produce { set; rev_elts }) -> (
        (* Registering when seen for the first time, there is no need
           for sharing. *)
        match find ~expected_depth h with
        | None -> None
        | Some v ->
            if not @@ Hashes.mem set h then (
              Hashes.add set h ();
              let elt = dehydrate_stream_node v in
              rev_elts := elt :: !rev_elts);
            Some v)
    | _ -> assert false

  let add_node_from_store t h v =
    match !t with
    | Empty -> v
    | Set { mode = Produce; set } ->
        (* Registering in [set] for sharing during [Produce] and traversal
           during [Serialise]. *)
        assert (not (Hashes.mem set.nodes h));
        Hashes.add set.nodes h v;
        v
    | Set { mode = Serialise; _ } ->
        (* There shouldn't be new nodes during this phase *)
        assert false
    | Set { mode = Deserialise; _ } ->
        (* This phase has no repo pointer *)
        assert false
    | Set { mode = Consume; _ } ->
        (* This phase has no repo pointer *)
        assert false
    | Stream (Produce { set; rev_elts }) ->
        (* Registering when seen for the first time and wrap its [find]
           function. *)
        if not @@ Hashes.mem set h then (
          Hashes.add set h ();
          let elt = dehydrate_stream_node v in
          rev_elts := elt :: !rev_elts);
        let v = N.with_handler (add_recnode_from_store t) v in
        v
    | Stream (Consume _) ->
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
end
