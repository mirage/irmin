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

  type 'a t = { before : kinded_hash; after : kinded_hash; state : 'a }
  [@@deriving irmin]

  let before t = t.before
  let after t = t.after
  let state t = t.state
  let v ~before ~after state = { after; before; state }
end

type bad_stream_exn =
  | Stream_too_long of { context : string; reason : string }
  | Stream_too_short of { context : string; reason : string }
  | Proof_mismatch of { context : string; reason : string }

exception Bad_proof of { context : string }
exception Bad_stream of bad_stream_exn

let bad_proof_exn context = raise (Bad_proof { context })

let bad_stream_too_long context reason =
  raise (Bad_stream (Stream_too_long { context; reason }))

let bad_stream_too_short context reason =
  raise (Bad_stream (Stream_too_short { context; reason }))

let bad_stream_exn context reason =
  raise (Bad_stream (Proof_mismatch { context; reason }))

let bad_stream_exn_fmt s fmt = Fmt.kstr (bad_stream_exn ("Proof.Env." ^ s)) fmt

let bad_stream_too_short_fmt s fmt =
  Fmt.kstr (bad_stream_too_short ("Proof.Env." ^ s)) fmt

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
  type kind = Set | Stream [@@deriving irmin]

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

  module Stream = struct
    let ref_t v = Type.map v ref ( ! )

    type produce = {
      set : unit Hashes.t;
      singleton_inodes : (int * H.t) Hashes.t;
      rev_elts : (H.t * P.elt) list ref;
      rev_elts_size : int ref;
    }
    [@@deriving irmin]

    type consume = {
      nodes : B.Node_portable.t Hashes.t;
      contents : B.Contents.Val.t Hashes.t;
      stream : P.elt Seq.t ref;
    }
    [@@deriving irmin]

    type t = Produce of produce | Consume of consume [@@deriving irmin]

    let producer () =
      let set = Hashes.create 13 in
      let singleton_inodes = Hashes.create 13 in
      let rev_elts = ref [] in
      let rev_elts_size = ref 0 in
      Produce { set; singleton_inodes; rev_elts; rev_elts_size }

    let consumer stream =
      let nodes = Hashes.create 13 in
      let contents = Hashes.create 13 in
      let stream = ref stream in
      Consume { nodes; contents; stream }

    let push { rev_elts; rev_elts_size; _ } h_elt index =
      incr rev_elts_size;
      rev_elts := List.insert_exn !rev_elts index h_elt
  end

  type v = Empty | Set of Set.t | Stream of Stream.t [@@deriving irmin]
  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty
  let copy ~into t = into := !t

  type hash = H.t [@@deriving irmin ~equal ~pp]

  let rec forward_lookup h singleton_inodes : (int * hash) list option =
    match Hashes.find_opt singleton_inodes h with
    | None -> None
    | Some (i', h') -> (
        match forward_lookup h' singleton_inodes with
        | None -> Some [ (i', h') ]
        | Some l -> Some ((i', h') :: l))

  let apply_extenders ~length singleton_inodes skips proofs =
    let rec accumulate_segments ~(acc : int Reversed_list.t) h = function
      | [] -> (Reversed_list.rev acc, h)
      | (i', h') :: rest -> accumulate_segments ~acc:(i' :: acc) h' rest
    in
    let inode = P.Inode { length; proofs } in
    match proofs with
    | [ (i, h) ] -> (
        match forward_lookup h singleton_inodes with
        | None -> inode
        | Some ls -> (
            let () =
              (* Push all hashes except the last one into [skips] *)
              match List.rev ((i, h) :: ls) with
              | [] | [ _ ] -> failwith "idk"
              | _ :: tl -> List.iter (fun (_, h) -> Hashes.add skips h ()) tl
            in
            let i, h = accumulate_segments ~acc:[ i ] h ls in
            match i with
            | [] | [ _ ] -> assert false
            | segments -> P.Inode_extender { length; segments; proof = h }))
    | _ -> inode

  let post_processing singleton_inodes (stream : (hash * P.elt) list) :
      P.elt list =
    let skips = Hashes.create 13 in
    (* [skips] are the elements of the [stream] that are included in the
       extenders, they will be removed from the final stream. *)
    let rec aux rev_elts = function
      | [] -> List.rev rev_elts
      | (h, elt) :: rest ->
          if Hashes.mem skips h then aux rev_elts rest
          else
            let elt' : P.elt =
              match (elt : P.elt) with
              | P.Inode { length; proofs } ->
                  apply_extenders ~length singleton_inodes skips proofs
              | Node ls -> Node ls
              | Contents c -> Contents c
              | Inode_extender _ -> assert false
            in
            aux (elt' :: rev_elts) rest
    in
    aux [] stream

  let to_stream t =
    match !t with
    | Stream (Produce { rev_elts; singleton_inodes; _ }) ->
        List.rev !rev_elts |> post_processing singleton_inodes |> List.to_seq
    | _ -> assert false

  let is_empty_stream t =
    match !t with
    | Stream (Consume { stream; _ }) -> (
        (* Peek the sequence but do not advance the ref *)
        match !stream () with Seq.Nil -> true | _ -> false)
    | _ -> false

  let set_mode t (kind : kind) mode =
    match kind with
    | Set -> (
        match (!t, mode) with
        | Empty, Produce -> t := Set Set.(producer ())
        | Empty, Deserialise -> t := Set Set.(deserialiser ())
        | Set (Produce set), Serialise -> t := Set Set.(Serialise set)
        | Set (Deserialise set), Consume -> t := Set Set.(Consume set)
        | _ -> assert false)
    | Stream -> (
        match (!t, mode) with
        | Empty, Produce -> t := Stream (Stream.producer ())
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
    let t = Stream (Stream.consumer stream) |> ref in
    let is_empty () = is_empty_stream t in
    let+ res = f t ~is_empty in
    t := Empty;
    res

  module Contents_hash = Hash.Typed (H) (B.Contents.Val)

  let check_contents_integrity v h =
    let h' = Contents_hash.hash v in
    if not (equal_hash h' h) then
      bad_stream_exn_fmt "check_contents_integrity" "got %a expected %a" pp_hash
        h' pp_hash h

  let check_node_integrity v h =
    let h' =
      try B.Node_portable.hash_exn ~force:false v
      with Not_found ->
        (* [v] is out of [of_proof], it is supposed to have its hash available
           without IOs.

           If these IOs were to occur, it would corrupt the stream being read.
        *)
        assert false
    in
    if not (equal_hash h' h) then
      bad_stream_exn_fmt "check_node_integrity" "got %a expected %a" pp_hash h'
        pp_hash h

  let dehydrate_stream_node v =
    (* [v] is fresh out of the node store, meaning that if it is represented
       recursively it is still in a shallow state.

       [head v] might trigger IOs. It is fine because [v] is already wrapped
       with [with_handler]. *)
    match B.Node.Val.head v with
    | `Node l ->
        let l =
          List.map
            (function
              | step, `Contents (k, m) ->
                  (step, `Contents (B.Contents.Key.to_hash k, m))
              | step, `Node k -> (step, `Node (B.Node.Key.to_hash k)))
            l
        in
        P.Node l
    | `Inode (length, proofs) -> P.Inode { length; proofs }

  let rehydrate_stream_node ~depth (elt : P.elt) h =
    let bad_stream_exn_fmt = bad_stream_exn_fmt "rehydrate_stream_node" in
    match elt with
    | Contents _ ->
        bad_stream_exn_fmt
          "found contents at depth %d when looking for node with hash %a" depth
          pp_hash h
    | Node l -> (
        match B.Node_portable.of_proof ~depth (`Values l) with
        | Some v -> v
        | None ->
            bad_stream_exn_fmt
              "could not deserialise Node at depth %d when looking for hash %a"
              depth pp_hash h)
    | Inode { length; proofs } ->
        let proofs = List.map (fun (i, h) -> (i, `Blinded h)) proofs in
        let inode = `Inode (length, proofs) in
        let v =
          match B.Node_portable.of_proof ~depth inode with
          | Some v -> v
          | None ->
              bad_stream_exn_fmt
                "could not deserialise Inode at depth %d when looking for hash \
                 %a"
                depth pp_hash h
        in
        v
    | Inode_extender { length; segments; proof } ->
        let elt =
          List.fold_left
            (fun acc i -> `Inode (length, [ (i, acc) ]))
            (`Blinded proof) (List.rev segments)
        in
        let v =
          match B.Node_portable.of_proof ~depth elt with
          | Some v -> v
          | None ->
              bad_stream_exn_fmt
                "could not deserialise Inode at depth %d when looking for hash \
                 %a"
                depth pp_hash h
        in
        v

  let rehydrate_stream_contents (elt : P.elt) h =
    let err k =
      bad_stream_exn_fmt "find_contents"
        "found %s when looking Contents with hash %a" k pp_hash h
    in
    match elt with
    | Node _ -> err "Node"
    | Inode _ -> err "Inode"
    | Inode_extender _ -> err "Inode"
    | Contents v -> v

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
                bad_stream_too_short_fmt "find_contents"
                  "empty stream when looking for hash %a" pp_hash h
            | Cons (elt, rest) ->
                let v = rehydrate_stream_contents elt h in
                check_contents_integrity v h;
                stream := rest;
                Hashes.add contents h v;
                Some v))

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
    | Stream (Produce ({ set; _ } as cache)) ->
        (* Registering when seen for the first time *)
        if not @@ Hashes.mem set h then (
          Hashes.add set h ();
          let h_elt : hash * P.elt = (h, Contents v) in
          Stream.push cache h_elt 0)
    | Stream (Consume _) ->
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
    | Stream (Produce _) ->
        (* There is no need for sharing with stream proofs *)
        None
    | Stream (Consume _) ->
        (* This phase looks for portable nodes *)
        None

  let find_recpnode t _find ~expected_depth h =
    assert (expected_depth > 0);
    match !t with
    | Stream (Consume { nodes; stream; _ }) -> (
        (* Use the Env to feed the values during consume *)
        match Hashes.find_opt nodes h with
        | Some v -> Some v
        | None -> (
            match !stream () with
            | Seq.Nil ->
                bad_stream_too_short_fmt "find_recnode"
                  "empty stream when looking for hash %a" pp_hash h
            | Cons (v, rest) ->
                let v = rehydrate_stream_node ~depth:expected_depth v h in
                (* There is no need to apply [with_handler] here because there
                   is no repo pointer in this inode. *)
                check_node_integrity v h;
                stream := rest;
                Hashes.add nodes h v;
                Some v))
    | _ -> assert false

  let find_pnode t h =
    match !t with
    | Set (Consume set) ->
        (* [set] has been filled during deserialise. Using it to provide values
            during consume. *)
        Hashes.find_opt set.nodes h
    | Stream (Consume { nodes; stream; _ }) -> (
        (* Use the Env to provide the values during consume. Since all hashes
             are unique in [stream], [nodes] provides a hash-based sharing. *)
        match Hashes.find_opt nodes h with
        | Some v -> Some v
        | None -> (
            match !stream () with
            | Seq.Nil ->
                bad_stream_too_short_fmt "find_node"
                  "empty stream when looking for hash %a" pp_hash h
            | Cons (v, rest) ->
                (* Shorten [stream] before calling [head] as it might itself
                   perform reads. *)
                stream := rest;
                let v =
                  (* [depth] is 0 because this context deals with root nodes *)
                  rehydrate_stream_node ~depth:0 v h
                in
                let v =
                  (* Call [with_handler] before [head] because the later might
                     perform reads *)
                  B.Node_portable.with_handler (find_recpnode t) v
                in
                let (_ : [ `Node of _ | `Inode of _ ]) =
                  (* At produce time [dehydrate_stream_node] called [head] which
                     might have performed IOs. If it did then we must consume
                     the stream accordingly right now in order to preserve
                     stream ordering. *)
                  B.Node_portable.head v
                in
                check_node_integrity v h;
                Hashes.add nodes h v;

                Some v))
    | _ -> None

  let add_recnode_from_store t find ~expected_depth k =
    assert (expected_depth > 0);
    match !t with
    | Stream (Produce ({ set; singleton_inodes; _ } as cache)) -> (
        (* Registering when seen for the first time, there is no need
           for sharing. *)
        match find ~expected_depth k with
        | None -> None
        | Some v ->
            let h = B.Node.Key.to_hash k in
            if not @@ Hashes.mem set h then (
              Hashes.add set h ();
              let elt = dehydrate_stream_node v in
              let () =
                match elt with
                | P.Inode { proofs = [ bucket ]; _ } ->
                    Hashes.add singleton_inodes h bucket
                | _ -> ()
              in
              Stream.push cache (h, elt) 0);
            Some v)
    | _ -> assert false

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
    | Stream (Produce ({ set; rev_elts_size; singleton_inodes; _ } as cache)) ->
        (* Registering when seen for the first time and wrap its [find]
           function. Since there is no sharing during the production of
           streamed proofs, the hash may already have been seened. *)
        let new_hash = not @@ Hashes.mem set h in
        let v =
          (* In all case [v] should be wrapped.
             If [not new_hash] then wrap it for future IOs on it.

             If [new_hash] then it additionally should be wrapped before
             calling [dehydrate_stream_node] as this call may trigger IOs. *)
          B.Node.Val.with_handler (add_recnode_from_store t) v
        in
        if new_hash then (
          Hashes.add set h ();
          let len0 = !rev_elts_size in
          let elt = dehydrate_stream_node v in
          let len1 = !rev_elts_size in
          let delta =
            (* [delta] is the number of reads that were performed by
               [dehydrate_stream_node]. *)
            len1 - len0
          in
          let () =
            match elt with
            | P.Inode { proofs = [ bucket ]; _ } ->
                Hashes.add singleton_inodes h bucket
            | _ -> ()
          in
          (* if [delta = 0] then push the pair at the head of the list.

             if [delta > 0] then insert it before the calls that it triggered. *)
          Stream.push cache (h, elt) delta);
        v
    | Stream (Consume _) ->
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
