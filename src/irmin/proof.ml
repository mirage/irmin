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
  type 'a inode = { length : int; proofs : (int * 'a) list } [@@deriving irmin]

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

  type mode = Produce | Consume [@@deriving irmin]
  type set_effects = { mode : mode; set : read_set } [@@deriving irmin]
  type v = Empty | Set of set_effects [@@deriving irmin]
  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty
  let empty_set () = { contents = Hashes.create 13; nodes = Hashes.create 13 }
  let copy ~into t = into := !t
  let mode t = match !t with Empty -> None | Set { mode; _ } -> Some mode

  let reads_as_set mode =
    let set = empty_set () in
    ref (Set { mode; set })

  let track_reads_as_sets_lwt mode f =
    let t = reads_as_set mode in
    let+ res = f t in
    t := Empty;
    res

  let track_reads_as_sets mode f =
    let t = reads_as_set mode in
    let res = f t in
    t := Empty;
    res

  let find_contents t h =
    match !t with
    | Set { set; _ } -> Hashes.find_opt set.contents h
    | Empty -> None

  let add_contents t h v =
    match !t with Set { set; _ } -> Hashes.add set.contents h v | Empty -> ()

  let add_contents_opt t h = function
    | Some v -> add_contents t h v
    | None -> ()

  let add_node_to_set t h v =
    match !t with Set { set; _ } -> Hashes.add set.nodes h v | _ -> ()

  let find_node t ?depth:_ h =
    match !t with
    | Set { set; _ } -> Hashes.find_opt set.nodes h
    | Empty -> None

  (* Wrap backend's [find] function in order to handle its side effects *)
  let rec handle : t -> 'a -> 'a =
   fun t find ->
    let find' ~expected_depth h =
      match !t with
      | Empty -> find ~expected_depth h
      | Set { mode = Consume; _ } ->
          (* [find'] should never hit the storage in [Consume] mode, it should
             only hit the env. *)
          find_node ~depth:expected_depth t h
      | Set { mode = Produce; _ } ->
          (* Call the backend's [find] function and push the result into the
             env before returning the value back to the backend. *)
          find ~expected_depth h |> add_node_opt t h
    in
    find'

  and add_node (t : t) h v =
    let tracked_v = N.with_handler (handle t) v in
    add_node_to_set t h tracked_v;
    tracked_v

  and add_node_opt t h = function
    | None -> None
    | Some v -> Some (add_node t h v)

  let find_contents_opt t = function
    | None -> None
    | Some h -> find_contents t h

  let find_node_opt t = function None -> None | Some h -> find_node t h

  (* x' = y' <- x union y *)
  let merge (x : t) (y : t) =
    match (!x, !y) with
    | Empty, Empty -> ()
    | Empty, y -> x := y
    | x, Empty -> y := x
    | Set _, Set _ ->
        failwith "Merging two non-empty [Proof.Env.t] is forbidden"
end
