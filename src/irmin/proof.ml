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

  type elt =
    | Empty
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
exception Bad_stream of { context : string }

let bad_proof_exn context = raise (Bad_proof { context })
let bad_stream_exn context = raise (Bad_stream { context })

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

  module Contents_hash = Hash.Typed (H) (C)
  module Node_hash = Hash.Typed (H) (N)

  type hash = H.t [@@deriving irmin ~equal]

  (* Keep track of read effects happening during a computation using
     sets. This does not keep track of the ordering of the reads. *)
  type read_set = { nodes : N.t Hashes.t; contents : C.t Hashes.t }
  [@@deriving irmin]

  type stream = P.elt Queue.t

  let list_of_stream s = List.of_seq (Queue.to_seq s)
  let stream_of_list l = Queue.of_seq (List.to_seq l)

  let stream_t : stream Type.t =
    Type.map [%typ: P.elt list] stream_of_list list_of_stream

  type mode = Produce | Consume [@@deriving irmin]
  type set_effects = { mode : mode; set : read_set } [@@deriving irmin]

  type stream_effects = { mode : mode; set : read_set; stream : stream }
  [@@deriving irmin]

  type v = Empty | Set of set_effects | Stream of stream_effects
  [@@deriving irmin]

  type t = v ref

  let t = Type.map v_t ref ( ! )
  let empty () : t = ref Empty
  let is_empty t = !t = Empty
  let empty_set () = { contents = Hashes.create 13; nodes = Hashes.create 13 }
  let empty_stream () = Queue.create ()
  let copy ~into t = into := !t

  let is_empty_stream t =
    match !t with
    | Stream { mode = Consume; stream; _ } -> Queue.is_empty stream
    | _ -> false

  let mode t =
    match !t with
    | Empty -> None
    | Set { mode; _ } | Stream { mode; _ } -> Some mode

  let set mode =
    let set = empty_set () in
    ref (Set { mode; set })

  let stream mode =
    let set = empty_set () in
    let stream = empty_stream () in
    ref (Stream { mode; set; stream })

  let of_stream (s : P.stream) =
    let v =
      Stream { mode = Consume; set = empty_set (); stream = Queue.of_seq s }
    in
    ref v

  let to_stream t =
    match !t with
    | Stream { stream; mode = Produce; _ } -> Some (Queue.to_seq stream)
    | _ -> None

  let track_reads_as_sets_lwt mode f =
    let t = set mode in
    let+ res = f t in
    t := Empty;
    res

  let track_reads_as_sets mode f =
    let t = set mode in
    let res = f t in
    t := Empty;
    res

  let track_reads_as_stream_lwt mode f =
    let t = stream mode in
    let+ res = f t in
    t := Empty;
    res

  let bad_stream_exn s = bad_stream_exn ("Proof.Env." ^ s)

  let check_content_integrity c h =
    let c = Contents_hash.hash c in
    if not (equal_hash c h) then bad_stream_exn "check_content_integrity"

  let check_node_integrity n h =
    let c = Node_hash.hash n in
    if not (equal_hash c h) then bad_stream_exn "check_node_integrity"

  let consume_contents (s : stream) h =
    let bad_stream_exn s = bad_stream_exn ("consume_contents: " ^ s) in
    match Queue.take s with
    | Empty -> None
    | Contents c ->
        check_content_integrity c h;
        Some c
    | Node _ -> bad_stream_exn "node"
    | Inode _ -> bad_stream_exn "inode"
    | exception Queue.Empty -> bad_stream_exn "empty"

  let rec consume_node ~depth (s : stream) h =
    let bad_stream_exn s = bad_stream_exn ("consume_node: " ^ s) in
    match Queue.take s with
    | Empty -> None
    | Node n -> (
        match N.of_values ~depth n with
        | None -> bad_stream_exn "consume_node: Node"
        | Some n as r ->
            check_node_integrity n h;
            r)
    | Inode i -> (
        match N.of_inode ~depth ~length:i.length i.proofs with
        | None -> bad_stream_exn "consume_node: Inode"
        | Some n ->
            let n =
              N.with_handler
                (fun _find ~expected_depth h ->
                  consume_node ~depth:expected_depth s h)
                n
            in
            check_node_integrity n h;
            Some n)
    | Contents _ -> bad_stream_exn "contents"
    | exception Queue.Empty -> bad_stream_exn "empty"

  let add_contents_to_set t h v =
    match !t with
    | Set { set; _ } | Stream { set; _ } -> Hashes.add set.contents h v
    | _ -> ()

  let add_contents_to_stream t h v =
    match !t with
    | Stream { set; stream; _ } ->
        if Hashes.mem set.contents h then ()
        else Queue.add (P.Contents v) stream
    | _ -> ()

  let add_no_contents t h =
    match !t with
    | Stream { set; stream; _ } ->
        if Hashes.mem set.contents h then () else Queue.add P.Empty stream
    | _ -> ()

  let add_contents t h v =
    add_contents_to_stream t h v;
    add_contents_to_set t h v

  let add_contents_opt t h = function
    | None -> add_no_contents t h
    | Some v -> add_contents t h v

  let add_node_to_stream t h n =
    match !t with
    | Stream { stream; set; _ } ->
        if not (Hashes.mem set.nodes h) then
          let v =
            match N.to_elt n with
            | `Empty -> assert false
            | `Node n -> P.Node n
            | `Inode (length, proofs) -> P.Inode { length; proofs }
          in
          Queue.add v stream
    | _ -> ()

  let add_node_to_set t h v =
    match !t with
    | Set { set; _ } | Stream { set; _ } -> Hashes.add set.nodes h v
    | _ -> ()

  let add_no_node t h =
    match !t with
    | Stream { set; stream; _ } ->
        if not (Hashes.mem set.nodes h) then Queue.add P.Empty stream
    | _ -> ()

  let find_contents ~consume t h =
    match !t with
    | Stream { stream; mode = Consume; _ } when consume ->
        let v = consume_contents stream h in
        Option.iter (add_contents_to_set t h) v;
        v
    | Set { set; _ } | Stream { set; _ } -> Hashes.find_opt set.contents h
    | Empty -> None

  let find_node ~consume t ?depth h =
    match !t with
    | Stream { stream; mode = Consume; _ } when consume ->
        let depth = match depth with None -> 0 | Some d -> d in
        let v = consume_node ~depth stream h in
        Option.iter (add_node_to_set t h) v;
        v
    | Set { set; _ } | Stream { set; _ } -> Hashes.find_opt set.nodes h
    | Empty -> None

  (* Wrap backend's [find] function in order to handle its side effects *)
  let rec handle : t -> 'a -> 'a =
   fun t find ->
    let find' ~expected_depth h =
      match !t with
      | Empty -> find ~expected_depth h
      | Set { mode = Consume; _ } | Stream { mode = Consume; _ } ->
          (* [find'] should never hit the storage in [Consume] mode, it should
             only hit the env. *)
          find_node ~consume:true ~depth:expected_depth t h
      | Set { mode = Produce; _ } | Stream { mode = Produce; _ } ->
          (* Call the backend's [find] function and push the result into the
             env before returning the value back to the backend. *)
          find ~expected_depth h |> add_node_opt t h
    in
    find'

  and add_node (t : t) h v =
    let tracked_v = N.with_handler (handle t) v in
    add_node_to_stream t h v;
    add_node_to_set t h tracked_v;
    tracked_v

  and add_node_opt t h = function
    | None ->
        add_no_node t h;
        None
    | Some v -> Some (add_node t h v)

  let find_contents_opt ~consume t = function
    | None -> None
    | Some h -> find_contents ~consume t h

  let find_node_opt ~consume t = function
    | None -> None
    | Some h -> find_node ~consume t h

  (* x' = y' <- x union y *)
  let merge (x : t) (y : t) =
    match (!x, !y) with
    | Empty, Empty -> ()
    | Empty, y -> x := y
    | x, Empty -> y := x
    | _ -> failwith "Merging two non-empty [Proof.Env.t] is forbidden"
end
