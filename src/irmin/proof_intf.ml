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

module Stream = struct
  type ('hash, 'metadata) value =
    [ `Node of 'hash | `Contents of 'hash * 'metadata ]
  [@@deriving irmin]

  type ('hash, 'step, 'metadata) elt =
    | Empty
    | Node of ('step * ('hash, 'metadata) value) list
    | Inode of { length : int; proofs : (int * 'hash) list }
    | Contents of 'hash * 'metadata

  (* TODO(craigfe): fix [ppx_irmin] for inline parameters. *)
  let elt_t hash_t step_t metadata_t =
    let open Type in
    variant "stream" (fun empty node inode contents -> function
      | Empty -> empty
      | Node x1 -> node x1
      | Inode { length; proofs } -> inode (length, proofs)
      | Contents (h, m) -> contents (h, m))
    |~ case0 "Empty" Empty
    |~ case1 "Node" [%typ: (step * (hash, metadata) value) list] (fun x1 ->
           Node x1)
    |~ case1 "Inode" [%typ: int * (int * hash) list] (fun (length, proofs) ->
           Inode { length; proofs })
    |~ case1 "Contents" [%typ: hash * metadata] (fun (h, m) -> Contents (h, m))
    |> Type.sealv

  type ('hash, 'step, 'metadata) t = ('hash, 'step, 'metadata) elt Seq.t

  let t hash_t step_t metadata_t =
    Type.map [%typ: (hash, step, metadata) elt list] List.to_seq List.of_seq
end

module Proof = struct
  type ('hash, 'step, 'metadata) t =
    | Blinded_node of 'hash
    | Blinded_contents of 'hash * 'metadata
    | Node of ('step * ('hash, 'step, 'metadata) t) list
    | Inode of {
        length : int;
        proofs : (int * ('hash, 'step, 'metadata) t) list;
      }

  (* TODO(craigfe): fix [ppx_irmin] for inline parameters. *)
  let t hash_t step_t metadata_t =
    let open Type in
    mu (fun t ->
        variant "proof" (fun blinded_node node inode blinded_contents ->
          function
          | Blinded_node x1 -> blinded_node x1
          | Node x1 -> node x1
          | Inode { length; proofs } -> inode (length, proofs)
          | Blinded_contents (x1, x2) -> blinded_contents (x1, x2))
        |~ case1 "Blinded_node" hash_t (fun x1 -> Blinded_node x1)
        |~ case1 "Node" [%typ: (step * t) list] (fun x1 -> Node x1)
        |~ case1 "Inode" [%typ: int * (int * t) list] (fun (length, proofs) ->
               Inode { length; proofs })
        |~ case1 "Blinded_contents" [%typ: hash * metadata] (fun (x1, x2) ->
               Blinded_contents (x1, x2))
        |> Type.sealv)

  exception End_of_stream
  exception Bad_stream of { context : string }
  exception Bad_proof of { context : string }
end

module type Proof = sig
  include module type of Proof
  (** @inline *)

  val bad_proof_exn : string -> 'a
  val bad_stream_exn : string -> 'a
  val end_of_stream_exn : unit -> 'a

  module Stream : sig
    include module type of Stream
    (** @inline *)
  end

  type ('a, 'b, 'c) stream = ('a, 'b, 'c) Stream.t [@@deriving irmin]
end
