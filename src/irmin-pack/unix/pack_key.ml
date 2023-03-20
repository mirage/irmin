(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
include Pack_key_intf

type safe = SAFE
type unsafe = UNSAFE

type (_, _) unsafe_state =
  | Direct : {
      hash : 'hash;
      offset : int63;
      length : int;
      volume_identifier : Lower.volume_identifier option;
    }
      -> ('hash, safe) unsafe_state
  | Indexed : 'hash -> ('hash, safe) unsafe_state
  | Offset : int63 -> ('hash, unsafe) unsafe_state

type 'hash state = ('hash, safe) unsafe_state
type 'hash t = State : { mutable state : ('hash, _) unsafe_state } -> 'hash t

let inspect (State t) =
  match t.state with
  | Offset _ -> failwith "inspect unsafe Offset"
  | Direct d -> Direct d
  | Indexed d -> Indexed d

let to_hash (State t) =
  match t.state with
  | Direct t -> t.hash
  | Indexed h -> h
  | Offset _ -> failwith "Hash unavailable"

let to_offset (State t) =
  match t.state with
  | Direct t -> Some t.offset
  | Offset offset -> Some offset
  | Indexed _ -> None

let to_length (State t) =
  match t.state with
  | Direct t -> Some t.length
  | Offset _ -> None
  | Indexed _ -> None

let promote_exn ~offset ~length ?volume_identifier (State t) =
  match t.state with
  | Direct _ -> failwith "Attempted to promote a key that is already Direct"
  | Offset _ -> failwith "Attempted to promote an offset without hash"
  | Indexed hash ->
      t.state <- Direct { hash; offset; length; volume_identifier }

let set_volume_identifier_exn ~volume_identifier (State t) =
  match t.state with
  | Indexed _ ->
      failwith "Attempted to set volume identifier to a key that is Indexed"
  | Offset _ ->
      failwith "Attempted to set volume identifier to an offset without hash"
  | Direct { hash; offset; length; _ } ->
      t.state <- Direct { hash; offset; length; volume_identifier }

let t : type h. h Irmin.Type.t -> h t Irmin.Type.t =
 fun hash_t ->
  let open Irmin.Type in
  variant "t" (fun direct indexed t ->
      match inspect t with
      | Direct { hash; offset; length; _ } -> direct (hash, offset, length)
      | Indexed x1 -> indexed x1)
  |~ case1 "Direct" [%typ: hash * int63 * int] (fun (hash, offset, length) ->
         State
           { state = Direct { hash; offset; length; volume_identifier = None } })
  |~ case1 "Indexed" [%typ: hash] (fun x1 -> State { state = Indexed x1 })
  |> sealv

let t (type hash) (hash_t : hash Irmin.Type.t) =
  let module Hash = struct
    type t = hash
    [@@deriving irmin ~equal ~compare ~pre_hash ~encode_bin ~decode_bin]

    let unboxed_encode_bin = Irmin.Type.(unstage (Unboxed.encode_bin t))
    let unboxed_decode_bin = Irmin.Type.(unstage (Unboxed.decode_bin t))

    let encoded_size =
      match Irmin.Type.Size.of_value t with
      | Static n -> n
      | Dynamic _ | Unknown ->
          failwith "Hash must have a fixed-width binary encoding"
  end in
  (* Equality and ordering on keys respects {i structural} equality semantics,
     meaning two objects (containing keys) are considered equal even if their
     children are stored at different offsets (either as duplicates in the same
     pack file, or inside different pack files), or with different lengths (in
     the event that the encoding environments were different). *)
  let equal a b = Hash.equal (to_hash a) (to_hash b) in
  let compare a b = Hash.compare (to_hash a) (to_hash b) in
  (* The pre-hash image of a key is just the hash of the corresponding value.

     NOTE: it's particularly important that we discard the file offset when
     computing hashes of structured values (e.g. inodes), so that this hashing
     process is reproducible in different stores (w/ different offsets for the
     values). *)
  let pre_hash t f = Hash.pre_hash (to_hash t) f in
  let encode_bin t f = Hash.encode_bin (to_hash t) f in
  let unboxed_encode_bin t f = Hash.unboxed_encode_bin (to_hash t) f in
  let decode_bin buf pos_ref =
    State { state = Indexed (Hash.decode_bin buf pos_ref) }
  in
  let unboxed_decode_bin buf pos_ref =
    State { state = Indexed (Hash.unboxed_decode_bin buf pos_ref) }
  in
  let size_of = Irmin.Type.Size.custom_static Hash.encoded_size in
  Irmin.Type.like (t hash_t) ~pre_hash ~equal ~compare
    ~bin:(encode_bin, decode_bin, size_of)
    ~unboxed_bin:(unboxed_encode_bin, unboxed_decode_bin, size_of)

let v_direct ~offset ~length ?volume_identifier hash =
  State { state = Direct { hash; offset; length; volume_identifier } }

let v_indexed hash = State { state = Indexed hash }
let v_offset offset = State { state = Offset offset }

module type S = sig
  type hash

  include Irmin_pack.Pack_key.S with type t = hash t and type hash := hash
end

module Make (Hash : Irmin.Hash.S) = struct
  type nonrec t = Hash.t t [@@deriving irmin]
  type hash = Hash.t [@@deriving irmin ~of_bin_string]

  let to_hash = to_hash
  let null_offset = Int63.minus_one
  let null_length = -1

  let null =
    let buf = String.make Hash.hash_size '\000' in
    let hash =
      match hash_of_bin_string buf with Ok x -> x | Error _ -> assert false
    in
    v_direct ~offset:null_offset ~length:null_length hash

  let unfindable_of_hash hash =
    v_direct ~offset:null_offset ~length:null_length hash
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
