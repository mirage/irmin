open! Import
include Pack_key_intf

module Codec = struct
  type t = Hash_and_offset | Hash
end

type 'hash state =
  | Direct of { hash : 'hash; offset : int63; length : int }
  | Direct_unknown_length of { hash : 'hash; offset : int63 }
  | Indexed of 'hash

type 'hash t = { mutable state : 'hash state }

let inspect t = t.state

let to_hash t =
  match t.state with
  | Direct t -> t.hash
  | Direct_unknown_length t -> t.hash
  | Indexed h -> h

let promote_exn t ~offset ~length =
  let () =
    match t.state with
    | Direct _ ->
        Fmt.failwith "Attempted to promote a key that is already Direct"
    | Direct_unknown_length t ->
        if not (Int63.equal offset t.offset) then
          Fmt.failwith
            "Attempted to promote a key with offset %a to one at offset %a"
            Int63.pp t.offset Int63.pp offset
    | Indexed _ -> ()
  in
  t.state <- Direct { hash = to_hash t; offset; length }

let t : type h. h Irmin.Type.t -> h t Irmin.Type.t =
 fun hash_t ->
  let open Irmin.Type in
  variant "t" (fun direct direct_blindfolded indexed t ->
      match t.state with
      | Direct { hash; offset; length } -> direct (hash, offset, length)
      | Direct_unknown_length { hash; offset } ->
          direct_blindfolded (hash, offset)
      | Indexed x1 -> indexed x1)
  |~ case1 "Direct" [%typ: hash * int63 * int] (fun (hash, offset, length) ->
         { state = Direct { hash; offset; length } })
  |~ case1 "Direct_unknown_length" [%typ: hash * int63] (fun (hash, offset) ->
         { state = Direct_unknown_length { hash; offset } })
  |~ case1 "Indexed" [%typ: hash] (fun x1 -> { state = Indexed x1 })
  |> sealv

let t (type hash) (codec : Codec.t) (hash_t : hash Irmin.Type.t) =
  let module Hash = struct
    type t = hash
    [@@deriving irmin ~equal ~compare ~pre_hash ~encode_bin ~decode_bin]

    let encoded_size =
      match Irmin.Type.Size.of_value t with
      | Static n -> n
      | Dynamic _ | Unknown ->
          Fmt.failwith "Hash must have a fixed-width binary encoding"
  end in
  (* The pre-hash image of a key is just the hash of the corresponding value.

     NOTE: it's particularly important that we discard the file offset when
     computing hashes of structured values (e.g. inodes), so that this hashing
     process is reproducible in different stores (w/ different offsets for the
     values). *)
  let equal a b =
    let result = Hash.equal (to_hash a) (to_hash b) in
    assert (
      if not result then true
      else
        match (a.state, b.state) with
        | Direct a, Direct b -> a.length = b.length
        | _, _ -> true);
    result
  in
  let compare a b = Hash.compare (to_hash a) (to_hash b) in
  let pre_hash t f = Hash.pre_hash (to_hash t) f in
  let encode_bin x f =
    match (codec, x.state) with
    | ( Hash_and_offset,
        ( Direct { hash; offset; length = _ }
        | Direct_unknown_length { hash; offset } ) ) ->
        Hash.encode_bin hash f;
        let buf = Bytes.create 8 in
        Bytes.set_int64_be buf 0 (Int63.to_int64 offset);
        f (Bytes.unsafe_to_string buf)
    | Hash, Indexed t -> Hash.encode_bin t f
    | Hash, (Direct { hash; _ } | Direct_unknown_length { hash; _ }) ->
        Hash.encode_bin hash f
    | Hash_and_offset, Indexed _ -> assert false
  in
  let decode_bin =
    match codec with
    | Hash_and_offset ->
        fun buf pos_ref ->
          let hash = Hash.decode_bin buf pos_ref in
          let buf = Bytes.unsafe_of_string buf in
          let offset = Bytes.get_int64_be buf !pos_ref |> Int63.of_int64 in
          pos_ref := !pos_ref + 8;
          { state = Direct_unknown_length { hash; offset } }
    | Hash ->
        fun buf pos_ref -> { state = Indexed (Hash.decode_bin buf pos_ref) }
  in
  let size_of =
    match codec with
    | Hash_and_offset -> Irmin.Type.Size.custom_static (Hash.encoded_size + 8)
    | Hash -> Irmin.Type.Size.custom_static Hash.encoded_size
  in
  Irmin.Type.like (t hash_t) ~pre_hash ~equal ~compare
    ~bin:(encode_bin, decode_bin, size_of)

let v_direct ~hash ~offset ~length =
  { state = Direct { hash; offset; length } }

let v_indexed hash = { state = Indexed hash }

module type S = sig
  type hash

  include S with type t = hash t and type hash := hash
end

module Make_no_repr (Hash : Irmin.Hash.S) = struct
  type nonrec t = Hash.t t
  type hash = Hash.t [@@deriving irmin ~of_bin_string]

  let to_hash = to_hash
  let null_offset = Int63.minus_one
  let null_length = -1

  let null =
    let buf = String.make Hash.hash_size '\000' in
    let hash =
      match hash_of_bin_string buf with Ok x -> x | Error _ -> assert false
    in
    v_direct ~hash ~offset:null_offset ~length:null_length

  let unfindable_of_hash hash =
    v_direct ~hash ~offset:null_offset ~length:null_length
end

module Make (Hash : Irmin.Hash.S) = struct
  include Make_no_repr (Hash)

  let t = t Codec.Hash_and_offset Hash.t
end

module Make_indexed (Hash : Irmin.Hash.S) = struct
  include Make_no_repr (Hash)

  let t = t Codec.Hash Hash.t
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
