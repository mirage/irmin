open! Import
include Pack_key_intf

type kind = Direct | Indexed

type 'hash t =
  | Direct of { hash : 'hash; offset : int63; length : int }
  | Direct_blindfolded of { hash : 'hash; offset : int63 }
  | Indexed of 'hash

let hash = function
  | Direct t -> t.hash
  | Direct_blindfolded t -> t.hash
  | Indexed h -> h

let t hash =
  let open Irmin.Type in
  variant "t" (fun direct direct_blindfolded indexed -> function
    | Direct { hash; offset; length } -> direct (hash, offset, length)
    | Direct_blindfolded { hash; offset } -> direct_blindfolded (hash, offset)
    | Indexed x1 -> indexed x1)
  |~ case1 "Direct" (triple hash int63 int) (fun (hash, offset, length) ->
         Direct { hash; offset; length })
  |~ case1 "Direct_blindfolded" (pair hash int63) (fun (hash, offset) ->
         Direct_blindfolded { hash; offset })
  |~ case1 "Indexed" hash (fun x1 -> Indexed x1)
  |> sealv

let t (type hash) (kind : kind) (hash_t : hash Irmin.Type.t) =
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
    let result = Hash.equal (hash a) (hash b) in
    assert (
      if not result then true
      else
        match (a, b) with
        | Direct a, Direct b -> a.length = b.length
        | _, _ -> true);
    result
  in
  let compare a b = Hash.compare (hash a) (hash b) in
  let pre_hash t f = Hash.pre_hash (hash t) f in
  let encode_bin x f =
    match (kind, x) with
    | ( Direct,
        ( Direct { hash; offset; length = _ }
        | Direct_blindfolded { hash; offset } ) ) ->
        Hash.encode_bin hash f;
        let buf = Bytes.create (8 + 4) in
        Bytes.set_int64_be buf 0 (Int63.to_int64 offset);
        f (Bytes.unsafe_to_string buf)
    | Indexed, Indexed t -> Hash.encode_bin t f
    | Indexed, (Direct { hash; _ } | Direct_blindfolded { hash; _ }) ->
        Hash.encode_bin hash f
    | Direct, Indexed _ -> assert false
  in
  let decode_bin =
    match kind with
    | Direct ->
        fun buf pos_ref ->
          let hash = Hash.decode_bin buf pos_ref in
          let buf = Bytes.unsafe_of_string buf in
          let offset = Bytes.get_int64_be buf !pos_ref |> Int63.of_int64 in
          pos_ref := !pos_ref + 8;
          Direct_blindfolded { hash; offset }
    | Indexed -> fun buf pos_ref -> Indexed (Hash.decode_bin buf pos_ref)
  in
  let size_of =
    match kind with
    | Direct -> Irmin.Type.Size.custom_static (Hash.encoded_size + 8 + 4)
    | Indexed -> Irmin.Type.Size.custom_static Hash.encoded_size
  in
  Irmin.Type.like (t hash_t) ~pre_hash ~equal ~compare
    ~bin:(encode_bin, decode_bin, size_of)

let v ~hash ~offset ~length = Direct { hash; offset; length }
let v_blindfolded ~hash ~offset = Direct_blindfolded { hash; offset }

module type S = sig
  type hash

  include S with type t = hash t and type hash := hash
end

module Make_no_repr (Hash : Irmin.Hash.S) = struct
  type nonrec t = Hash.t t
  type hash = Hash.t [@@deriving irmin ~of_bin_string]

  let to_hash = hash

  let to_offset = function
    | Direct t -> t.offset
    | Direct_blindfolded t -> t.offset
    | Indexed _ -> assert false

  let to_length = function
    | Direct t -> t.length
    | Direct_blindfolded _ | Indexed _ -> assert false

  let null_offset = Int63.minus_one
  let null_length = -1

  let null =
    let buf = String.make Hash.hash_size '\000' in
    let hash =
      match hash_of_bin_string buf with Ok x -> x | Error _ -> assert false
    in
    Direct { hash; offset = null_offset; length = null_length }

  let unfindable_of_hash hash =
    Direct { hash; offset = null_offset; length = null_length }
end

module Make (Hash : Irmin.Hash.S) = struct
  include Make_no_repr (Hash)

  let t = t Direct Hash.t
end

module Make_indexed (Hash : Irmin.Hash.S) = struct
  include Make_no_repr (Hash)

  let t = t Indexed Hash.t
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
