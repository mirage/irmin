open! Import
include Pack_key_intf

type 'hash state =
  | Direct of { hash : 'hash; offset : int63; length : int }
  | Indexed of 'hash

type 'hash t = { mutable state : 'hash state }

let inspect t = t.state
let to_hash t = match t.state with Direct t -> t.hash | Indexed h -> h

let promote_exn t ~offset ~length =
  let () =
    match t.state with
    | Direct _ ->
        Fmt.failwith "Attempted to promote a key that is already Direct"
    | Indexed _ -> ()
  in
  t.state <- Direct { hash = to_hash t; offset; length }

let t : type h. h Irmin.Type.t -> h t Irmin.Type.t =
 fun hash_t ->
  let open Irmin.Type in
  variant "t" (fun direct indexed t ->
      match t.state with
      | Direct { hash; offset; length } -> direct (hash, offset, length)
      | Indexed x1 -> indexed x1)
  |~ case1 "Direct" [%typ: hash * int63 * int] (fun (hash, offset, length) ->
         { state = Direct { hash; offset; length } })
  |~ case1 "Indexed" [%typ: hash] (fun x1 -> { state = Indexed x1 })
  |> sealv

let t (type hash) (hash_t : hash Irmin.Type.t) =
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
  let encode_bin t f = Hash.encode_bin (to_hash t) f in
  let decode_bin buf pos_ref =
    { state = Indexed (Hash.decode_bin buf pos_ref) }
  in
  let size_of = Irmin.Type.Size.custom_static Hash.encoded_size in
  Irmin.Type.like (t hash_t) ~pre_hash ~equal ~compare
    ~bin:(encode_bin, decode_bin, size_of)

let v_direct ~hash ~offset ~length = { state = Direct { hash; offset; length } }
let v_indexed hash = { state = Indexed hash }

module type S = sig
  type hash

  include S with type t = hash t and type hash := hash
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
    v_direct ~hash ~offset:null_offset ~length:null_length

  let unfindable_of_hash hash =
    v_direct ~hash ~offset:null_offset ~length:null_length
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
