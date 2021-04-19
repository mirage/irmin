open! Import
include Pack_key_intf

(* XXX: support "v1" keys that are just hashes *)
type 'hash t = {
  hash : 'hash;
  (* The offset and length of the corresponding value in the pack file: *)
  offset : int63;
  length : int;
}
[@@deriving irmin]

let t hash_t =
  (* The pre-hash image of a key is just the hash of the corresponding value.

     NOTE: it's particularly important that we discard the file offset when
     computing hashes of structured values (e.g. inodes), so that this hashing
     process is reproducible in different stores (w/ different offsets for the
     values). *)
  let pre_hash =
    let f = Irmin.Type.(unstage (pre_hash hash_t)) in
    Irmin.Type.stage (fun t -> f t.hash)
  in
  Irmin.Type.like (t hash_t) ~pre_hash

let v ~hash ~offset ~length = { hash; offset; length }

module type S = sig
  type hash

  include S with type t = hash t and type hash := hash
end

module Make (Hash : Irmin.Hash.S) = struct
  type nonrec t = Hash.t t [@@deriving irmin]
  type hash = Hash.t [@@deriving irmin ~of_bin_string]

  let to_hash t = t.hash
  let to_offset t = t.offset
  let to_length t = t.length

  let null =
    let buf = String.make Hash.hash_size '\000' in
    let hash =
      match hash_of_bin_string buf with Ok x -> x | Error _ -> assert false
    in
    { hash; offset = Int63.minus_one; length = -1 }

  let unfindable_of_hash hash = { null with hash }
end
