open! Import
include Pack_key_intf

type 'hash t = {
  hash : 'hash;
  offset : int63;
  length : int63;
      (* XXX: should probably be [int], but kept as [int63] now for the fixed-width encoding *)
}
[@@deriving irmin]

let t hash_t =
  (* The pre-hash image of a key is just the hash of the corresponding value.

     NOTE: it's particularly important that we discard the file offset when
     computing hashes of structured values (e.g. inodes), so that this hashing
     process is reproducible in different stores (w/ different offsets for the
     values). *)
  let equal =
    let hash_equal = Irmin.Type.(unstage (equal hash_t)) in
    fun a b ->
      let result = hash_equal a.hash b.hash in
      assert ((not result) || Int63.equal a.length b.length);
      result
  in
  let compare =
    let hash_compare = Irmin.Type.(unstage (compare hash_t)) in
    fun a b ->
      match hash_compare a.hash b.hash with
      | 0 -> Int63.compare a.length b.length
      | r -> r
  in
  let pre_hash =
    let f = Irmin.Type.(unstage (pre_hash hash_t)) in
    fun t -> f t.hash
  in
  Irmin.Type.like (t hash_t) ~pre_hash ~equal ~compare

let v ~hash ~offset ~length = { hash; offset; length = Int63.of_int length }

module type S = sig
  type hash

  include S with type t = hash t and type hash := hash
end

module Make (Hash : Irmin.Hash.S) = struct
  type nonrec t = Hash.t t [@@deriving irmin]
  type hash = Hash.t [@@deriving irmin ~of_bin_string]

  let to_hash t = t.hash
  let to_offset t = t.offset
  let to_length t = Int63.to_int t.length

  let null =
    let buf = String.make Hash.hash_size '\000' in
    let hash =
      match hash_of_bin_string buf with Ok x -> x | Error _ -> assert false
    in
    { hash; offset = Int63.minus_one; length = Int63.minus_one }

  let unfindable_of_hash hash = { null with hash }
end

module type Store_spec = sig
  type ('h, _) contents_key = 'h t
  type 'h node_key = 'h t
  type 'h commit_key = 'h t
end

module rec Store_spec : Store_spec = Store_spec
