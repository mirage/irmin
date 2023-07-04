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
include Pack_value_intf

module Kind = struct
  type t =
    | Commit_v1
    | Commit_v2
    | Contents
    | Inode_v1_unstable
    | Inode_v1_stable
    | Inode_v2_root
    | Inode_v2_nonroot
    | Dangling_parent_commit

  let to_magic = function
    | Commit_v1 -> 'C'
    | Commit_v2 -> 'D'
    | Contents -> 'B'
    | Inode_v1_unstable -> 'I'
    | Inode_v1_stable -> 'N'
    | Inode_v2_root -> 'R'
    | Inode_v2_nonroot -> 'O'
    | Dangling_parent_commit -> 'P'

  let of_magic_exn = function
    | 'C' -> Commit_v1
    | 'D' -> Commit_v2
    | 'B' -> Contents
    | 'I' -> Inode_v1_unstable
    | 'N' -> Inode_v1_stable
    | 'R' -> Inode_v2_root
    | 'O' -> Inode_v2_nonroot
    | 'P' -> Dangling_parent_commit
    | c -> Fmt.failwith "Kind.of_magic: unexpected magic char %C" c

  let all =
    [
      Commit_v1;
      Commit_v2;
      Contents;
      Inode_v1_unstable;
      Inode_v1_stable;
      Inode_v2_root;
      Inode_v2_nonroot;
      Dangling_parent_commit;
    ]

  let to_enum = function
    | Commit_v1 -> 0
    | Commit_v2 -> 1
    | Contents -> 2
    | Inode_v1_unstable -> 3
    | Inode_v1_stable -> 4
    | Inode_v2_root -> 5
    | Inode_v2_nonroot -> 6
    | Dangling_parent_commit -> 7

  let pp =
    Fmt.of_to_string (function
      | Commit_v1 -> "Commit_v1"
      | Commit_v2 -> "Commit_v2"
      | Contents -> "Contents"
      | Inode_v1_unstable -> "Inode_v1_unstable"
      | Inode_v1_stable -> "Inode_v1_stable"
      | Inode_v2_root -> "Inode_v2_root"
      | Inode_v2_nonroot -> "Inode_v2_nonroot"
      | Dangling_parent_commit -> "Dangling_parent_commit")

  let length_header_exn : t -> length_header =
    let some_varint = Some `Varint in
    function
    | Commit_v1 | Inode_v1_unstable | Inode_v1_stable -> None
    | Commit_v2 | Inode_v2_root | Inode_v2_nonroot | Dangling_parent_commit ->
        some_varint
    | Contents ->
        Fmt.failwith
          "Can't determine length header for user-defined codec Contents"

  let t = Irmin.Type.map ~pp Irmin.Type.char of_magic_exn to_magic
end

type ('h, 'a) value = { hash : 'h; kind : Kind.t; v : 'a } [@@deriving irmin]

module type S = S with type kind := Kind.t

let get_dynamic_sizer_exn : type a. a Irmin.Type.t -> string -> int -> int =
 fun typ ->
  match Irmin.Type.(Size.of_encoding typ) with
  | Unknown ->
      Fmt.failwith "Type must have a recoverable encoded length: %a"
        Irmin.Type.pp_ty typ
  | Static n -> fun _ _ -> n
  | Dynamic f -> f

module Of_contents
    (Conf : Config)
    (Hash : Irmin.Hash.S)
    (Key : T)
    (Data : Irmin.Type.S) =
struct
  module Hash = Irmin.Hash.Typed (Hash) (Data)

  type t = Data.t [@@deriving irmin ~size_of]
  type key = Key.t
  type hash = Hash.t
  type kinded += Contents of t

  let to_kinded t = Contents t
  let of_kinded = function Contents c -> c | _ -> assert false
  let hash = Hash.hash
  let kind = Kind.Contents
  let length_header = Fun.const Conf.contents_length_header
  let value = [%typ: (Hash.t, Data.t) value]
  let encode_value = Irmin.Type.(unstage (encode_bin value))
  let decode_value = Irmin.Type.(unstage (decode_bin value))

  let encode_bin ~dict:_ ~offset_of_key:_ hash v f =
    encode_value { kind; hash; v } f

  let decode_bin ~dict:_ ~key_of_offset:_ ~key_of_hash:_ s off =
    let t = decode_value s off in
    t.v

  let decode_bin_length = get_dynamic_sizer_exn value
  let kind _ = kind

  let weight =
    let size = Mem.repr_size t in
    fun v -> Immediate (size v)
end

module Of_commit
    (Hash : Irmin.Hash.S)
    (Key : Irmin.Key.S with type hash = Hash.t)
    (Commit : Irmin.Commit.Generic_key.S
                with type node_key = Key.t
                 and type commit_key = Key.t) =
struct
  module Hash = Irmin.Hash.Typed (Hash) (Commit)

  type t = Commit.t [@@deriving irmin]
  type key = Key.t
  type hash = Hash.t [@@deriving irmin ~encode_bin ~decode_bin]
  type kinded += Commit of t

  let to_kinded t = Commit t
  let of_kinded = function Commit c -> c | _ -> assert false
  let hash = Hash.hash
  let kind _ = Kind.Commit_v2

  let weight =
    let size = Mem.repr_size t in
    fun v -> Deferred (fun () -> size v)

  (* A commit implementation that uses integer offsets for addresses where possible. *)
  module Commit_direct = struct
    type address = Offset of int63 | Hash of Hash.t [@@deriving irmin]

    type t = {
      node_offset : address;
      parent_offsets : address list;
      info : Commit.Info.t;
    }
    [@@deriving irmin ~encode_bin ~to_bin_string ~decode_bin]

    let size_of =
      match Irmin.Type.Size.of_value t with
      | Dynamic f -> f
      | Static _ | Unknown -> assert false
  end

  module Entry = struct
    module V0 = struct
      type t = (hash, Commit.t) value [@@deriving irmin ~decode_bin]
    end

    module V1 = struct
      type data = { length : int; v : Commit_direct.t } [@@deriving irmin]
      type t = (hash, data) value [@@deriving irmin ~encode_bin ~decode_bin]
    end
  end

  let length_header = function
    | Kind.Contents -> assert false
    | x -> Kind.length_header_exn x

  let encode_bin ~dict:_ ~offset_of_key hash v f =
    let address_of_key k : Commit_direct.address =
      match offset_of_key k with
      | None -> Hash (Key.to_hash k)
      | Some k -> Offset k
    in
    let v =
      let info = Commit.info v in
      let node_offset = address_of_key (Commit.node v) in
      let parent_offsets = List.map address_of_key (Commit.parents v) in
      { Commit_direct.node_offset; parent_offsets; info }
    in
    let length = Commit_direct.size_of v in
    Entry.V1.encode_bin { hash; kind = Commit_v2; v = { length; v } } f

  let decode_bin ~dict:_ ~key_of_offset ~key_of_hash s off =
    let key_of_address : Commit_direct.address -> Key.t = function
      | Offset x -> key_of_offset x
      | Hash x -> key_of_hash x
    in
    match Kind.of_magic_exn s.[!off + Hash.hash_size] with
    | Commit_v1 -> (Entry.V0.decode_bin s off).v
    | Commit_v2 | Dangling_parent_commit ->
        let { v = { Entry.V1.v = commit; _ }; _ } = Entry.V1.decode_bin s off in
        let info = commit.info in
        let node = key_of_address commit.node_offset in
        let parents = List.map key_of_address commit.parent_offsets in
        Commit.v ~info ~node ~parents
    | _ -> assert false

  let decode_bin_length =
    let of_v0_entry = get_dynamic_sizer_exn Entry.V0.t
    and of_v1_entry = get_dynamic_sizer_exn Entry.V1.t in
    fun s off ->
      match Kind.of_magic_exn s.[off + Hash.hash_size] with
      | Commit_v1 -> of_v0_entry s off
      | Commit_v2 -> of_v1_entry s off
      | _ -> assert false
end
