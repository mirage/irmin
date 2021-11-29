open! Import
include Pack_value_intf

module Kind = struct
  type t =
    | Commit_v0
    | Commit_v1
    | Contents
    | Inode_v0_unstable
    | Inode_v0_stable
    | Inode_v1_root
    | Inode_v1_nonroot

  let to_magic = function
    | Commit_v0 -> 'C'
    | Commit_v1 -> 'D'
    | Contents -> 'B'
    | Inode_v0_unstable -> 'I'
    | Inode_v0_stable -> 'N'
    | Inode_v1_root -> 'R'
    | Inode_v1_nonroot -> 'O'

  let of_magic_exn = function
    | 'C' -> Commit_v0
    | 'D' -> Commit_v1
    | 'B' -> Contents
    | 'I' -> Inode_v0_unstable
    | 'N' -> Inode_v0_stable
    | 'R' -> Inode_v1_root
    | 'O' -> Inode_v1_nonroot
    | c -> Fmt.failwith "Kind.of_magic: unexpected magic char %C" c

  let t = Irmin.Type.(map char) of_magic_exn to_magic
  let pp = Fmt.using to_magic Fmt.char
end

type ('h, 'a) value = { hash : 'h; kind : Kind.t; v : 'a } [@@deriving irmin]

module type S = S with type kind := Kind.t
module type Persistent = Persistent with type kind := Kind.t

let get_dynamic_sizer_exn : type a. a Irmin.Type.t -> string -> int -> int =
 fun typ ->
  match Irmin.Type.(Size.of_encoding typ) with
  | Unknown ->
      Fmt.failwith "Type must have a recoverable encoded length: %a"
        Irmin.Type.pp_ty typ
  | Static n -> fun _ _ -> n
  | Dynamic f -> f

module Of_contents (Conf : sig
  val contents_length_header : [ `Varint | `None ]
end)
(Hash : Irmin.Hash.S) (Key : sig
  type t
end)
(Data : Irmin.Type.S) =
struct
  module Hash = Irmin.Hash.Typed (Hash) (Data)

  type t = Data.t [@@deriving irmin]
  type key = Key.t
  type hash = Hash.t

  let hash = Hash.hash
  let kind = Kind.Contents
  let value = [%typ: (Hash.t, Data.t) value]
  let encode_value = Irmin.Type.(unstage (encode_bin value))
  let decode_value = Irmin.Type.(unstage (decode_bin value))

  let length_header =
    match Conf.contents_length_header with
    | `None -> `Never
    | `Varint -> `Sometimes (Fun.const (Some `Varint))

  let encode_bin ~dict:_ ~offset_of_key:_ hash v =
    encode_value { kind; hash; v }

  let decode_bin ~dict:_ ~key_of_offset:_ ~key_of_hash:_ s off =
    let t = decode_value s off in
    t.v

  let decode_bin_length = get_dynamic_sizer_exn value
  let kind _ = kind
end

module Of_commit
    (Hash : Irmin.Hash.S)
    (Key : T)
    (Commit : Irmin.Commit.Generic_key.S
                with type node_key = Key.t
                 and type commit_key = Key.t) =
struct
  module Hash = Irmin.Hash.Typed (Hash) (Commit)

  type t = Commit.t [@@deriving irmin]
  type key = Key.t
  type hash = Hash.t [@@deriving irmin ~encode_bin ~decode_bin]

  let hash = Hash.hash
  let kind _ = Kind.Commit_v1

  (* A commit implementation that uses integer offsets *)
  module Value_direct = struct
    type t = {
      node_offset : int63;
      parent_offsets : int63 list;
      info : Commit.Info.t;
    }
    [@@deriving irmin ~encode_bin ~to_bin_string ~decode_bin]
  end

  module Entry = struct
    type t = (hash, Commit.t) value [@@deriving irmin ~decode_bin]
  end

  module Entry_direct = struct
    type t = (hash, int32 * Value_direct.t) value
    [@@deriving irmin ~encode_bin ~decode_bin]
  end

  let length_header =
    let some_int32_be = Some `Int32_be in
    `Sometimes
      (function
      (* TODO: avoid duplicating this *)
      | Kind.Inode_v0_unstable -> None
      | Inode_v0_stable -> None
      | Inode_v1_root -> some_int32_be
      | Inode_v1_nonroot -> some_int32_be
      | Commit_v0 -> None
      | Commit_v1 -> some_int32_be
      | Contents -> assert false)

  let encode_bin ~dict:_ ~offset_of_key hash v f =
    let offset_of_key k =
      match offset_of_key k with
      | None -> assert false (* TODO: justify *)
      | Some k -> k
    in
    let v =
      let info = Commit.info v in
      let node_offset = offset_of_key (Commit.node v) in
      let parent_offsets = List.map offset_of_key (Commit.parents v) in
      { Value_direct.node_offset; parent_offsets; info }
    in
    let encoded_commit = Value_direct.to_bin_string v in
    let length_header = Int32.of_int (String.length encoded_commit) in
    Entry_direct.encode_bin { hash; kind = Commit_v1; v = (length_header, v) } f

  let decode_bin ~dict:_ ~key_of_offset ~key_of_hash:_ s off =
    match Kind.of_magic_exn s.[!off + Hash.hash_size] with
    | Commit_v0 -> (Entry.decode_bin s off).v
    | Commit_v1 ->
        let { v = _len, commit; _ } = Entry_direct.decode_bin s off in
        let info = commit.info in
        let node = key_of_offset commit.node_offset in
        let parents = List.map key_of_offset commit.parent_offsets in
        Commit.v ~info ~node ~parents
    | _ -> assert false

  let decode_bin_length =
    let of_v0_entry = get_dynamic_sizer_exn Entry.t in
    let of_v1_entry = get_dynamic_sizer_exn Entry_direct.t in
    fun s off ->
      match Kind.of_magic_exn s.[off + Hash.hash_size] with
      | Commit_v0 -> of_v0_entry s off
      | Commit_v1 -> of_v1_entry s off
      | _ -> assert false
end
