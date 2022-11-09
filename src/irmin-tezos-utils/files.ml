open Import
module Kind = Irmin_pack.Pack_value.Kind

module Varint = struct
  type t = int [@@deriving repr ~decode_bin ~encode_bin]

  (** LEB128 stores 7 bits per byte. An OCaml [int] has at most 63 bits.
      [63 / 7] equals [9]. *)
  let max_encoded_size = 9
end

module Make (Conf : Irmin_pack.Conf.S) (Schema : Irmin.Schema.Extended) = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  module Store = Maker.Make (Schema)
  module Hash = Store.Hash
  module Key = Irmin_pack_unix.Pack_key.Make (Hash)
  module Io = Irmin_pack_unix.Io.Unix
  module Errs = Irmin_pack_unix.Io_errors.Make (Io)
  module Pack_index = Irmin_pack_unix.Index.Make (Hash)
  module Mapping_file = Irmin_pack_unix.Mapping_file.Make (Io)

  module File_manager =
    Irmin_pack_unix.File_manager.Make (Io) (Pack_index) (Mapping_file) (Errs)

  module Dispatcher = Irmin_pack_unix.Dispatcher.Make (File_manager)

  module Inode = struct
    module Value = Schema.Node (Key) (Key)
    include Irmin_pack.Inode.Make_internal (Conf) (Hash) (Key) (Value)

    type compress = Compress.t [@@deriving repr ~decode_bin]
  end

  module Commit = struct
    module Value = struct
      include Schema.Commit (Key) (Key)
      module Info = Schema.Info
    end

    include Irmin_pack.Pack_value.Of_commit (Hash) (Key) (Value)

    type compress = Commit_direct.t [@@deriving repr ~decode_bin]
  end

  type hash = Store.hash [@@deriving repr ~pp]
  type key = Key.t [@@deriving repr ~pp]
  type entry = [ `Contents | `Inode | `Commit ]

  let max_bytes_needed_to_discover_length =
    Hash.hash_size + 1 + Varint.max_encoded_size

  let min_bytes_needed_to_discover_length = Hash.hash_size + 1

  let decode_entry_header buffer =
    let buffer = Bytes.unsafe_to_string buffer in
    let i0 = 0 in

    let imagic = i0 + Hash.hash_size in
    let kind = Kind.of_magic_exn buffer.[imagic] in

    let ilength = i0 + Hash.hash_size + 1 in
    let pos_ref = ref ilength in
    let suffix_length = Varint.decode_bin buffer pos_ref in
    let length_length = !pos_ref - ilength in

    (kind, Hash.hash_size + 1 + length_length + suffix_length)

  let decode_entry_kind buffer =
    let buffer = Bytes.unsafe_to_string buffer in
    let i0 = 0 in
    let imagic = i0 + Hash.hash_size in
    Kind.of_magic_exn buffer.[imagic]

  let decode_entry_len buffer =
    let buffer = Bytes.unsafe_to_string buffer in
    let i0 = 0 in
    let ilength = i0 + Hash.hash_size + 1 in
    let pos_ref = ref ilength in
    let suffix_length = Varint.decode_bin buffer pos_ref in
    let length_length = !pos_ref - ilength in
    Hash.hash_size + 1 + length_length + suffix_length

  let decode_entry dispatcher buffer off =
    let accessor =
      Dispatcher.create_accessor_from_range_exn dispatcher ~off
        ~min_len:min_bytes_needed_to_discover_length
        ~max_len:max_bytes_needed_to_discover_length
    in
    Dispatcher.read_exn dispatcher accessor buffer;
    let kind, len = decode_entry_header buffer in
    let accessor =
      Dispatcher.create_accessor_exn dispatcher ~off ~len:Hash.hash_size
    in
    Dispatcher.read_exn dispatcher accessor buffer;
    let hash = Bytes.sub_string buffer 0 Hash.hash_size in
    let accessor = Dispatcher.create_accessor_exn dispatcher ~off ~len in
    Dispatcher.read_exn dispatcher accessor buffer;
    let content = Bytes.sub_string buffer 0 len in
    (hash, kind, len, content)

  let iter_store fm ~on_entry =
    let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
    let seq =
      Dispatcher.create_sequential_accessor_seq dispatcher
        ~min_header_len:min_bytes_needed_to_discover_length
        ~max_header_len:max_bytes_needed_to_discover_length
        ~read_len:decode_entry_len
    in
    let buffer = Bytes.create (4096 * 4096) in
    let on_entry (off, accessor) =
      Dispatcher.read_exn dispatcher accessor buffer;
      let kind = decode_entry_kind buffer in
      let entry =
        match kind with
        | Inode_v1_unstable | Inode_v1_stable | Commit_v1 -> assert false
        | Dangling_parent_commit | Commit_v2 -> `Commit
        | Contents -> `Contents
        | Inode_v2_root | Inode_v2_nonroot -> `Inode
      in
      on_entry off entry
    in
    Seq.iter on_entry seq

  let rec traverse_dict dict size buffer off acc =
    if off < size then (
      File_manager.Dict.read_exn dict buffer ~off ~len:4;
      let len = Int32.to_int @@ Bytes.get_int32_be buffer 0 in
      let off = Int63.(add off (of_int 4)) in
      File_manager.Dict.read_exn dict buffer ~off ~len;
      let str = Bytes.sub_string buffer 0 len in
      let acc = str :: acc in
      let off = Int63.(add off (of_int len)) in
      traverse_dict dict size buffer off acc)
    else acc

  let load_dict (dict : File_manager.Dict.t) buffer =
    let max_offset = File_manager.Dict.end_poff dict in
    let off = Int63.zero in
    let dict = traverse_dict dict max_offset buffer off [] in
    List.rev dict
end
