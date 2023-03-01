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

  module File_manager =
    Irmin_pack_unix.File_manager.Make (Io) (Pack_index) (Errs)

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
    let _ =
      Dispatcher.read_range_exn dispatcher ~off
        ~min_len:min_bytes_needed_to_discover_length
        ~max_len:max_bytes_needed_to_discover_length buffer
    in
    let kind, len = decode_entry_header buffer in
    let _ = Dispatcher.read_exn dispatcher ~off ~len:Hash.hash_size buffer in
    let hash = Bytes.sub_string buffer 0 Hash.hash_size in
    let _ = Dispatcher.read_exn dispatcher ~off ~len buffer in
    let content = Bytes.sub_string buffer 0 len in
    (hash, kind, len, content)

  let guess_entry_len dispatcher ~off =
    let min_bytes_needed_to_discover_length = Hash.hash_size + 1 in
    let max_bytes_needed_to_discover_length =
      min_bytes_needed_to_discover_length + Varint.max_encoded_size
    in
    let buffer = Bytes.create max_bytes_needed_to_discover_length in
    let _ =
      Dispatcher.read_range_exn dispatcher ~off
        ~min_len:min_bytes_needed_to_discover_length
        ~max_len:max_bytes_needed_to_discover_length buffer
    in
    decode_entry_len buffer

  let iter_store fm ~on_entry =
    let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
    let buffer = Bytes.create (4096 * 4096) in
    let on_entry ~off ~len =
      let _ = Dispatcher.read_exn dispatcher ~off ~len buffer in
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
    let rec traverse off =
      match Dispatcher.next_valid_offset dispatcher ~off with
      | None -> ()
      | Some off ->
          let len = guess_entry_len dispatcher ~off in
          on_entry ~off ~len;
          let next_off = Int63.add off (Int63.of_int len) in
          traverse next_off
    in
    traverse Int63.zero

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
