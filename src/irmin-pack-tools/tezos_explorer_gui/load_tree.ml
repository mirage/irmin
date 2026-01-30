open Optint
module Kind = Irmin_pack.Pack_value.Kind

module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end

module Content = Irmin.Contents.String
module Schema = Irmin.Schema.KV (Content)
module Maker = Irmin_pack_unix.Maker (Conf)
module Store = Maker.Make (Schema)
module Hash = Store.Hash
module Key = Irmin_pack_unix.Pack_key.Make (Hash)
module Io = Irmin_pack_unix.Io.Unix
module Errs = Irmin_pack_unix.Io_errors.Make (Io)
module Pack_index = Irmin_pack_unix.Index.Make (Hash)
module File_manager = Irmin_pack_unix.File_manager.Make (Io) (Pack_index) (Errs)
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

module Varint = struct
  type t = int [@@deriving repr ~decode_bin ~encode_bin]

  (** LEB128 stores 7 bits per byte. An OCaml [int] has at most 63 bits.
      [63 / 7] equals [9]. *)
  let max_encoded_size = 9
end

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

exception Commit of string * Int63.t

let buffer = Bytes.create (4096 * 4096)

let get_entry dispatcher off =
  let _, _, _, contents = decode_entry dispatcher buffer off in
  contents

open Tree

let load_inode dispatcher addr =
  let contents = get_entry dispatcher addr in
  contents

let load_commit dispatcher addr =
  let contents = get_entry dispatcher addr in
  let entry_header = Hash.hash_size + 2 in
  let contents_len = String.length contents - entry_header in
  let contents = String.sub contents entry_header contents_len in
  contents

let get_name dict (n : Inode.Compress.name) =
  match n with
  | Indirect dict_key ->
      let key = File_manager.Dict.find dict dict_key in
      Option.get key
  | Direct step -> step

let get_tree_from_commit (loading : Loading.t) dispatcher dict max_depth
    last_commit_off commit_hash =
  let rec get_commit_tree depth (commit : Commit.Commit_direct.t) =
    loading.max.commits <- loading.max.commits + 1;
    Loading.update loading;
    if depth <> max_depth then
      match commit.node_offset with
      | Offset addr ->
          let current = addr > last_commit_off in
          let contents = load_inode dispatcher addr in
          let inode = Inode.decode_bin_compress contents (ref 0) in
          {
            depth;
            path = commit_hash;
            obj = Commit (Some (get_node_tree (depth + 1) "root" inode current));
            current = true;
          }
      | Hash _hash -> assert false
    else { depth; path = commit_hash; obj = Commit None; current = true }
  and get_node_tree depth name (inode : Inode.compress) current =
    let addr_show name (addr : Inode.Compress.address) =
      match addr with
      | Offset addr ->
          let current = addr > last_commit_off in
          let contents = load_inode dispatcher addr in
          let inode = Inode.decode_bin_compress contents (ref 0) in
          get_node_tree (depth + 1) name inode current
      | Hash _hash -> assert false
    in
    let value (v : Inode.Compress.value) =
      match v with
      | Contents (n, addr, ()) -> (
          match addr with
          | Offset addr ->
              let current = addr > last_commit_off in
              loading.max.entries <- loading.max.entries + 1;
              Loading.update loading;
              { depth = depth + 1; path = get_name dict n; obj = Leaf; current }
          | Hash _hash -> assert false)
      | Node (n, addr) -> addr_show (get_name dict n) addr
    in
    let ptr (p : Inode.Compress.ptr) =
      addr_show (string_of_int p.index) p.hash
    in
    let tree (t : Inode.Compress.tree) = List.map ptr t.entries in
    let v (tv : Inode.Compress.v) =
      if depth <> max_depth && current then
        match tv with
        | Values l -> Tree.Values (Some (List.map value l))
        | Tree t -> Tree (Some (tree t))
      else match tv with Values _ -> Tree.Values None | Tree _ -> Tree None
    in
    let l =
      match inode.tv with
      | V1_stable tv | V1_unstable tv -> v tv
      | V2_root tv | V2_nonroot tv -> v tv.v
    in
    loading.max.inodes <- loading.max.inodes + 1;
    Loading.update loading;
    { depth; path = name; obj = Inode l; current }
  in
  get_commit_tree 0

let load_tree sw fs loading store_path ~max_depth (hash, off) last_commit_off =
  Loading.set_state loading Load_tree;
  let conf = Irmin_pack.Conf.init ~sw ~fs store_path in
  let fm = Errs.raise_if_error @@ File_manager.open_ro conf in
  let dispatcher = Dispatcher.v fm |> Errs.raise_if_error in
  let dict = File_manager.dict fm in
  let contents = load_commit dispatcher off in
  let commit = Commit.decode_bin_compress contents (ref 0) in
  get_tree_from_commit loading dispatcher dict max_depth last_commit_off hash
    commit

let load_index store_path =
  let index = Pack_index.v_exn ~readonly:true ~log_size:500_000 store_path in
  let l = ref [] in
  Pack_index.iter
    (fun h (off, _, k) ->
      if k = Commit_v1 || k = Commit_v2 then
        l := (string_of_int @@ Hash.short_hash h, off) :: !l)
    index;
  let cmp (_, off1) (_, off2) = Int63.(to_int @@ sub off1 off2) in
  List.sort cmp !l
