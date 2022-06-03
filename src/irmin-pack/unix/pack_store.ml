open Import
include Pack_store_intf

module Varint = struct
  type t = int [@@deriving irmin ~decode_bin]

  (** LEB128 stores 7 bits per byte. An OCaml [int] has at most 63 bits.
      [63 / 7] equals [9]. *)
  let max_encoded_size = 9
end

exception Invalid_read of string
exception Corrupted_store of string

let invalid_read fmt = Fmt.kstr (fun s -> raise (Invalid_read s)) fmt
let corrupted_store fmt = Fmt.kstr (fun s -> raise (Corrupted_store s)) fmt

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

module Make
    (File_manager : File_manager.S)
    (* (Index : Pack_index.S) *)
    (Hash : Irmin.Hash.S with type t = Index.key)
    (Val : Pack_value.Persistent
             with type hash := Hash.t
              and type key := Hash.t Pack_key.t) =
struct
  module Io_legacy = Io_legacy.Unix
  module Tbl = Table (Hash)
  module Dict = Pack_dict
  module Key = Pack_key.Make (Hash)

  module Lru = Irmin.Backend.Lru.Make (struct
    include Hash

    let hash = Hash.short_hash
    let equal = Irmin.Type.(unstage (equal Hash.t))
  end)

  type 'a t = {
    lru : Val.t Lru.t;
    staging : Val.t Tbl.t;
    readonly : bool;
    io : Io_legacy.t;
    index : Index.t;
    indexing_strategy : Irmin_pack.Indexing_strategy.t;
    dict : Dict.t;
  }

  type hash = Hash.t [@@deriving irmin ~pp ~equal ~decode_bin]
  type key = Key.t [@@deriving irmin ~pp]
  type value = Val.t [@@deriving irmin ~pp]

  let index_direct_with_kind t hash =
    [%log.debug "index %a" pp_hash hash];
    match Index.find t.index hash with
    | None -> None
    | Some (offset, length, kind) ->
        let key = Pack_key.v_direct ~hash ~offset ~length in
        Some (key, kind)

  let index_direct t hash =
    index_direct_with_kind t hash |> Option.map (fun (key, _) -> key)

  let index t hash = Lwt.return (index_direct t hash)

  let flush ?(index = true) ?(index_merge = false) t =
    if index_merge then Index.merge t.index;
    Dict.flush t.dict;
    Io_legacy.flush t.io;
    if index then Index.flush t.index;
    Tbl.clear t.staging

  let v ~readonly ~lru_size ~index ~indexing_strategy ~dict ~io =
    let staging = Tbl.create 127 in
    let lru = Lru.create lru_size in
    { lru; staging; readonly; io; index; indexing_strategy; dict } |> Lwt.return

  let io_read_and_decode_hash ~off t =
    let buf = Bytes.create Hash.hash_size in
    let n = Io_legacy.read t.io ~off buf in
    assert (n = Hash.hash_size);
    decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0)

  type span = { offset : int63; length : int }
  (** The type of contiguous ranges of bytes in the pack file. *)

  (** Refer to the index for the position of a pack entry, assuming it is
      indexed: *)
  let get_entry_span_from_index_exn t hash : span =
    match index_direct t hash with
    | Some key' -> (
        match Pack_key.inspect key' with
        | Direct { offset; length; _ } -> { offset; length }
        | Indexed _ ->
            (* [index_direct] returns only [Direct] keys. *)
            assert false)
    | None ->
        corrupted_store "Unexpected object %a missing from index" pp_hash hash

  module Entry_prefix = struct
    type t = {
      hash : hash;
      kind : Pack_value.Kind.t;
      size_of_value_and_length_header : int option;
          (** Remaining bytes in the entry after reading the hash and the kind
              (i.e. the length of the length header + the value of the length
              header), if the entry has a length header (otherwise [None]).

              NOTE: the length stored in the index and in direct pack keys is
              the {!total_entry_length} (including the hash and the kind). See
              [pack_value.mli] for a description. *)
    }
    [@@deriving irmin ~pp_dump]

    let min_length = Hash.hash_size + 1
    let max_length = Hash.hash_size + 1 + Varint.max_encoded_size

    let total_entry_length t =
      Option.map (fun len -> min_length + len) t.size_of_value_and_length_header
  end

  let read_and_decode_entry_prefix ~off ~io_read =
    let buf = Bytes.create Entry_prefix.max_length in
    let bytes_read = io_read ~off buf in
    (* We may read fewer then [Entry_prefix.max_length] bytes when reading the
       final entry in the pack file (if the data section of the entry is
       shorter than [Varint.max_encoded_size]. In this case, an invalid read
       may be discovered below when attempting to decode the length header. *)
    if bytes_read < Entry_prefix.min_length then
      invalid_read
        "Attempted to read an entry at offset %a in the pack file, but got \
         only %d bytes"
        Int63.pp off bytes_read;
    let hash = decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0) in
    let kind = Pack_value.Kind.of_magic_exn (Bytes.get buf Hash.hash_size) in
    let size_of_value_and_length_header =
      match Val.length_header kind with
      | None -> None
      | Some `Varint ->
          let length_header_start = Entry_prefix.min_length in
          (* The bytes starting at [length_header_start] are a
             variable-length length field (if they exist / were read
             correctly): *)
          let pos_ref = ref length_header_start in
          let length_header =
            Varint.decode_bin (Bytes.unsafe_to_string buf) pos_ref
          in
          let length_header_length = !pos_ref - length_header_start in
          Some (length_header_length + length_header)
    in
    { Entry_prefix.hash; kind; size_of_value_and_length_header }

  let io_read_and_decode_entry_prefix ~off t =
    let io_read = Io_legacy.read t.io in
    read_and_decode_entry_prefix ~off ~io_read

  let pack_file_contains_key t k =
    let key = Pack_key.inspect k in
    match key with
    | Indexed hash -> Index.mem t.index hash
    | Direct { offset; _ } ->
        let io_offset = Io_legacy.offset t.io in
        let minimal_entry_length = Entry_prefix.min_length in
        if
          Int63.compare
            (Int63.add offset (Int63.of_int minimal_entry_length))
            io_offset
          > 0
        then (
          (* Can't fit an entry into this suffix of the store, so this key
             isn't (yet) valid. If we're a read-only instance, the key may
             become valid on [sync]; otherwise we know that this key wasn't
             constructed for this store. *)
          if not t.readonly then
            invalid_read
              "invalid key %a checked for membership (IO offset = %a)" pp_key k
              Int63.pp io_offset;
          false)
        else
          (* Read the hash explicitly as an integrity check: *)
          let hash = io_read_and_decode_hash ~off:offset t in
          let expected_hash = Key.to_hash k in
          if not (equal_hash hash expected_hash) then
            invalid_read
              "invalid key %a checked for membership (read hash %a at this \
               offset instead)"
              pp_key k pp_hash hash;
          (* At this point we consider the key to be contained in the pack
             file. However, we could also be in the presence of a forged (or
             unlucky) key that points to an offset that mimics a real pack
             entry (e.g. in the middle of a blob). *)
          true

  let unsafe_mem t k =
    [%log.debug "[pack] mem %a" pp_key k];
    Tbl.mem t.staging (Key.to_hash k)
    || Lru.mem t.lru (Key.to_hash k)
    || pack_file_contains_key t k

  let mem t k =
    let b = unsafe_mem t k in
    Lwt.return b

  let check_hash h v =
    let h' = Val.hash v in
    if equal_hash h h' then Ok () else Error (h, h')

  let check_key k v = check_hash (Key.to_hash k) v

  let io_read_and_decode ~off ~len t =
    let () =
      if not (Io_legacy.readonly t.io) then
        let io_offset = Io_legacy.offset t.io in
        if Int63.add off (Int63.of_int len) > io_offset then
          (* This is likely a store corruption. We raise [Invalid_read]
             specifically so that [integrity_check] below can handle it. *)
          invalid_read
            "Got request to read %d bytes (at offset %a), but max IO offset is \
             %a"
            len Int63.pp off Int63.pp io_offset
    in
    let buf = Bytes.create len in
    let n = Io_legacy.read t.io ~off buf in
    if n <> len then
      invalid_read "Read %d bytes (at offset %a) but expected %d" n Int63.pp off
        len;
    let key_of_offset offset =
      [%log.debug "key_of_offset: %a" Int63.pp offset];
      (* Attempt to eagerly read the length at the same time as reading the
         hash in order to save an extra IO read when dereferencing the key: *)
      let entry_prefix = io_read_and_decode_entry_prefix ~off:offset t in
      match Entry_prefix.total_entry_length entry_prefix with
      | Some length -> Pack_key.v_direct ~hash:entry_prefix.hash ~offset ~length
      | None ->
          (* NOTE: we could store [offset] in this key, but since we know the
             entry doesn't have a length header we'll need to check the index
             when dereferencing this key anyway. {i Not} storing the offset
             avoids doing another failed check in the pack file for the length
             header during [find]. *)
          Pack_key.v_indexed entry_prefix.hash
    in
    let key_of_hash = Pack_key.v_indexed in
    let dict = Dict.find t.dict in
    Val.decode_bin ~key_of_offset ~key_of_hash ~dict
      (Bytes.unsafe_to_string buf)
      (ref 0)

  let pp_io ppf t =
    let name = Filename.basename (Filename.dirname (Io_legacy.name t.io)) in
    let mode = if t.readonly then ":RO" else "" in
    Fmt.pf ppf "%s%s" name mode

  let find_in_pack_file ~check_integrity t key hash =
    let loc, { offset; length } =
      match Pack_key.inspect key with
      | Direct { offset; length; _ } ->
          (Stats.Pack_store.Pack_direct, { offset; length })
      | Indexed hash ->
          let entry_span = get_entry_span_from_index_exn t hash in
          (* Cache the offset and length information in the existing key: *)
          Pack_key.promote_exn key ~offset:entry_span.offset
            ~length:entry_span.length;
          (Stats.Pack_store.Pack_indexed, entry_span)
    in
    let io_offset = Io_legacy.offset t.io in
    if Int63.add offset (Int63.of_int length) > io_offset then (
      (* Can't fit an entry into this suffix of the store, so this key
         isn't (yet) valid. If we're a read-only instance, the key may
         become valid on [sync]; otherwise we know that this key wasn't
         constructed for this store. *)
      match t.readonly with
      | false ->
          invalid_read "attempt to dereference invalid key %a (IO offset = %a)"
            pp_key key Int63.pp io_offset
      | true ->
          [%log.debug
            "Direct store key references an unknown starting offset %a (length \
             = %d, IO offset = %a)"
            Int63.pp offset length Int63.pp io_offset];
          (Stats.Pack_store.Not_found, None))
    else
      let v = io_read_and_decode ~off:offset ~len:length t in
      Lru.add t.lru hash v;
      (if check_integrity then
       check_key key v |> function
       | Ok () -> ()
       | Error (expected, got) ->
           corrupted_store "Got hash %a, expecting %a (for val: %a)." pp_hash
             got pp_hash expected pp_value v);
      (loc, Some v)

  let unsafe_find ~check_integrity t k =
    [%log.debug "[pack:%a] find %a" pp_io t pp_key k];
    let hash = Key.to_hash k in
    let location, value =
      match Tbl.find t.staging hash with
      | v ->
          Lru.add t.lru hash v;
          (Stats.Pack_store.Staging, Some v)
      | exception Not_found -> (
          match Lru.find t.lru hash with
          | v -> (Stats.Pack_store.Lru, Some v)
          | exception Not_found -> find_in_pack_file ~check_integrity t k hash)
    in
    Stats.report_pack_store ~field:location;
    value

  let find t k =
    let v = unsafe_find ~check_integrity:true t k in
    Lwt.return v

  let cast t = (t :> read_write t)

  let integrity_check ~offset ~length hash t =
    try
      let value = io_read_and_decode ~off:offset ~len:length t in
      match check_hash hash value with
      | Ok () -> Ok ()
      | Error _ -> Error `Wrong_hash
    with Invalid_read _ -> Error `Absent_value

  let batch t f =
    let* r = f (cast t) in
    if Tbl.length t.staging = 0 then Lwt.return r
    else (
      flush t;
      Lwt.return r)

  let auto_flush = 1024

  let unsafe_append ~ensure_unique ~overcommit t hash v =
    let unguarded_append () =
      [%log.debug "[pack] append %a" pp_hash hash];
      let offset_of_key k =
        match Pack_key.inspect k with
        | Direct { offset; _ } ->
            Stats.incr_appended_offsets ();
            Some offset
        | Indexed hash -> (
            match Index.find t.index hash with
            | None ->
                Stats.incr_appended_hashes ();
                None
            | Some (offset, _, _) ->
                Stats.incr_appended_offsets ();
                Some offset)
      in
      let kind = Val.kind v in
      let () =
        (* Bump the pack file version header if necessary *)
        let value_version = Pack_value.Kind.version kind
        and io_version = Io_legacy.version t.io in
        if Version.compare value_version io_version > 0 then
          Io_legacy.set_version t.io value_version
      in
      let dict = Dict.index t.dict in
      let off = Io_legacy.offset t.io in
      Val.encode_bin ~offset_of_key ~dict hash v (Io_legacy.append t.io);
      let len = Int63.to_int (Io_legacy.offset t.io -- off) in
      let key = Pack_key.v_direct ~hash ~offset:off ~length:len in
      let () =
        let kind = Val.kind v in
        let should_index = t.indexing_strategy ~value_length:len kind in
        if should_index then Index.add ~overcommit t.index hash (off, len, kind)
      in
      if Tbl.length t.staging >= auto_flush then flush t
      else Tbl.add t.staging hash v;
      Lru.add t.lru hash v;
      [%log.debug "[pack] append done %a <- %a" pp_hash hash pp_key key];
      key
    in
    match ensure_unique with
    | false -> unguarded_append ()
    | true -> (
        match index_direct t hash with
        | None -> unguarded_append ()
        | Some key -> key)

  let unsafe_add t hash v =
    unsafe_append ~ensure_unique:true ~overcommit:false t hash v |> Lwt.return

  let add t v = unsafe_add t (Val.hash v) v

  let unsafe_close t =
    Tbl.clear t.staging;
    Lru.clear t.lru

  let close t =
    unsafe_close t;
    Lwt.return_unit

  let sync t =
    let former_offset = Io_legacy.offset t.io in
    let offset = Io_legacy.force_offset t.io in
    if offset > former_offset then (
      Dict.sync t.dict;
      Index.sync t.index)

  let offset t = Io_legacy.offset t.io
end

(* module Make
 *     (Index : Pack_index.S)
 *     (Hash : Irmin.Hash.S with type t = Index.key)
 *     (Val : Pack_value.Persistent
 *              with type hash := Hash.t
 *               and type key := Hash.t Pack_key.t) =
 * struct
 *   module Inner = Make_without_close_checks (Index) (Hash) (Val)
 *   include Indexable.Closeable (Inner)
 *
 *   let v ~readonly ~lru_size ~index ~indexing_strategy ~dict ~io =
 *     Inner.v ~readonly ~lru_size ~index ~indexing_strategy ~dict ~io
 *     >|= make_closeable
 *
 *   let sync t = Inner.sync (get_open_exn t)
 *
 *   let flush ?index ?index_merge t =
 *     Inner.flush ?index ?index_merge (get_open_exn t)
 *
 *   let offset t = Inner.offset (get_open_exn t)
 *
 *   let integrity_check ~offset ~length k t =
 *     Inner.integrity_check ~offset ~length k (get_open_exn t)
 *
 *   module Entry_prefix = Inner.Entry_prefix
 *
 *   let read_and_decode_entry_prefix = Inner.read_and_decode_entry_prefix
 *   let index_direct_with_kind t = Inner.index_direct_with_kind (get_open_exn t)
 * end *)
