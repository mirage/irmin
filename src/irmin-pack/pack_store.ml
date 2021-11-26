open! Import
include Pack_store_intf

module Indexing_strategy = struct
  type t = value_length:int -> Pack_value.Kind.t -> bool

  let always ~value_length:_ _ = true

  let minimal : t =
   fun ~value_length:_ -> function
    | Commit_v1 ->
        (* Commits must be indexed as the branch store contains only their
           hashes. All {i internal} references to V1 commits are via offset
           (from other V1 commit objects). *)
        true
    | Inode_v1_root ->
        (* It's safe not to index V1 root inodes because they are never
           referenced by V0 commit objects (only V1 commit objects, which
           contain direct pointers rather than hashes).*)
        false
    | Inode_v1_nonroot -> false
    | Contents -> false
    | Commit_v0 | Inode_v0_unstable | Inode_v0_stable ->
        (* We never append new V0 values, so this choice is irrelevant to the
           store implementation, but we do assume that existing V0 objects are
           indexed (as they may be referenced via hash by other V0 objects), and
           this must be accounted for when reconstructing the index. *)
        true
end

module type S = S with type indexing_strategy := Indexing_strategy.t
module type Maker = Maker with type indexing_strategy := Indexing_strategy.t

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

exception Invalid_read of string
exception Corrupted_store of string

let invalid_read fmt = Fmt.kstr (fun s -> raise (Invalid_read s)) fmt
let corrupted_store fmt = Fmt.kstr (fun s -> raise (Corrupted_store s)) fmt

module Varint = struct
  type t = int [@@deriving irmin ~decode_bin]

  let max_encoded_size = 9
end

module Maker
    (V : Version.S)
    (Index : Pack_index.S)
    (K : Irmin.Hash.S with type t = Index.key) :
  Maker with type hash = K.t and type index := Index.t = struct
  module IO_cache = IO.Cache
  module IO = IO.Unix
  module Tbl = Table (K)
  module Dict = Pack_dict.Make (V)

  module Hash = struct
    include K

    let hash = K.short_hash
    let equal = Irmin.Type.(unstage (equal K.t))
  end

  module Key = Pack_key.Make (K)

  type hash = K.t

  type 'a t = {
    mutable block : IO.t;
    index : Index.t;
    indexing_strategy : Indexing_strategy.t;
    dict : Dict.t;
    mutable open_instances : int;
  }

  let clear ?keep_generation t =
    if IO.offset t.block <> Int63.zero then (
      Index.clear t.index;
      match V.version with
      | `V1 -> IO.truncate t.block
      | `V2 ->
          IO.clear ?keep_generation t.block;
          Dict.clear t.dict)

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~index ~indexing_strategy ~fresh ~readonly file =
    let root = Filename.dirname file in
    let dict = Dict.v ~fresh ~readonly root in
    let block = IO.v ~version:(Some V.version) ~fresh ~readonly file in
    { block; index; indexing_strategy; dict; open_instances = 1 }

  let IO_cache.{ v } =
    IO_cache.memoize ~clear ~valid
      ~v:(fun (index, indexing_strategy) -> unsafe_v ~index ~indexing_strategy)
      Layout.pack

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      Dict.close t.dict)

  module Make_without_close_checks
      (Val : Pack_value.Persistent with type hash := K.t and type key := Key.t) =
  struct
    module Key = Key
    module Tbl = Table (K)
    module Lru = Irmin.Backend.Lru.Make (Hash)

    type nonrec 'a t = {
      pack : 'a t;
      lru : Val.t Lru.t;
      staging : Val.t Tbl.t;
      mutable open_instances : int;
      readonly : bool;
    }

    type hash = K.t [@@deriving irmin ~pp ~equal ~decode_bin]
    type key = Key.t [@@deriving irmin ~pp]
    type value = Val.t [@@deriving irmin ~pp]

    let index_direct t hash =
      [%log.debug "index %a" pp_hash hash];
      match Index.find t.pack.index hash with
      | None -> None
      | Some (offset, length, _) ->
          Some (Pack_key.v_direct ~hash ~offset ~length)

    let index t hash = Lwt.return (index_direct t hash)

    let unsafe_clear ?keep_generation t =
      clear ?keep_generation t.pack;
      Tbl.clear t.staging;
      Lru.clear t.lru

    (* we need another cache here, as we want to share the LRU and
       staging caches too. *)

    let roots = Hashtbl.create 10

    let valid t =
      if t.open_instances <> 0 then (
        t.open_instances <- t.open_instances + 1;
        true)
      else false

    let flush ?(index = true) ?(index_merge = false) t =
      if index_merge then Index.merge t.pack.index;
      Dict.flush t.pack.dict;
      IO.flush t.pack.block;
      if index then Index.flush t.pack.index;
      Tbl.clear t.staging

    let unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index ~indexing_strategy
        root =
      let pack = v (index, indexing_strategy) ~fresh ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      { staging; lru; pack; open_instances = 1; readonly }

    let unsafe_v ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000)
        ~index ~indexing_strategy root =
      try
        let t = Hashtbl.find roots (root, readonly) in
        if valid t then (
          if fresh then unsafe_clear t;
          t)
        else (
          Hashtbl.remove roots (root, readonly);
          raise Not_found)
      with Not_found ->
        let t =
          unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index ~indexing_strategy
            root
        in
        if fresh then unsafe_clear t;
        Hashtbl.add roots (root, readonly) t;
        t

    let v ?fresh ?readonly ?lru_size ~index ~indexing_strategy root =
      let t =
        unsafe_v ?fresh ?readonly ?lru_size ~index ~indexing_strategy root
      in
      Lwt.return t

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
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
          | Direct_unknown_length _ | Indexed _ ->
              (* [index_direct] returns only [Direct] keys. *)
              assert false)
      | None ->
          corrupted_store "Unexpected object %a missing from index" pp_hash hash

    module Entry_prefix = struct
      type t = {
        hash : hash;
        kind : Pack_value.Kind.t;
        value_length : int option;
            (** Length of the value segment including the length header (if it
                exists). *)
      }
      [@@deriving irmin ~pp_dump]

      let min_length = K.hash_size + 1
      let max_length = K.hash_size + 1 + Varint.max_encoded_size
    end

    let io_read_and_decode_entry_prefix ~off t =
      let buf = Bytes.create Entry_prefix.max_length in
      let bytes_read = IO.read t.pack.block ~off buf in
      if bytes_read < Entry_prefix.min_length then
        invalid_read
          "Attempted to read an entry at offset %a in the pack file, but got \
           only %d bytes"
          Int63.pp off bytes_read;
      let hash = decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0) in
      let kind = Pack_value.Kind.of_magic_exn (Bytes.get buf Hash.hash_size) in
      let value_length =
        match Val.length_header with
        | `Never -> None
        | `Sometimes has_length_header -> (
            let length_header_start = Entry_prefix.min_length in
            match has_length_header kind with
            | None -> None
            | Some `Int32_be ->
                (* The 4 bytes at [length_header_start] are a fixed-length field
                   (if they exist / were read correctly): *)
                let length_header =
                  Bytes.get_int32_be buf length_header_start |> Int32.to_int
                in
                Some (4 + length_header)
            | Some `Varint ->
                (* The bytes starting at [length_header_start] are a
                   variable-length length field (if they exist / were read
                   correctly): *)
                let pos_ref = ref length_header_start in
                let length_header =
                  Varint.decode_bin (Bytes.unsafe_to_string buf) pos_ref
                in
                let length_header_length = !pos_ref - length_header_start in
                Some (length_header_length + length_header))
      in
      { Entry_prefix.hash; kind; value_length }

    let pack_file_contains_key t k =
      let key = Pack_key.inspect k in
      match key with
      | Indexed hash -> Index.mem t.pack.index hash
      | Direct { offset; _ } | Direct_unknown_length { offset; _ } ->
          let minimal_entry_length = Hash.hash_size + 1 in
          let io_offset = IO.offset t.pack.block in
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
                "invalid key %a checked for membership (IO offset = %a)" pp_key
                k Int63.pp io_offset;
            false)
          else
            (* Read the hash explicitly as an integrity check: *)
            let hash = io_read_and_decode_hash ~off:offset t in
            let expected_hash = Key.to_hash k in
            if not (Hash.equal hash expected_hash) then
              invalid_read
                "invalid key %a checked for membership (read hash %a at this \
                 offset instead)"
                pp_key k pp_hash hash;
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
        if not (IO.readonly t.pack.block) then
          let io_offset = IO.offset t.pack.block in
          if off > io_offset then
            (* This is likely a store corruption. We raise [Invalid_read]
               specifically so that [integrity_check] below can handle it. *)
            invalid_read
              "Got request to read %d bytes (at offset %a), but max IO offset \
               is %a"
              len Int63.pp off Int63.pp io_offset
      in
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      if n <> len then
        invalid_read "Read %d bytes (at offset %a) but expected %d" n Int63.pp
          off len;
      let key_of_offset offset =
        (* Attempt to eagerly read the length at the same time as reading the
           hash in order to save an extra IO read when dereferencing the key: *)
        let { Entry_prefix.hash; value_length; _ } =
          io_read_and_decode_entry_prefix ~off:offset t
        in
        match value_length with
        | Some value_length ->
            let length = Hash.hash_size + 1 + value_length in
            Pack_key.v_direct ~hash ~offset ~length
        | None ->
            (* NOTE: we could store [offset] in this key, but since we know the
               entry doesn't have a length header we'll need to check the index
               when dereferencing this key anyway. {i Not} storing the offset
               avoids doing another failed check in the pack file for the length
               header during [find]. *)
            Pack_key.v_indexed hash
      in
      let dict = Dict.find t.pack.dict in
      Val.decode_bin ~key_of_offset ~dict (Bytes.unsafe_to_string buf) (ref 0)

    let pp_io ppf t =
      let name = Filename.basename (Filename.dirname (IO.name t.pack.block)) in
      let mode = if t.readonly then ":RO" else "" in
      Fmt.pf ppf "%s%s" name mode

    let find_in_pack_file ~check_integrity t key hash =
      let { offset; length } =
        match Pack_key.inspect key with
        | Direct { offset; length; _ } ->
            Stats.incr_find_direct ();
            { offset; length }
        | Direct_unknown_length { hash = _; offset } ->
            Stats.incr_find_direct_unknown_length ();
            let length =
              (* First try to recover the length from the pack file: *)
              let prefix = io_read_and_decode_entry_prefix ~off:offset t in
              match prefix.value_length with
              | Some value_length -> Hash.hash_size + 1 + value_length
              | None ->
                  (* Otherwise, we must check the index: *)
                  let span = get_entry_span_from_index_exn t hash in
                  if span.offset <> offset then
                    corrupted_store
                      "Attempted to recover the length of the object \
                       referenced by %a, but the index returned the \
                       inconsistent binding { offset = %a; length = %d }"
                      pp_key key Int63.pp span.offset span.length;
                  span.length
            in
            (* Cache the offset and length information in the existing key: *)
            Pack_key.promote_exn key ~offset ~length;
            { offset; length }
        | Indexed hash ->
            Stats.incr_find_indexed ();
            let entry_span = get_entry_span_from_index_exn t hash in
            Pack_key.promote_exn key ~offset:entry_span.offset
              ~length:entry_span.length;
            entry_span
      in
      let io_offset = IO.offset t.pack.block in
      if Int63.add offset (Int63.of_int length) > io_offset then (
        (* This key is from a different store instance, referencing an
           offset that is not yet visible to this one. It's possible
           that the key _is_ a valid pointer into the same store, but
           the offset just hasn't been flushed to disk yet, so we return
           [None]. *)
        [%log.debug
          "Direct store key references an unknown starting offset %a (length = \
           %d, IO offset = %a)."
          Int63.pp offset length Int63.pp io_offset];
        None)
      else
        let v = io_read_and_decode ~off:offset ~len:length t in
        Lru.add t.lru hash v;
        (if check_integrity then
         check_key key v |> function
         | Ok () -> ()
         | Error (expected, got) ->
             corrupted_store "Got hash %a, expecting %a (for val: %a)." pp_hash
               got pp_hash expected pp_value v);
        Some v

    let unsafe_find ~check_integrity t (k : _ Pack_key.t) =
      [%log.debug "[pack:%a] find %a" pp_io t pp_key k];
      Stats.incr_finds ();
      let hash = Key.to_hash k in
      match Tbl.find t.staging hash with
      | v ->
          Lru.add t.lru hash v;
          Some v
      | exception Not_found -> (
          match Lru.find t.lru hash with
          | v -> Some v
          | exception Not_found ->
              Stats.incr_cache_misses ();
              find_in_pack_file ~check_integrity t k hash)

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

    let unsafe_append ~ensure_unique_indexed ~overcommit t hash v =
      let unguarded_append () =
        [%log.debug "[pack] append %a" pp_hash hash];
        let offset_of_key k =
          match Pack_key.inspect k with
          | Direct { offset; _ } | Direct_unknown_length { offset; _ } ->
              Stats.incr_appended_offsets ();
              Some offset
          | Indexed hash -> (
              match Index.find t.pack.index hash with
              | None ->
                  Stats.incr_appended_hashes ();
                  None
              | Some (offset, _, _) ->
                  Stats.incr_appended_offsets ();
                  Some offset)
        in
        let dict = Dict.index t.pack.dict in
        let off = IO.offset t.pack.block in
        Val.encode_bin ~offset_of_key ~dict v hash (fun s ->
            IO.append t.pack.block s);
        let len = Int63.to_int (IO.offset t.pack.block -- off) in
        let key = Pack_key.v_direct ~hash ~offset:off ~length:len in
        let () =
          let kind = Val.kind v in
          let should_index = t.pack.indexing_strategy ~value_length:len kind in
          if should_index then
            Index.add ~overcommit t.pack.index hash (off, len, Val.kind v)
        in
        if Tbl.length t.staging >= auto_flush then flush t
        else Tbl.add t.staging hash v;
        Lru.add t.lru hash v;
        key
      in
      match ensure_unique_indexed with
      | false -> unguarded_append ()
      | true -> (
          match index_direct t hash with
          | None -> unguarded_append ()
          | Some key -> key)

    let unsafe_add t hash v =
      unsafe_append ~ensure_unique_indexed:true ~overcommit:false t hash v
      |> Lwt.return

    let add t v = unsafe_add t (Val.hash v) v

    let unsafe_close t =
      t.open_instances <- t.open_instances - 1;
      if t.open_instances = 0 then (
        [%log.debug "[pack] close %s" (IO.name t.pack.block)];
        Tbl.clear t.staging;
        Lru.clear t.lru;
        close t.pack)

    let close t =
      unsafe_close t;
      Lwt.return_unit

    let clear t =
      unsafe_clear t;
      Lwt.return_unit

    let clear_keep_generation t =
      unsafe_clear ~keep_generation:() t;
      Lwt.return_unit

    let clear_caches t =
      Tbl.clear t.staging;
      Lru.clear t.lru

    let sync ?(on_generation_change = Fun.id) t =
      let former_offset = IO.offset t.pack.block in
      let former_generation = IO.generation t.pack.block in
      let h = IO.force_headers t.pack.block in
      if former_generation <> h.generation then (
        [%log.debug "[pack] generation changed, refill buffers"];
        clear_caches t;
        on_generation_change ();
        IO.close t.pack.block;
        let block =
          IO.v ~fresh:false ~version:(Some V.version) ~readonly:true
            (IO.name t.pack.block)
        in
        t.pack.block <- block;
        Dict.sync t.pack.dict;
        Index.sync t.pack.index)
      else if h.offset > former_offset then (
        Dict.sync t.pack.dict;
        Index.sync t.pack.index)

    let version t = IO.version t.pack.block
    let generation t = IO.generation t.pack.block
    let offset t = IO.offset t.pack.block
  end

  module Make
      (Val : Pack_value.Persistent with type hash := K.t and type key := Key.t) =
  struct
    module Inner = Make_without_close_checks (Val)
    include Indexable.Closeable (Inner)

    let v ?fresh ?readonly ?lru_size ~index ~indexing_strategy path =
      Inner.v ?fresh ?readonly ?lru_size ~index ~indexing_strategy path
      >|= make_closeable

    let sync ?on_generation_change t =
      Inner.sync ?on_generation_change (get_open_exn t)

    let flush ?index ?index_merge t =
      Inner.flush ?index ?index_merge (get_open_exn t)

    let version t = Inner.version (get_open_exn t)
    let offset t = Inner.offset (get_open_exn t)
    let clear_caches t = Inner.clear_caches (get_open_exn t)

    let integrity_check ~offset ~length k t =
      Inner.integrity_check ~offset ~length k (get_open_exn t)
  end
end
