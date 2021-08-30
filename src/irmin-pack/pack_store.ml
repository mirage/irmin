open! Import
include Pack_store_intf

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

exception Invalid_read of string

let invalid_read fmt = Fmt.kstr (fun s -> raise (Invalid_read s)) fmt

module Maker
    (V : Version.S)
    (Index : Pack_index.S)
    (K : Irmin.Hash.S with type t = Index.key) :
  Maker with type hash = K.t and type index = Index.t = struct
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
  type index = Index.t

  type 'a t = {
    mutable block : IO.t;
    index : Index.t;
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

  let unsafe_v ~index ~fresh ~readonly file =
    let root = Filename.dirname file in
    let dict = Dict.v ~fresh ~readonly root in
    let block = IO.v ~version:(Some V.version) ~fresh ~readonly file in
    { block; index; dict; open_instances = 1 }

  let IO_cache.{ v } =
    IO_cache.memoize ~clear ~valid ~v:(fun index -> unsafe_v ~index) Layout.pack

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      Dict.close t.dict)

  module Make_without_close_checks
      (Val : Pack_value.Persistent with type hash := K.t) =
  struct
    module Key = Key
    module Tbl = Table (K)
    module Lru = Irmin.Private.Lru.Make (Hash)

    type nonrec 'a t = {
      pack : 'a t;
      lru : Val.t Lru.t;
      staging : Val.t Tbl.t;
      mutable open_instances : int;
      readonly : bool;
    }

    type hash = K.t [@@deriving irmin ~equal]
    type key = Key.t [@@deriving irmin ~pp]
    type value = Val.t [@@deriving irmin ~pp]
    type index = Index.t

    let index_direct t hash =
      match Index.find t.pack.index hash with
      | None -> None
      | Some (offset, length, _) -> Some (Pack_key.v ~hash ~offset ~length)

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
      Fmt.epr "Doing a flush, you know.@.";
      if index_merge then Index.merge t.pack.index;
      Dict.flush t.pack.dict;
      IO.flush t.pack.block;
      if index then Index.flush ~no_callback:() t.pack.index;
      Tbl.clear t.staging

    let unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root =
      let pack = v index ~fresh ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      { staging; lru; pack; open_instances = 1; readonly }

    let unsafe_v ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000)
        ~index root =
      try
        let t = Hashtbl.find roots (root, readonly) in
        if valid t then (
          if fresh then unsafe_clear t;
          t)
        else (
          Hashtbl.remove roots (root, readonly);
          raise Not_found)
      with Not_found ->
        let t = unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root in
        if fresh then unsafe_clear t;
        Hashtbl.add roots (root, readonly) t;
        t

    let v ?fresh ?readonly ?lru_size ~index root =
      let t = unsafe_v ?fresh ?readonly ?lru_size ~index root in
      Lwt.return t

    let pp_hash = Irmin.Type.pp K.t
    let decode_hash = Irmin.Type.(unstage (decode_bin K.t))

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
      let _, hash = decode_hash (Bytes.unsafe_to_string buf) 0 in
      hash

    let pack_file_contains_key t k =
      let offset = Key.to_offset k in
      if Int63.compare offset (IO.offset t.pack.block) >= 0 then false
      else
        (* We read the hash explicitly as an integrity check: it's not
           sufficient to assume that any offset within the file is valid. *)
        let hash = io_read_and_decode_hash ~off:offset t in
        let expected_hash = Key.to_hash k in
        (* XXX: raise exception / log error in the [false] case here? *)
        Hash.equal hash expected_hash

    (* NOTE: may return false negatives, since the index may be partial. *)
    (* let mem_via_hash t h =
     *   Log.debug (fun l -> l "[pack] contains_hash %a" pp_hash h);
     *   Tbl.mem t.staging h || Lru.mem t.lru h || Index.mem t.pack.index h *)

    let unsafe_mem t k =
      Log.debug (fun l -> l "[pack] mem %a" pp_key k);
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
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      if n <> len then
        invalid_read "Read %d bytes (at offset %a) but expected %d" n Int63.pp
          off len;
      let key_of_offset off =
        let hash = io_read_and_decode_hash ~off t in
        Pack_key.v ~hash ~offset:off ~length:len
      in
      let dict = Dict.find t.pack.dict in
      Val.decode_bin ~key_of_offset ~dict (Bytes.unsafe_to_string buf) 0

    let pp_io ppf t =
      let name = Filename.basename (Filename.dirname (IO.name t.pack.block)) in
      let mode = if t.readonly then ":RO" else "" in
      Fmt.pf ppf "%s%s" name mode

    let pp_tbl : Val.t Tbl.t Fmt.t =
     fun ppf x -> Repr.(pp_dump (seq (pair Hash.t Val.t))) ppf (Tbl.to_seq x)

    let unsafe_find ~check_integrity t k =
      Log.debug (fun l -> l "[pack:%a] find %a" pp_io t pp_key k);
      Log.err (fun l -> l "tbl state: %a" pp_tbl t.staging);
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
              (* XXX *)
              (* match Index.find t.pack.index k with
               * | None -> None
               * | Some (off, len, _) -> *)
              let off, len = Key.(to_offset k, to_length k) in
              if off >= IO.offset t.pack.block then
                (* This key is from a different store instance, referencing an
                   offset that is not yet visible to this one. It's possible
                   that the key _is_ a valid pointer into the same store, but
                   the offset just hasn't been flushed to disk yet, so we return
                   [None]. *)
                None
              else
                let () = Fmt.epr "-> %s@." __LOC__ in
                let v = snd (io_read_and_decode ~off ~len t) in
                (if check_integrity then
                 check_key k v |> function
                 | Ok () -> ()
                 | Error (expected, got) ->
                     Fmt.failwith
                       "corrupted value: got %a, expecting %a (for val: %a)."
                       pp_hash got pp_hash expected pp_value v);
                Lru.add t.lru hash v;
                Some v)

    let find t k =
      let v = unsafe_find ~check_integrity:true t k in
      Lwt.return v

    let cast t = (t :> read_write t)

    let integrity_check ~offset ~length hash t =
      try
        let value = snd (io_read_and_decode ~off:offset ~len:length t) in
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
        Log.debug (fun l -> l "[pack] append %a" pp_hash hash);
        let offset_of_key k =
          (* XXX: don't need option any more? *)
          Some (Key.to_offset k)
          (* match Index.find t.pack.index k with
           * | None ->
           *     Stats.incr_appended_hashes ();
           *     None
           * | Some (off, _, _) ->
           *     Stats.incr_appended_offsets ();
           *     Some off *)
        in
        (* let hash = Key.to_hash k in *)
        let dict = Dict.index t.pack.dict in
        let off = IO.offset t.pack.block in
        Val.encode_bin ~offset_of_key ~dict v hash (fun s ->
            Fmt.epr "Appending string: %S@." s;
            IO.append t.pack.block s);
        let len = Int63.to_int (IO.offset t.pack.block -- off) in
        let key = Pack_key.v ~hash ~offset:off ~length:len in
        (* XXX: parameterise over indexing strategies here *)
        Index.add ~overcommit t.pack.index hash (off, len, Val.kind v);
        if Tbl.length t.staging >= auto_flush then flush t
        else Tbl.add t.staging hash v;
        Log.err (fun f -> f "Tbl: %a" pp_tbl t.staging);
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
        Log.debug (fun l -> l "[pack] close %s" (IO.name t.pack.block));
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
        Log.debug (fun l -> l "[pack] generation changed, refill buffers");
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

  module Make (Val : Pack_value.Persistent with type hash := K.t) = struct
    module Inner = Make_without_close_checks (Val)
    include Indexable.Closeable (Inner)

    let v ?fresh ?readonly ?lru_size ~index path =
      Inner.v ?fresh ?readonly ?lru_size ~index path >|= make_closeable

    type index = Inner.index

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
