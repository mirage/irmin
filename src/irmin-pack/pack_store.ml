open! Import
include Pack_store_intf

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

module Maker
    (V : Version.S)
    (Index : Pack_index.S)
    (K : Irmin.Hash.S with type t = Index.key) :
  Maker with type key = K.t and type index = Index.t = struct
  module IO_cache = IO.Cache
  module IO = IO.Unix
  module Tbl = Table (K)
  module Dict = Pack_dict.Make (V)

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

  type key = K.t

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      Dict.close t.dict)

  module Make_without_close_checks (Val : Pack_value.S with type hash := K.t) =
  struct
    module H = struct
      include K

      let hash = K.short_hash
      let equal = Irmin.Type.(unstage (equal K.t))
    end

    module Tbl = Table (K)
    module Lru = Irmin.Backend.Lru.Make (H)

    module Offsets = Irmin.Backend.Lru.Make (struct
      type t = int63

      let hash = Hashtbl.hash
      let equal = ( = )
    end)

    type offset_and_value = int63 * Val.t

    type nonrec 'a t = {
      pack : 'a t;
      lru : offset_and_value Lru.t;
      staging : offset_and_value Tbl.t;
      offsets : Val.t Offsets.t;
      mutable open_instances : int;
      readonly : bool;
    }

    type key = K.t

    let equal_key = Irmin.Type.(unstage (equal K.t))

    type value = Val.t
    type index = Index.t

    let unsafe_clear ?keep_generation t =
      clear ?keep_generation t.pack;
      Tbl.clear t.staging;
      Lru.clear t.lru;
      Offsets.clear t.offsets

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
      if index then Index.flush ~no_callback:() t.pack.index;
      Tbl.clear t.staging

    let unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root =
      let pack = v index ~fresh ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      let offsets = Offsets.create lru_size in
      { staging; lru; pack; open_instances = 1; readonly; offsets }

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
    let decode_key = Irmin.Type.(unstage (decode_bin K.t))

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
      let _, v = decode_key (Bytes.unsafe_to_string buf) 0 in
      v

    let unsafe_mem t k =
      [%log.debug "[pack] mem %a" pp_hash k];
      Tbl.mem t.staging k || Lru.mem t.lru k || Index.mem t.pack.index k

    let mem t k =
      let b = unsafe_mem t k in
      Lwt.return b

    let check_key k v =
      let k' = Val.hash v in
      if equal_key k k' then Ok () else Error (k, k')

    exception Invalid_read

    let io_read_and_decode ~off ~len t =
      if (not (IO.readonly t.pack.block)) && off > IO.offset t.pack.block then
        raise Invalid_read;
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      if n <> len then raise Invalid_read;
      let hash off =
        match Offsets.find t.offsets off with
        | v -> Val.hash v
        | exception Not_found -> io_read_and_decode_hash ~off t
      in
      let dict = Dict.find t.pack.dict in
      let _, v = Val.decode_bin ~hash ~dict (Bytes.unsafe_to_string buf) 0 in
      v

    let pp_io ppf t =
      let name = Filename.basename (Filename.dirname (IO.name t.pack.block)) in
      let mode = if t.readonly then ":RO" else "" in
      Fmt.pf ppf "%s%s" name mode

    let unsafe_find_aux t k f g =
      [%log.debug "[pack:%a] find %a" pp_io t pp_hash k];
      Stats.incr_finds ();
      match Tbl.find t.staging k with
      | v ->
          Lru.add t.lru k v;
          Some (f v)
      | exception Not_found -> (
          match Lru.find t.lru k with
          | v -> Some (f v)
          | exception Not_found -> (
              Stats.incr_cache_misses ();
              match Index.find t.pack.index k with
              | None -> None
              | Some (off, len, _) -> g off len))

    let unsafe_find ~check_integrity t k =
      let find off len =
        let v = io_read_and_decode ~off ~len t in
        (if check_integrity then
         check_key k v |> function
         | Ok () -> ()
         | Error (expected, got) ->
             Fmt.failwith "corrupted value: got %a, expecting %a." pp_hash got
               pp_hash expected);
        Lru.add t.lru k (off, v);
        Some v
      in
      unsafe_find_aux t k snd find

    let find t k =
      let v = unsafe_find ~check_integrity:true t k in
      Lwt.return v

    let cast t = (t :> read_write t)

    let integrity_check ~offset ~length k t =
      try
        let value = io_read_and_decode ~off:offset ~len:length t in
        match check_key k value with
        | Ok () -> Ok ()
        | Error _ -> Error `Wrong_hash
      with Invalid_read -> Error `Absent_value

    let batch t f =
      let* r = f (cast t) in
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        flush t;
        Lwt.return r)

    let auto_flush = 1024

    let unsafe_append ~ensure_unique ~overcommit t k v =
      if ensure_unique && unsafe_mem t k then ()
      else (
        [%log.debug "[pack] append %a" pp_hash k];
        let offset k =
          let f off _ = Some off in
          match unsafe_find_aux t k fst f with
          | Some _ as f ->
              Stats.incr_appended_offsets ();
              f
          | None ->
              Stats.incr_appended_hashes ();
              None
        in
        let dict = Dict.index t.pack.dict in
        let off = IO.offset t.pack.block in
        Val.encode_bin ~offset ~dict v k (IO.append t.pack.block);
        let len = Int63.to_int (IO.offset t.pack.block -- off) in
        Index.add ~overcommit t.pack.index k (off, len, Val.kind v);
        Offsets.add t.offsets off v;
        let v = (off, v) in
        if Tbl.length t.staging >= auto_flush then flush t
        else Tbl.add t.staging k v;
        Lru.add t.lru k v)

    let add t v =
      let k = Val.hash v in
      unsafe_append ~ensure_unique:true ~overcommit:false t k v;
      Lwt.return k

    let unsafe_add t k v =
      unsafe_append ~ensure_unique:true ~overcommit:false t k v;
      Lwt.return ()

    let unsafe_close t =
      t.open_instances <- t.open_instances - 1;
      if t.open_instances = 0 then (
        [%log.debug "[pack] close %s" (IO.name t.pack.block)];
        Tbl.clear t.staging;
        Lru.clear t.lru;
        Offsets.clear t.offsets;
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
      Lru.clear t.lru;
      Offsets.clear t.offsets

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

  module Make (Val : Pack_value.S with type hash := K.t) = struct
    module Inner = Make_without_close_checks (Val)
    include Content_addressable.Closeable (Inner)

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
