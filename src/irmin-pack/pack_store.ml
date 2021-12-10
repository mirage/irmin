open! Import
include Pack_store_intf

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

let selected_version = `V2

module Maker (Index : Pack_index.S) (K : Irmin.Hash.S with type t = Index.key) :
  Maker with type hash = K.t and type index := Index.t = struct
  module IO_cache = IO.Cache
  module IO = IO.Unix
  module Tbl = Table (K)
  module Dict = Pack_dict

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
    dict : Dict.t;
    mutable open_instances : int;
  }

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~index ~fresh ~readonly file =
    let root = Filename.dirname file in
    let dict = Dict.v ~fresh ~readonly root in
    let block =
      (* If the file already exists in V1, we will bump the generation header
         lazily when appending a V2 entry. *)
      let version = Some selected_version in
      IO.v ~version ~fresh ~readonly file
    in
    { block; index; dict; open_instances = 1 }

  let IO_cache.{ v } =
    IO_cache.memoize ~valid
      ~clear:(fun t -> IO.truncate t.block)
      ~v:(fun index -> unsafe_v ~index)
      Layout.pack

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      Dict.close t.dict)

  let debug_block t = t.block

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

    let debug_block t = debug_block t.pack

    type hash = K.t [@@deriving irmin ~pp ~equal ~decode_bin]
    type key = Key.t [@@deriving irmin ~pp ~equal]
    type value = Val.t [@@deriving irmin ~pp]

    let index_direct _ hash = Some hash
    let index t hash = Lwt.return (index_direct t hash)

    let clear t =
      if IO.offset t.block <> Int63.zero then (
        Index.clear t.index;
        IO.truncate t.block)

    let unsafe_clear t =
      clear t.pack;
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

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
      decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0)

    let unsafe_mem t k =
      [%log.debug "[pack] mem %a" pp_hash k];
      Tbl.mem t.staging k || Lru.mem t.lru k || Index.mem t.pack.index k

    let mem t k =
      let b = unsafe_mem t k in
      Lwt.return b

    let check_hash h v =
      let h' = Val.hash v in
      if equal_hash h h' then Ok () else Error (h, h')

    exception Invalid_read

    let io_read_and_decode ~off ~len t =
      if (not (IO.readonly t.pack.block)) && off > IO.offset t.pack.block then
        raise Invalid_read;
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      if n <> len then raise Invalid_read;
      let key_of_offset off = io_read_and_decode_hash ~off t in
      let key_of_hash hash = hash in
      let dict = Dict.find t.pack.dict in
      Val.decode_bin ~key_of_offset ~key_of_hash ~dict
        (Bytes.unsafe_to_string buf)
        (ref 0)

    let pp_io ppf t =
      let name = Filename.basename (Filename.dirname (IO.name t.pack.block)) in
      let mode = if t.readonly then ":RO" else "" in
      Fmt.pf ppf "%s%s" name mode

    let unsafe_find ~check_integrity t k =
      [%log.debug "[pack:%a] find %a" pp_io t pp_hash k];
      let location, value =
        match Tbl.find t.staging k with
        | v ->
            Lru.add t.lru k v;
            (Stats.Find.Staging, Some v)
        | exception Not_found -> (
            match Lru.find t.lru k with
            | v -> (Stats.Find.Lru, Some v)
            | exception Not_found -> (
                match Index.find t.pack.index k with
                | None -> (Stats.Find.Not_found, None)
                | Some (off, len, _) ->
                    let v = io_read_and_decode ~off ~len t in
                    (if check_integrity then
                     check_hash k v |> function
                     | Ok () -> ()
                     | Error (expected, got) ->
                         Fmt.failwith "corrupted value: got %a, expecting %a."
                           pp_hash got pp_hash expected);
                    Lru.add t.lru k v;
                    (Stats.Find.Pack, Some v)))
      in
      Stats.report_find ~location;
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
      with Invalid_read -> Error `Absent_value

    let batch t f =
      let* r = f (cast t) in
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        flush t;
        Lwt.return r)

    let auto_flush = 1024

    let unsafe_append ~ensure_unique ~overcommit t k v =
      if ensure_unique && unsafe_mem t k then k
      else (
        [%log.debug "[pack] append %a" pp_hash k];
        let offset_of_key k =
          match Index.find t.pack.index k with
          | None ->
              Stats.incr_appended_hashes ();
              None
          | Some (off, _, _) ->
              Stats.incr_appended_offsets ();
              Some off
        in
        let kind = Val.kind v in
        let () =
          (* Bump the pack file version header if necessary *)
          let value_version = Pack_value.Kind.version kind
          and io_version = IO.version t.pack.block in
          if Version.compare value_version io_version > 0 then
            IO.set_version t.pack.block value_version
        in
        let dict = Dict.index t.pack.dict in
        let off = IO.offset t.pack.block in
        Val.encode_bin ~offset_of_key ~dict k v (IO.append t.pack.block);
        let len = Int63.to_int (IO.offset t.pack.block -- off) in
        Index.add ~overcommit t.pack.index k (off, len, kind);
        if Tbl.length t.staging >= auto_flush then flush t
        else Tbl.add t.staging k v;
        Lru.add t.lru k v;
        k)

    let add t v =
      let k = Val.hash v in
      unsafe_append ~ensure_unique:true ~overcommit:false t k v |> Lwt.return

    let unsafe_add t k v =
      unsafe_append ~ensure_unique:true ~overcommit:false t k v |> Lwt.return

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

    let clear_caches t =
      Tbl.clear t.staging;
      Lru.clear t.lru

    let sync t =
      let former_offset = IO.offset t.pack.block in
      let offset = IO.force_offset t.pack.block in
      if offset > former_offset then (
        Dict.sync t.pack.dict;
        Index.sync t.pack.index)

    let offset t = IO.offset t.pack.block
  end

  module Make
      (Val : Pack_value.Persistent with type hash := K.t and type key := Key.t) =
  struct
    module Inner = Make_without_close_checks (Val)
    include Indexable.Closeable (Inner)

    let debug_block (t:'a t) = Inner.debug_block (get_open_exn t)

    let v ?fresh ?readonly ?lru_size ~index path =
      Inner.v ?fresh ?readonly ?lru_size ~index path >|= make_closeable

    let sync t = Inner.sync (get_open_exn t)

    let flush ?index ?index_merge t =
      Inner.flush ?index ?index_merge (get_open_exn t)

    let offset t = Inner.offset (get_open_exn t)
    let clear_caches t = Inner.clear_caches (get_open_exn t)

    let integrity_check ~offset ~length k t =
      Inner.integrity_check ~offset ~length k (get_open_exn t)

  end
end
