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
    module Cclru = Cachecache.Lru.Make (H)

    type nonrec 'a t = {
      pack : 'a t;
      lru : Val.t Lru.t;
      staging : Val.t Tbl.t;
      mutable open_instances : int;
      readonly : bool;
      offsets_for_free_lru : int63 Cclru.t;
      read_buffer : Bytes.t;
    }

    type key = K.t

    let equal_key = Irmin.Type.(unstage (equal K.t))

    type value = Val.t
    type index = Index.t

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
      if index then Index.flush ~no_callback:() t.pack.index;
      Tbl.clear t.staging

    type st = {
      mutable offlru_insertions : int;
      mutable unsafe_find : int;
      mutable unsafe_find_table_hit : int;
      mutable unsafe_find_vallru_hit : int;
      mutable unsafe_find_offlru_hit : int;
      mutable unsafe_find_long_none : int;
      mutable unsafe_find_long_some : int;
      mutable unsafe_append : int;
      mutable unsafe_append_offset : int;
      mutable unsafe_append_offset_offlru_hit : int;
      mutable unsafe_append_offset_long_none : int;
      mutable unsafe_append_offset_long_some : int;
      mutable blindfolded_read_first_try : int;
      mutable blindfolded_read_second_try : int;
      mutable blindfolded_read_third_or_more_try : int;
    }
    [@@deriving irmin]

    let st =
      {
        offlru_insertions = 0;
        unsafe_find = 0;
        unsafe_find_table_hit = 0;
        unsafe_find_vallru_hit = 0;
        unsafe_find_offlru_hit = 0;
        unsafe_find_long_none = 0;
        unsafe_find_long_some = 0;
        unsafe_append = 0;
        unsafe_append_offset = 0;
        unsafe_append_offset_offlru_hit = 0;
        unsafe_append_offset_long_none = 0;
        unsafe_append_offset_long_some = 0;
        blindfolded_read_first_try = 0;
        blindfolded_read_second_try = 0;
        blindfolded_read_third_or_more_try = 0;
      }

    let () =
      let st0 =
        {
          offlru_insertions = 0;
          unsafe_find = 0;
          unsafe_find_table_hit = 0;
          unsafe_find_vallru_hit = 0;
          unsafe_find_offlru_hit = 0;
          unsafe_find_long_none = 0;
          unsafe_find_long_some = 0;
          unsafe_append = 0;
          unsafe_append_offset = 0;
          unsafe_append_offset_offlru_hit = 0;
          unsafe_append_offset_long_none = 0;
          unsafe_append_offset_long_some = 0;
          blindfolded_read_first_try = 0;
          blindfolded_read_second_try = 0;
          blindfolded_read_third_or_more_try = 0;
        }
      in
      at_exit (fun () ->
          if st <> st0 then (
            Printf.eprintf "exit\n%!";
            Fmt.epr "%a\n%!" (Irmin.Type.pp st_t) st;
            Fmt.epr "%a\n%!" (Irmin.Type.pp Stats.t) (Stats.get ());
            Printf.eprintf "\n%!"))

    let unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root =
      let pack = v index ~fresh ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      let s =
        try "LRU_SIZE_OFF" |> Sys.getenv |> int_of_string
        with Not_found -> 1000
      in
      let read_buffer = Bytes.create (4096 * 3) in
      Printf.eprintf "LRU SIZE! = %d\n%!" s;
      (* Fmt.epr "& v %d\n%!" s; *)
      let offsets_for_free_lru = Cclru.v s in
      {
        staging;
        lru;
        pack;
        open_instances = 1;
        readonly;
        offsets_for_free_lru;
        read_buffer;
      }

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
      decode_key (Bytes.unsafe_to_string buf) (ref 0)
    (* let n = IO.read_buffer t.pack.block ~off ~buf:t.read_buffer ~len:K.hash_size in
     * assert (n = K.hash_size);
     * decode_key (Bytes.unsafe_to_string t.read_buffer) (ref 0) *)

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

    let min_load_bytes = K.hash_size + 30 |> Int63.of_int

    let blindfolded_io_read_at startoff next_page_idx_to_load t =
      (* Fmt.epr "| blindfolded_io_read_at with buflen = %d\n%!"
       *   (Bytes.length t.read_buffer); *)
      let endoff =
        IO.end_offset_of_page_idx t.pack.block next_page_idx_to_load
      in
      let len = Int63.sub endoff startoff |> Int63.to_int in
      (* Fmt.epr "| blindfolded_io_read_at with len %d\n%!" len; *)
      assert (Bytes.length t.read_buffer >= len);
      (* Fmt.epr
       *   "blindfolded_io_read_at npitl:%a startoff:%a endoff:%a maxoff:%a len:%d \n\
       *    %!"
       *   Int63.pp next_page_idx_to_load Int63.pp startoff Int63.pp endoff
       *   Int63.pp (IO.offset t.pack.block) len; *)
      if len <= 0 then raise Invalid_read;
      assert (Int63.compare startoff endoff < 0);

      let n =
        IO.read_buffer t.pack.block ~off:startoff ~buf:t.read_buffer ~len
      in

      (* let buf = Bytes.create len in
       * let n = IO.read t.pack.block ~off:startoff buf in *)
      if n <> len then (
        Fmt.epr
          "FAILED: blindfolded_io_read_at npitl:%a startoff:%a endoff:%a \
           len:%d. Read only %d \n\
           %!"
          Int63.pp next_page_idx_to_load Int63.pp startoff Int63.pp endoff len n;
        raise Invalid_read);
      n

    (* t.read_buffer *)
    (* buf *)

    let is_long_enough buf buf_len =
      try
        (* Printf.eprintf "is_long_enough?\n%!"; *)
        let len = Val.decode_bin_length (Bytes.unsafe_to_string buf) 0 in
        (* Printf.eprintf "good: len:%d buflen:%d \n%!" len (Bytes.length buf); *)
        len <= buf_len
      with
      | Invalid_argument msg when msg = "index out of bounds" -> false
      | Invalid_argument msg when msg = "String.blit / Bytes.blit_string" ->
          false

    let blindfolded_io_read off t =
      (* Printf.eprintf "| blindfolded_io_read\n%!"; *)
      let rec aux attempt startoff next_page_idx_to_load payload =
        (* Printf.eprintf "| blindfolded_io_read | aux %d\n%!" attempt; *)
        let ((bytes_read, buf) as payload) =
          match payload with
          | None ->
              let n = blindfolded_io_read_at startoff next_page_idx_to_load t in
              (n, t.read_buffer)
          | Some (bytes_read, buf) ->
              let buf = Bytes.sub buf 0 bytes_read in
              let n = blindfolded_io_read_at startoff next_page_idx_to_load t in
              let buf = Bytes.cat buf t.read_buffer in
              (bytes_read + n, buf)
        in
        if is_long_enough buf bytes_read then (
          if attempt = 0 then
            st.blindfolded_read_first_try <- st.blindfolded_read_first_try + 1
          else if attempt = 1 then
            st.blindfolded_read_second_try <- st.blindfolded_read_second_try + 1
          else
            st.blindfolded_read_third_or_more_try <-
              st.blindfolded_read_third_or_more_try + 1;
          buf)
        else
          aux (Int.succ attempt)
            (Int63.succ next_page_idx_to_load
            |> IO.start_offset_of_page_idx t.pack.block)
            (Int63.succ next_page_idx_to_load)
            (Some payload)
      in

      let page0_idx = IO.page_idx_of_offset t.pack.block off in
      let page0_remaining =
        IO.bytes_remaning_in_page_from_offset t.pack.block off
      in
      (* Fmt.epr "blindfolded_io_read pidx:%a rem:%a\n%!"
       *   Int63.pp page0_idx
       *   Int63.pp page0_remaining
       * ; *)
      assert (Int63.compare page0_remaining Int63.zero > 0);
      if Int63.compare page0_remaining min_load_bytes < 0 then
        (* If [off] is near the end of the page, load next page too *)
        aux 0 off (Int63.succ page0_idx) None
      else aux 0 off page0_idx None

    let io_read_and_decode ~off ~len_opt t =
      (* Printf.eprintf "* io_read_and_decode\n%!"; *)
      if (not (IO.readonly t.pack.block)) && off > IO.offset t.pack.block then
        raise Invalid_read;

      let buf =
        match len_opt with
        | Some len ->
            let buf = Bytes.create len in
            let n = IO.read t.pack.block ~off buf in
            if n <> len then raise Invalid_read;
            buf
        | None -> blindfolded_io_read off t
      in

      (* Fmt.epr "| io_read_and_decode, made a buffer of len %d\n%!"
       *   (Bytes.length buf); *)
      let hash off =
        (* Fmt.epr "| io_read_and_decode | hash\n%!"; *)
        let h = io_read_and_decode_hash ~off t in
        Cclru.replace t.offsets_for_free_lru h off;
        st.offlru_insertions <- st.offlru_insertions + 1;
        h
      in
      let dict = Dict.find t.pack.dict in
      (* Fmt.epr "| go\n%!"; *)
      let res =
        Val.decode_bin ~hash ~dict (Bytes.unsafe_to_string buf) (ref 0)
      in
      (* Fmt.epr "| done\n%!"; *)
      assert (Bytes.length t.read_buffer = 4096 * 3);
      res

    let pp_io ppf t =
      let name = Filename.basename (Filename.dirname (IO.name t.pack.block)) in
      let mode = if t.readonly then ":RO" else "" in
      Fmt.pf ppf "%s%s" name mode

    let unsafe_find' ~check_integrity t k off len_opt =
      let v = io_read_and_decode ~off ~len_opt t in
      (if check_integrity then
       check_key k v |> function
       | Ok () -> ()
       | Error (expected, got) ->
           Fmt.failwith "corrupted value: got %a, expecting %a." pp_hash got
             pp_hash expected);
      Lru.add t.lru k v;
      Some v

    let unsafe_find ~check_integrity t k =
      [%log.debug "[pack:%a] find %a" pp_io t pp_hash k];
      Stats.incr_finds ();
      st.unsafe_find <- st.unsafe_find + 1;
      match Tbl.find t.staging k with
      | v ->
          st.unsafe_find_table_hit <- st.unsafe_find_table_hit + 1;
          Lru.add t.lru k v;
          Some v
      | exception Not_found -> (
          match Lru.find t.lru k with
          | v ->
              st.unsafe_find_vallru_hit <- st.unsafe_find_vallru_hit + 1;
              Some v
          | exception Not_found -> (
              (* match raise Not_found with *)
              match Cclru.find t.offsets_for_free_lru k with
              | off ->
                  st.unsafe_find_offlru_hit <- st.unsafe_find_offlru_hit + 1;
                  unsafe_find' ~check_integrity t k off None
              | exception Not_found -> (
                  Stats.incr_cache_misses ();
                  match Index.find t.pack.index k with
                  | None ->
                      st.unsafe_find_long_none <- st.unsafe_find_long_none + 1;
                      None
                  | Some (off, len, _) ->
                      st.unsafe_find_long_some <- st.unsafe_find_long_some + 1;
                      unsafe_find' ~check_integrity t k off (Some len))))

    let find t k =
      let v = unsafe_find ~check_integrity:true t k in
      Lwt.return v

    let cast t = (t :> read_write t)

    let integrity_check ~offset ~length k t =
      (* Fmt.epr " pack_store.integrity_check\n%!"; *)
      try
        let value = io_read_and_decode ~off:offset ~len_opt:(Some length) t in
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
        st.unsafe_append <- st.unsafe_append + 1;
        [%log.debug "[pack] append %a" pp_hash k];
        let offset k =
          st.unsafe_append_offset <- st.unsafe_append_offset + 1;
          (* match raise Not_found with *)
          match Cclru.find t.offsets_for_free_lru k with
          | off ->
              st.unsafe_append_offset_offlru_hit <-
                st.unsafe_append_offset_offlru_hit + 1;
              Stats.incr_appended_offsets ();
              Some off
          | exception Not_found -> (
              match Index.find t.pack.index k with
              | None ->
                  st.unsafe_append_offset_long_none <-
                    st.unsafe_append_offset_long_none + 1;
                  Stats.incr_appended_hashes ();
                  None
              | Some (off, _, _) ->
                  st.unsafe_append_offset_long_some <-
                    st.unsafe_append_offset_long_some + 1;
                  Stats.incr_appended_offsets ();
                  Some off)
        in
        let dict = Dict.index t.pack.dict in
        let off = IO.offset t.pack.block in
        Val.encode_bin ~offset ~dict v k (IO.append t.pack.block);
        let len = Int63.to_int (IO.offset t.pack.block -- off) in
        Index.add ~overcommit t.pack.index k (off, len, Val.kind v);
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
