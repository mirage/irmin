(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Import
include Pack_store_intf

exception Invalid_read of string
exception Corrupted_store of string
exception Dangling_hash

let invalid_read fmt = Fmt.kstr (fun s -> raise (Invalid_read s)) fmt
let corrupted_store fmt = Fmt.kstr (fun s -> raise (Corrupted_store s)) fmt

module Table (K : Irmin.Hash.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = K.short_hash
  let equal = Irmin.Type.(unstage (equal K.t))
end)

module Make_without_close_checks
    (Fm : File_manager.S)
    (Dict : Dict.S)
    (Dispatcher : Dispatcher.S with module Fm = Fm)
    (Hash : Irmin.Hash.S with type t = Fm.Index.key)
    (Val : Pack_value.Persistent
             with type hash := Hash.t
              and type key := Hash.t Pack_key.t)
    (Errs : Io_errors.S with module Io = Fm.Io) =
struct
  module Tbl = Table (Hash)
  module Control = Fm.Control
  module Suffix = Fm.Suffix
  module Index = Fm.Index
  module Key = Pack_key.Make (Hash)

  module Lru = struct
    include Lru

    let add t k v = Val.to_kinded v |> add t k (Val.weight v)
    let find t k = find t k |> Val.of_kinded
  end

  type file_manager = Fm.t
  type dict = Dict.t
  type dispatcher = Dispatcher.t

  type 'a t = {
    lru : Lru.t;
    staging : Val.t Tbl.t;
    indexing_strategy : Irmin_pack.Indexing_strategy.t;
    fm : Fm.t;
    dict : Dict.t;
    dispatcher : Dispatcher.t;
  }

  type hash = Hash.t [@@deriving irmin ~pp ~equal ~decode_bin]
  type key = Key.t [@@deriving irmin ~pp]
  type value = Val.t [@@deriving irmin ~pp]

  let get_location t k =
    match Pack_key.inspect k with
    | Indexed hash -> (
        match Index.find (Fm.index t.fm) hash with
        | None -> raise Dangling_hash
        | Some (off, len, _kind) ->
            Pack_key.promote_exn k ~offset:off ~length:len;
            (off, len, None))
    | Direct { offset; length; volume_identifier; _ } ->
        (offset, length, volume_identifier)

  let get_offset t k =
    match Pack_key.to_offset k with
    | Some off -> off
    | None ->
        let off, _, _ = get_location t k in
        off

  let get_length t k =
    match Pack_key.to_length k with
    | Some len -> len
    | None ->
        let _, len, _ = get_location t k in
        len

  let len_of_direct_key k =
    match Pack_key.inspect k with
    | Indexed _ -> assert false
    | Direct { length; _ } -> length

  let off_of_direct_key k =
    match Pack_key.to_offset k with
    | None -> assert false
    | Some offset -> offset

  let index_direct_with_kind t hash =
    [%log.debug "index %a" pp_hash hash];
    match Index.find (Fm.index t.fm) hash with
    | None -> None
    | Some (offset, length, kind) ->
        let key = Pack_key.v_direct ~offset ~length hash in
        Some (key, kind)

  let index_direct t hash =
    index_direct_with_kind t hash |> Option.map (fun (key, _) -> key)

  let index t hash = Lwt.return (index_direct t hash)

  let v ~config ~fm ~dict ~dispatcher ~lru =
    let indexing_strategy = Conf.indexing_strategy config in
    let staging = Tbl.create 127 in
    Fm.register_suffix_consumer fm ~after_flush:(fun () -> Tbl.clear staging);
    Fm.register_prefix_consumer fm ~after_reload:(fun () -> Ok (Lru.clear lru));
    { lru; staging; indexing_strategy; fm; dict; dispatcher }

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

  let read_and_decode_entry_prefix ~off ?volume_identifier dispatcher =
    let buf = Bytes.create Entry_prefix.max_length in
    let _len, _volume =
      try
        (* We may read fewer then [Entry_prefix.max_length] bytes when reading the
           final entry in the pack file (if the data section of the entry is
           shorter than [Varint.max_encoded_size]. In this case, an invalid read
           may be discovered below when attempting to decode the length header. *)
        Dispatcher.read_range_exn dispatcher ?volume_identifier ~off
          ~min_len:Entry_prefix.min_length ~max_len:Entry_prefix.max_length buf
      with Errors.Pack_error `Read_out_of_bounds ->
        invalid_read
          "Attempted to read an entry at offset %a in the pack file, but got \
           less than %d bytes"
          Int63.pp off Entry_prefix.max_length
    in
    let hash =
      (* Bytes.unsafe_to_string usage: buf is created locally, so we have unique
         ownership; we assume Dispatcher.read_at_most_exn returns unique
         ownership; use of Bytes.unsafe_to_string converts buffer to shared
         ownership; the rest of the code seems to require only shared ownership
         (buffer is read, but not mutated). This is safe. *)
      decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0)
    in
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
          (* Bytes.unsafe_to_string usage: buf is shared at this point; we assume
             Varint.decode_bin requires only shared ownership. This usage is safe. *)
          let length_header =
            Varint.decode_bin (Bytes.unsafe_to_string buf) pos_ref
          in
          let length_header_length = !pos_ref - length_header_start in
          Some (length_header_length + length_header)
    in
    { Entry_prefix.hash; kind; size_of_value_and_length_header }

  (* This function assumes magic is written at hash_size + 1 for every
     object. *)
  let gced t buf =
    let kind = Pack_value.Kind.of_magic_exn (Bytes.get buf Hash.hash_size) in
    match (kind, Fm.gc_behaviour t.fm) with
    | kind, `Delete -> kind = Pack_value.Kind.Dangling_parent_commit
    | _, `Archive -> false

  let pack_file_contains_key t k =
    let off, _, volume_identifier = get_location t k in
    let len = Hash.hash_size + 1 in
    let buf = Bytes.create len in
    let (_volume : Lower.volume_identifier option) =
      Dispatcher.read_exn t.dispatcher ~off ~len ?volume_identifier buf
    in
    if gced t buf then false
    else
      (* Bytes.unsafe_to_string usage: [buf] is local and never reused after
         the call to [decode_bin_hash]. *)
      let hash = decode_bin_hash (Bytes.unsafe_to_string buf) (ref 0) in
      if not (equal_hash hash (Key.to_hash k)) then
        invalid_read
          "invalid key %a checked for membership (read hash %a at this offset \
           instead)"
          pp_key k pp_hash hash;
      (* At this point we consider the key to be contained in the pack
         file. However, we could also be in the presence of a forged (or
         unlucky) key that points to an offset that mimics a real pack
         entry (e.g. in the middle of a blob). *)
      true

  let pack_file_contains_key t k =
    try pack_file_contains_key t k with
    | Dangling_hash -> false
    | Errors.Pack_error `Read_out_of_bounds ->
        (* Can't fit an entry into this suffix of the store, so this key
           isn't (yet) valid. If we're a read-only instance, the key may
           become valid on [reload]; otherwise we know that this key wasn't
           constructed for this store. *)
        (if not (Control.readonly (Fm.control t.fm)) then
         let io_offset = Dispatcher.end_offset t.dispatcher in
         invalid_read "invalid key %a checked for membership (IO offset = %a)"
           pp_key k Int63.pp io_offset);
        false
    | Errors.Pack_error (`Invalid_sparse_read _) -> false
    | Errors.Pack_error (`Invalid_prefix_read _) -> false

  let unsafe_mem t k =
    [%log.debug "[pack] mem %a" pp_key k];
    match Pack_key.inspect k with
    | Indexed hash ->
        (* The key doesn't contain an offset, let's skip the lookup in [lru] and
           go straight to disk read. *)
        Tbl.mem t.staging hash || pack_file_contains_key t k
    | Direct { offset; hash; _ } ->
        Tbl.mem t.staging hash
        || Lru.mem t.lru offset
        || pack_file_contains_key t k

  let mem t k =
    let b = unsafe_mem t k in
    Lwt.return b

  let check_hash h v =
    let h' = Val.hash v in
    if equal_hash h h' then Ok () else Error (h, h')

  let check_key k v = check_hash (Key.to_hash k) v

  (** Produce a key from an offset in the context of decoding inode and commit
      children. *)
  let key_of_offset ?volume_identifier t offset =
    [%log.debug "key_of_offset: %a" Int63.pp offset];
    (* Attempt to eagerly read the length at the same time as reading the
       hash in order to save an extra IO read when dereferencing the key: *)
    let entry_prefix =
      read_and_decode_entry_prefix ?volume_identifier ~off:offset t.dispatcher
    in
    (* This function is called on the parents of a commit when deserialising
       it. Dangling_parent_commit are usually treated as removed objects,
       except here, where in order to correctly deserialise the gced commit,
       they are treated as kept commits.

       Volume identifier is excplicitly set to [None] for dangling parent commits
       so that its data will not be read from the volume (or prefix). When it is
       read, the routing will try to find its location in a previous volume.
    *)
    let kind, volume_identifier =
      match entry_prefix.kind with
      | Pack_value.Kind.Dangling_parent_commit ->
          (Pack_value.Kind.Commit_v2, None)
      | kind -> (kind, volume_identifier)
    in
    let key =
      let entry_prefix = { entry_prefix with kind } in
      match Entry_prefix.total_entry_length entry_prefix with
      | Some length ->
          Pack_key.v_direct ~offset ~length ?volume_identifier entry_prefix.hash
      | None ->
          (* NOTE: we could store [offset] in this key, but since we know the
             entry doesn't have a length header we'll need to check the index
             when dereferencing this key anyway. {i Not} storing the offset
             avoids doing another failed check in the pack file for the length
             header during [find]. *)
          Pack_key.v_indexed entry_prefix.hash
    in
    key

  let find_in_pack_file ~key_of_offset t key =
    let off, len, volume_identifier = get_location t key in
    let buf = Bytes.create len in
    let volume_identifier =
      Dispatcher.read_exn t.dispatcher ~off ~len ?volume_identifier buf
    in
    if gced t buf then None
    else
      let () = Pack_key.set_volume_identifier_exn ~volume_identifier key in
      let key_of_offset = key_of_offset ?volume_identifier t in
      let key_of_hash = Pack_key.v_indexed in
      let dict = Dict.find t.dict in
      let v =
        (* Bytes.unsafe_to_string usage: buf created, uniquely owned; after
           creation, we assume Dispatcher.read_if_not_gced returns unique
           ownership; we give up unique ownership in call to
           [Bytes.unsafe_to_string]. This is safe. *)
        Val.decode_bin ~key_of_offset ~key_of_hash ~dict
          (Bytes.unsafe_to_string buf)
          (ref 0)
      in
      Some v

  let find_in_pack_file ~key_of_offset t key =
    try find_in_pack_file ~key_of_offset t key with
    | Dangling_hash -> None
    | Errors.Pack_error `Read_out_of_bounds -> (
        (* Can't fit an entry into this suffix of the store, so this key
         * isn't (yet) valid. If we're a read-only instance, the key may
         * become valid on [reload]; otherwise we know that this key wasn't
         * constructed for this store. *)
        let io_offset = Dispatcher.end_offset t.dispatcher in
        match Control.readonly (Fm.control t.fm) with
        | false ->
            invalid_read
              "attempt to dereference invalid key %a (IO offset = %a)" pp_key
              key Int63.pp io_offset
        | true ->
            [%log.debug
              "Direct store key references an unknown starting offset %a \
               (length = %d, IO offset = %a)"
              Int63.pp (off_of_direct_key key) (len_of_direct_key key) Int63.pp
                io_offset];
            None)
    | Errors.Pack_error (`Invalid_sparse_read _) -> None
    | Errors.Pack_error (`Invalid_prefix_read _) as e -> raise e

  let unsafe_find ~check_integrity t k =
    [%log.debug "[pack] find %a" pp_key k];
    let find_location = ref Stats.Pack_store.Not_found in
    let find_in_pack_file_guarded ~is_indexed =
      let res = find_in_pack_file ~key_of_offset t k in
      Option.iter
        (fun v ->
          if is_indexed then find_location := Stats.Pack_store.Pack_indexed
          else find_location := Stats.Pack_store.Pack_direct;
          Lru.add t.lru (off_of_direct_key k) v;
          if check_integrity then
            check_key k v |> function
            | Ok () -> ()
            | Error (expected, got) ->
                corrupted_store "Got hash %a, expecting %a (for val: %a)."
                  pp_hash got pp_hash expected pp_value v)
        res;
      res
    in
    let value_opt =
      match Pack_key.inspect k with
      | Indexed hash -> (
          match Tbl.find t.staging hash with
          | v ->
              (* Hit in staging, but we don't have offset to put in LRU *)
              find_location := Stats.Pack_store.Staging;
              Some v
          | exception Not_found -> find_in_pack_file_guarded ~is_indexed:true)
      | Direct { offset; hash; _ } -> (
          match Tbl.find t.staging hash with
          | v ->
              Lru.add t.lru offset v;
              find_location := Stats.Pack_store.Staging;
              Some v
          | exception Not_found -> (
              match Lru.find t.lru offset with
              | v ->
                  find_location := Stats.Pack_store.Lru;
                  Some v
              | exception Not_found ->
                  find_in_pack_file_guarded ~is_indexed:false))
    in
    Stats.report_pack_store ~field:!find_location;
    value_opt

  let unsafe_find_no_prefetch t key =
    let key_of_offset ?volume_identifier:_ _ = Pack_key.v_offset in
    find_in_pack_file ~key_of_offset t key

  let find t k =
    let v = unsafe_find ~check_integrity:true t k in
    Lwt.return v

  let integrity_check ~offset ~length hash t =
    let k = Pack_key.v_direct ~offset ~length hash in
    (* TODO: new error for reading gced objects. *)
    match find_in_pack_file ~key_of_offset t k with
    | exception Errors.Pack_error (`Invalid_prefix_read _) ->
        Error `Absent_value
    | exception Invalid_read _ -> Error `Absent_value
    | None -> Error `Wrong_hash
    | Some value -> (
        match check_hash hash value with
        | Ok () -> Ok ()
        | Error _ -> Error `Wrong_hash)

  let cast t = (t :> read_write t)

  (** [batch] is required by the [Backend] signature of irmin core, but
      irmin-pack is really meant to be used using the [batch] of the repo (in
      [ext.ml]). The following batch exists only for compatibility, but it is
      very tempting to replace the implementation by an [assert false]. *)
  let batch t f =
    [%log.warn
      "[pack] calling batch directory on a store is not recommended. Use \
       repo.batch instead."];
    let on_success res =
      Fm.flush t.fm |> Errs.raise_if_error;
      Lwt.return res
    in
    let on_fail exn =
      [%log.info
        "[pack] batch failed. calling flush. (%s)" (Printexc.to_string exn)];
      let () =
        match Fm.flush t.fm with
        | Ok () -> ()
        | Error err ->
            [%log.err
              "[pack] batch failed and flush failed. Silencing flush fail. (%a)"
                (Irmin.Type.pp Errs.t) err]
      in
      raise exn
    in
    Lwt.try_bind (fun () -> f (cast t)) on_success on_fail

  let unsafe_append ~ensure_unique ~overcommit t hash v =
    let kind = Val.kind v in
    let use_index =
      (* the index is required for non-minimal indexing strategies and
         for commits. *)
      (not (Irmin_pack.Indexing_strategy.is_minimal t.indexing_strategy))
      || kind = Commit_v1
      || kind = Commit_v2
    in
    let unguarded_append () =
      let offset_of_key k =
        match Pack_key.inspect k with
        | Direct { offset; _ } ->
            Stats.incr_appended_offsets ();
            Some offset
        | Indexed hash -> (
            (* TODO: Why don't we promote the key here? *)
            match Index.find (Fm.index t.fm) hash with
            | None ->
                Stats.incr_appended_hashes ();
                None
            | Some (offset, _, _) ->
                Stats.incr_appended_offsets ();
                Some offset)
      in
      let dict = Dict.index t.dict in
      let off = Dispatcher.end_offset t.dispatcher in

      (* [encode_bin] will most likely call [append] several time. One of these
         call may trigger an auto flush. *)
      let append = Suffix.append_exn (Fm.suffix t.fm) in
      Val.encode_bin ~offset_of_key ~dict hash v append;

      let open Int63.Syntax in
      let len = Int63.to_int (Dispatcher.end_offset t.dispatcher - off) in
      let key = Pack_key.v_direct ~offset:off ~length:len hash in
      let () =
        let should_index = t.indexing_strategy ~value_length:len kind in
        if should_index then
          Index.add ~overcommit (Fm.index t.fm) hash (off, len, kind)
      in
      Tbl.add t.staging hash v;
      Lru.add t.lru off v;
      [%log.debug "[pack] append %a" pp_key key];
      key
    in
    match ensure_unique && use_index with
    | false -> unguarded_append ()
    | true ->
        let key = Pack_key.v_indexed hash in
        if unsafe_mem t key then key else unguarded_append ()

  let unsafe_add t hash v =
    unsafe_append ~ensure_unique:true ~overcommit:false t hash v |> Lwt.return

  let add t v = unsafe_add t (Val.hash v) v

  (** This close is a noop.

      Closing the file manager would be inadequate because it is passed to [v].
      The caller should close the file manager.

      We could clear the caches here but that really is not necessary. *)
  let close _ = Lwt.return ()

  let purge_lru t = Lru.clear t.lru
end

module Make
    (Fm : File_manager.S)
    (Dict : Dict.S)
    (Dispatcher : Dispatcher.S with module Fm = Fm)
    (Hash : Irmin.Hash.S with type t = Fm.Index.key)
    (Val : Pack_value.Persistent
             with type hash := Hash.t
              and type key := Hash.t Pack_key.t)
    (Errs : Io_errors.S with module Io = Fm.Io) =
struct
  module Inner =
    Make_without_close_checks (Fm) (Dict) (Dispatcher) (Hash) (Val) (Errs)

  include Inner
  include Indexable.Closeable (Inner)

  let v ~config ~fm ~dict ~dispatcher ~lru =
    Inner.v ~config ~fm ~dict ~dispatcher ~lru |> make_closeable

  let cast t = Inner.cast (get_if_open_exn t) |> make_closeable

  let integrity_check ~offset ~length k t =
    Inner.integrity_check ~offset ~length k (get_if_open_exn t)

  module Entry_prefix = Inner.Entry_prefix

  let read_and_decode_entry_prefix ~off dispatcher =
    Inner.read_and_decode_entry_prefix ~off dispatcher

  let index_direct_with_kind t =
    Inner.index_direct_with_kind (get_if_open_exn t)

  let purge_lru t = Inner.purge_lru (get_if_open_exn t)
  let key_of_offset t offset = Inner.key_of_offset (get_if_open_exn t) offset

  let unsafe_find_no_prefetch t key =
    Inner.unsafe_find_no_prefetch (get_if_open_exn t) key

  let get_offset t key = Inner.get_offset (get_if_open_exn t) key
  let get_length t key = Inner.get_length (get_if_open_exn t) key
end
