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

open! Import
open Snapshot_intf

let rm_index path =
  let path_index = Filename.concat path "index" in
  Sys.readdir path_index
  |> Array.iter (fun name -> Unix.unlink (Filename.concat path_index name));
  Unix.rmdir path_index;
  Unix.rmdir path

module Make (Args : Args) = struct
  module Hashes = Irmin.Hash.Set.Make (Args.Hash)
  open Args
  module Inode_pack = Inode.Pack
  module Pack_index = Pack_index.Make (Hash)

  let pp_hash = Irmin.Type.pp Hash.t
  let pp_key = Irmin.Type.pp Inode_pack.Key.t
  let pp_kind = Irmin.Type.pp Pack_value.Kind.t
  let pp_snapshot = Irmin.Type.pp Inode.Snapshot.inode_t

  module Export = struct
    module Value_unit = struct
      type t = unit [@@deriving irmin]

      let encode _ = ""
      let encoded_size = 0
      let decode _ _ = ()
    end

    module Index =
      Index_unix.Make (Pack_index.Key) (Value_unit) (Index.Cache.Unbounded)

    type t = {
      fm : Fm.t;
      dispatcher : Dispatcher.t;
      log_size : int;
      inode_pack : read Inode_pack.t;
      contents_pack : read Contents_pack.t;
    }

    let v config contents_pack inode_pack =
      (* In order to read from the pack files, we need to open at least two
         files: suffix and control. We just open the file manager for
         simplicity. *)
      let fm = Fm.open_ro config |> Fm.Errs.raise_if_error in
      let dispatcher = Dispatcher.v fm |> Fm.Errs.raise_if_error in
      let log_size = Conf.index_log_size config in
      { fm; dispatcher; log_size; inode_pack; contents_pack }

    let close t = Fm.close t.fm

    let key_of_hash hash t =
      Inode_pack.index_direct_with_kind t hash |> Option.get

    let length_of_hash hash t =
      let key, _ = key_of_hash hash t in
      match Pack_key.inspect key with
      | Indexed _ ->
          (* This case cannot happen, as [key_of_hash] converts an
             indexed key to a direct one. *)
          assert false
      | Direct { length; _ } -> length

    let io_read_and_decode_entry_prefix ~off t =
      let entry_prefix : Inode_pack.Entry_prefix.t =
        Inode_pack.read_and_decode_entry_prefix ~off t.dispatcher
      in
      let length =
        match Inode_pack.Entry_prefix.total_entry_length entry_prefix with
        | Some length -> length
        | None ->
            (* If the length is not on disk, the object is in index. *)
            length_of_hash entry_prefix.hash t.inode_pack
      in
      let key = Pack_key.v_direct ~offset:off ~length entry_prefix.hash in
      (key, entry_prefix.kind)

    (* Get the childrens offsets and then read their keys at that offset. *)
    let decode_children_offsets ~off ~len t =
      let buf = Bytes.create len in
      let _ = Dispatcher.read_exn t.dispatcher ~off ~len buf in
      let entry_of_offset offset =
        [%log.debug "key_of_offset: %a" Int63.pp offset];
        io_read_and_decode_entry_prefix ~off:offset t
      in
      let entry_of_hash hash = key_of_hash hash t.inode_pack in
      (* Bytes.unsafe_to_string usage: buf is created locally, uniquely owned; we assume
         Dispatcher.read_exn returns unique ownership; then call to Bytes.unsafe_to_string
         gives up unique ownership of buf. This is safe. *)
      Inode.Raw.decode_children_offsets ~entry_of_offset ~entry_of_hash
        (Bytes.unsafe_to_string buf) (* safe: see comment above *)
        (ref 0)

    type visit = { visited : Hash.t -> bool; set_visit : Hash.t -> unit }

    let iter t v f_contents f_inodes (root_key, root_kind) =
      let total_visited = ref 0 in
      let set_visit h =
        incr total_visited;
        v.set_visit h
      in
      let rec aux (key, kind) =
        match Pack_key.inspect key with
        | Indexed _ ->
            (* This case cannot happen:
               - either the root key is indexed, in which case it converted to a
               direct key just before the call to [aux];
               - or one of the children of a node is indexed, in which case
               [Inode.Raw.decode_children_offsets] converts it to a direct key
               before the call to [aux]. *)
            assert false
        | Direct { length; offset; hash; _ } ->
            if v.visited hash then Lwt.return_unit
            else (
              set_visit hash;
              [%log.debug "visit hash: %a, %a" pp_hash hash pp_kind kind];
              (* [unsafe_find] decodes the values based on their kind, we need
                 to detect the type in order to call the correspoding
                 [unsafe_find].*)
              match kind with
              | Contents -> (
                  let value =
                    Contents_pack.unsafe_find ~check_integrity:false
                      t.contents_pack key
                  in
                  match value with
                  | None ->
                      Fmt.failwith "contents not found in store. Key: %a "
                        pp_key key
                  | Some value ->
                      let snapshot_blob = value in
                      f_contents snapshot_blob)
              | Inode_v1_unstable | Inode_v1_stable | Inode_v2_root
              | Inode_v2_nonroot -> (
                  let children =
                    decode_children_offsets ~off:offset ~len:length t
                  in
                  let* () = Lwt_list.iter_s (fun key -> aux key) children in
                  let value =
                    Inode_pack.unsafe_find ~check_integrity:false t.inode_pack
                      key
                  in
                  match value with
                  | None ->
                      Fmt.failwith "node not found in store. Key: %a " pp_key
                        key
                  | Some value ->
                      let snapshot_inode = Inode.to_snapshot value in
                      [%log.debug
                        "iter inode snapshot: %a" pp_snapshot snapshot_inode];
                      f_inodes snapshot_inode)
              | Commit_v1 | Commit_v2 ->
                  (* The traversal starts with a node, it never iters over
                     commits. *)
                  assert false
              | Dangling_parent_commit -> assert false)
      in
      (* In case the root node of a tree is indexed, we need to convert it to a
         direct key first. *)
      let root_key =
        match Pack_key.inspect root_key with
        | Indexed hash -> key_of_hash hash t.inode_pack |> fst
        | Direct _ -> root_key
      in
      let* () = aux (root_key, root_kind) in
      Lwt.return !total_visited

    let run_in_memory t f_contents f_inodes root_key =
      [%log.info "iter in memory"];
      let visited_hash = Hashes.create ~initial_slots:100_000 () in
      let visited h = Hashes.mem visited_hash h in
      let set_visit h =
        match Hashes.add visited_hash h with
        | `Duplicate ->
            Fmt.failwith "should not visit hash twice. Hash: %a " pp_hash h
        | `Ok -> ()
      in
      iter t { visited; set_visit } f_contents f_inodes root_key

    let run_on_disk path t f_contents f_inodes root_key =
      [%log.info "iter on disk"];
      let index =
        Index.v ~fresh:true ~readonly:false ~log_size:t.log_size path
      in
      let visited h = Index.mem index h in
      let set_visit h =
        if visited h then
          Fmt.failwith "Should not visit hash twice. Hash: %a " pp_hash h
        else Index.replace index h ()
      in
      let* total = iter t { visited; set_visit } f_contents f_inodes root_key in
      Index.close index;
      rm_index path;
      Lwt.return total

    let run ?on_disk =
      match on_disk with
      | None -> run_in_memory
      | Some (`Path path) -> run_on_disk path
  end

  module Import = struct
    module Value = struct
      type t = int63 * int [@@deriving irmin]

      let encoded_size = (64 / 8) + (32 / 8)

      let encode ((off, len) : t) =
        let buf = Bytes.create encoded_size in
        Bytes.set_int64_be buf 0 (Int63.to_int64 off);
        Bytes.set_int32_be buf 8 (Int32.of_int len);
        (* Bytes.unsafe_to_string usage: buf is local, uniquely owned; we assume the
           Bytes.set... functions return unique ownership; then Bytes.unsafe_to_string
           gives up unique ownership of buf to get shared ownership of the resulting
           string, which is then returned. buf is no longer accessible. This is safe. *)
        Bytes.unsafe_to_string buf

      let decode s pos : t =
        (* Bytes.unsafe_of_string usage: s is shared; buf is shared (we cannot mutate it);
           we assume Bytes.get_... functions need shared ownership only. This usage is
           safe. *)
        let buf = Bytes.unsafe_of_string s in
        let off = Bytes.get_int64_be buf pos |> Int63.of_int64 in
        let len = Bytes.get_int32_be buf (pos + 8) |> Int32.to_int in
        (off, len)
    end

    module Index =
      Index_unix.Make (Pack_index.Key) (Value) (Index.Cache.Unbounded)

    type path = string

    type t = {
      inode_pack : read Inode_pack.t;
      contents_pack : read Contents_pack.t;
      visited : Hash.t -> Hash.t Pack_key.t;
      set_visit : Hash.t -> Hash.t Pack_key.t -> unit;
      index : (path * Index.t) option;
    }

    let save_contents t b : Hash.t Pack_key.t Lwt.t =
      let* key =
        Contents_pack.batch t.contents_pack (fun writer ->
            Contents_pack.add writer b)
      in
      let hash = Inode.Key.to_hash key in
      t.set_visit hash key;
      Lwt.return key

    let save_inodes t i : Hash.t Pack_key.t Lwt.t =
      let inode = Inode.of_snapshot t.inode_pack ~index:t.visited i in
      let key = Inode.save ~allow_non_root:true t.inode_pack inode in
      let hash = Inode.Key.to_hash key in
      t.set_visit hash key;
      Lwt.return key

    let hash_not_found h =
      Fmt.failwith
        "You are trying to save to the backend an inode that contains pointers \
         to objects unknown to the backend. Hash: %a"
        pp_hash h

    let save_reuse_index inodes =
      [%log.info "save reuse index "];
      (* objects are added to index by [save_contents] and [save_inodes]
         functions. *)
      let set_visit _ _ = () in
      let visited h =
        match Inode_pack.index_direct inodes h with
        | Some x -> x
        | None -> hash_not_found h
      in
      (set_visit, visited, None)

    let save_in_memory () =
      [%log.info "save in memory"];
      let tbl : (Hash.t, Hash.t Pack_key.t) Hashtbl.t = Hashtbl.create 10 in
      let set_visit h k = Hashtbl.add tbl h k in
      let visited h =
        match Hashtbl.find_opt tbl h with
        | Some x -> x
        | None -> hash_not_found h
      in
      (set_visit, visited, None)

    let save_on_disk log_size path =
      (* Make sure we are not reusing the same index as irmin-pack. *)
      let path = path ^ "_tmp" in
      [%log.info "save on disk: %s" path];
      let index = Index.v ~fresh:true ~readonly:false ~log_size path in

      let set_visit h k =
        let offset, length =
          match Pack_key.inspect k with
          | Direct { offset; length; _ } -> (offset, length)
          | Indexed _ ->
              (* Visited objects have direct keys. *)
              assert false
        in
        Index.replace index h (offset, length)
      in
      let visited h =
        try
          let offset, length = Index.find index h in
          let key = Pack_key.v_direct ~offset ~length h in
          key
        with Not_found -> hash_not_found h
      in
      (set_visit, visited, Some (path, index))

    let v ?on_disk log_size contents_pack inode_pack =
      let set_visit, visited, index =
        match on_disk with
        | None -> save_in_memory ()
        | Some (`Path path) -> save_on_disk log_size path
        | Some `Reuse -> save_reuse_index inode_pack
      in
      { inode_pack; contents_pack; visited; set_visit; index }

    let close t =
      Option.iter
        (fun (path, index) ->
          Index.close index;
          rm_index path)
        t.index
  end
end
