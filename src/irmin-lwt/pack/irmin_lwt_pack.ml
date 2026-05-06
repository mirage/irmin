(*
 * Copyright (c) 2026 Tarides
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

module Conf = Irmin_pack.Conf

let runeio = Lwt_eio.run_eio
let await = Lwt_eio.Promise.await_lwt

module Maker (Config : Irmin_pack.Conf.S) = struct
  type endpoint = unit

  module Inner_maker = Irmin_pack_unix.Maker (Config)

  type ('h, 'v) contents_key = ('h, 'v) Inner_maker.contents_key
  type 'h node_key = 'h Inner_maker.node_key
  type 'h commit_key = 'h Inner_maker.commit_key

  module Make (S : Irmin_lwt.Schema.S) = struct
    module Schema_eio = Irmin_lwt.Lwt_to_eio.Schema_extended (S)
    module Inner = Inner_maker.Make (Schema_eio)
    include Irmin_lwt.Wrap_store.Make (S) (Schema_eio) (Inner)

    (* {1 irmin-pack-unix advanced surface, bridged to Lwt} *)

    (* {2 Integrity checks} *)

    let integrity_check ?ppf ?heads ~auto_repair r =
      runeio (fun () -> Inner.integrity_check ?ppf ?heads ~auto_repair r)

    let integrity_check_inodes ?heads r =
      runeio (fun () -> Inner.integrity_check_inodes ?heads r)

    let traverse_pack_file mode config =
      runeio (fun () -> Inner.traverse_pack_file mode config)

    let test_traverse_pack_file mode config =
      runeio (fun () -> Inner.test_traverse_pack_file mode config)

    (* {2 Chunking / lower layer / on-disk} *)

    let split r = runeio (fun () -> Inner.split r)
    let is_split_allowed r = runeio (fun () -> Inner.is_split_allowed r)
    let add_volume r = runeio (fun () -> Inner.add_volume r)
    let reload r = runeio (fun () -> Inner.reload r)
    let flush r = runeio (fun () -> Inner.flush r)

    let create_one_commit_store ~domain_mgr r ck path =
      runeio (fun () -> Inner.create_one_commit_store ~domain_mgr r ck path)

    (* {2 Statistics} *)

    let stats ~dump_blob_paths_to ~commit r =
      runeio (fun () -> Inner.stats ~dump_blob_paths_to ~commit r)

    (* {2 Garbage collection} *)

    module Gc = struct
      type process_state = Inner.Gc.process_state
      type msg = Inner.Gc.msg

      let start_exn ~domain_mgr ?unlink r ck =
        runeio (fun () -> Inner.Gc.start_exn ~domain_mgr ?unlink r ck)

      let finalise_exn ?wait r' =
        runeio (fun () -> Inner.Gc.finalise_exn ?wait r')

      (* [Inner.Gc.run]'s [?finished] callback runs in Eio direct-style.
         Bridge it: user gives us a Lwt-typed callback, we await its
         promise inside Eio. *)
      let run ~domain_mgr ?finished r ck =
        let finished_eio =
          Option.map (fun f result -> await (f result)) finished
        in
        runeio (fun () -> Inner.Gc.run ~domain_mgr ?finished:finished_eio r ck)

      let wait r = runeio (fun () -> Inner.Gc.wait r)
      let cancel r = runeio (fun () -> Inner.Gc.cancel r)
      let is_finished r = runeio (fun () -> Inner.Gc.is_finished r)
      let behaviour r = runeio (fun () -> Inner.Gc.behaviour r)
      let is_allowed r = runeio (fun () -> Inner.Gc.is_allowed r)
      let latest_gc_target r = runeio (fun () -> Inner.Gc.latest_gc_target r)
    end

    (* {2 Snapshots} *)

    module Snapshot = struct
      type kinded_hash = Inner.Snapshot.kinded_hash =
        | Contents of hash * metadata
        | Node of hash
      [@@deriving irmin]

      type entry = Inner.Snapshot.entry = { step : string; hash : kinded_hash }
      [@@deriving irmin]

      type inode_tree = Inner.Snapshot.inode_tree = {
        depth : int;
        length : int;
        pointers : (int * hash) list;
      }
      [@@deriving irmin]

      type v = Inner.Snapshot.v =
        | Inode_tree of inode_tree
        | Inode_value of entry list
      [@@deriving irmin]

      type inode = Inner.Snapshot.inode = { v : v; root : bool }
      [@@deriving irmin]

      type t = Inner.Snapshot.t =
        | Inode of inode
        | Blob of Inner.Backend.Contents.Val.t
      [@@deriving irmin]

      let export ?on_disk r f ~root_key =
        runeio (fun () ->
            Inner.Snapshot.export ?on_disk r
              (fun elt -> await (f elt))
              ~root_key)

      module Import = struct
        type process = Inner.Snapshot.Import.process

        let v ?on_disk r = runeio (fun () -> Inner.Snapshot.Import.v ?on_disk r)
        let save_elt p t = runeio (fun () -> Inner.Snapshot.Import.save_elt p t)
        let close p r = runeio (fun () -> Inner.Snapshot.Import.close p r)
      end
    end
  end
end
