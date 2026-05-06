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

(** Lwt-flavoured shim over Irmin 4's [irmin-pack-unix] backend.

    [Maker (Config).Make (Schema)] produces a Lwt-typed Irmin store on top of
    [Irmin_pack_unix.Maker (Config).Make (Schema_eio)] via [Wrap_store.Make],
    plus the [irmin-pack-unix]-specific surface (integrity checks, chunking,
    lower layer, GC, snapshots, statistics) wrapped in [Lwt.t]. Each Eio-direct
    effectful operation in [Inner] becomes a Lwt promise here.

    Some entry points unavoidably leak Eio types because they take Eio
    capabilities as arguments (notably [Eio.Domain_manager.t] and [Eio.Path.t]).
    Users on Lwt who want to call these have to obtain the corresponding Eio
    values from their runner -- this shim does not hide Eio entirely. See
    LIMITATIONS.md. *)

module Conf = Irmin_pack.Conf

(** {1 Maker} *)

module Maker (Config : Irmin_pack.Conf.S) : sig
  type endpoint = unit

  type ('h, 'v) contents_key =
    ('h, 'v) Irmin_pack_unix.Maker(Config).contents_key

  type 'h node_key = 'h Irmin_pack_unix.Maker(Config).node_key
  type 'h commit_key = 'h Irmin_pack_unix.Maker(Config).commit_key

  module Make (S : Irmin_lwt.Schema.S) : sig
    include Irmin_lwt.Generic_key.S with module Schema = S
    (** @inline *)

    (** {1 Integrity checks} *)

    val integrity_check :
      ?ppf:Format.formatter ->
      ?heads:commit list ->
      auto_repair:bool ->
      repo ->
      ( [> `Fixed of int | `No_error ],
        [> `Cannot_fix of string | `Corrupted of int ] )
      result
      Lwt.t

    val integrity_check_inodes :
      ?heads:commit list ->
      repo ->
      ([> `No_error ], [> `Cannot_fix of string ]) result Lwt.t

    val traverse_pack_file :
      [ `Reconstruct_index of [ `In_place | `Output of string ]
      | `Check_index
      | `Check_and_fix_index ] ->
      Irmin_lwt.config ->
      unit Lwt.t

    val test_traverse_pack_file :
      [ `Reconstruct_index of [ `In_place | `Output of string ]
      | `Check_index
      | `Check_and_fix_index ] ->
      Irmin_lwt.config ->
      unit Lwt.t

    (** {1 Chunking / lower layer / on-disk} *)

    val split : repo -> unit Lwt.t
    val is_split_allowed : repo -> bool Lwt.t
    val add_volume : repo -> unit Lwt.t
    val reload : repo -> unit Lwt.t
    val flush : repo -> unit Lwt.t

    val create_one_commit_store :
      domain_mgr:_ Eio.Domain_manager.t ->
      repo ->
      commit_key ->
      Eio.Fs.dir_ty Eio.Path.t ->
      unit Lwt.t

    (** {1 Statistics} *)

    val stats :
      dump_blob_paths_to:string option -> commit:commit -> repo -> unit Lwt.t

    (** {1 Garbage collection} *)

    module Gc : sig
      type process_state =
        [ `Idle | `Running | `Finalised of Irmin_pack_io.Stats.Latest_gc.stats ]

      type msg = [ `Msg of string ]

      val start_exn :
        domain_mgr:_ Eio.Domain_manager.t ->
        ?unlink:bool ->
        repo ->
        commit_key ->
        bool Lwt.t

      val finalise_exn : ?wait:bool -> repo -> process_state Lwt.t

      val run :
        domain_mgr:_ Eio.Domain_manager.t ->
        ?finished:
          ((Irmin_pack_io.Stats.Latest_gc.stats, msg) result -> unit Lwt.t) ->
        repo ->
        commit_key ->
        (bool, msg) result Lwt.t

      val wait :
        repo -> (Irmin_pack_io.Stats.Latest_gc.stats option, msg) result Lwt.t

      val cancel : repo -> bool Lwt.t
      val is_finished : repo -> bool Lwt.t
      val behaviour : repo -> [ `Archive | `Delete ] Lwt.t
      val is_allowed : repo -> bool Lwt.t
      val latest_gc_target : repo -> commit_key option Lwt.t
    end

    (** {1 Snapshots} *)

    module Snapshot : sig
      type kinded_hash = Contents of hash * metadata | Node of hash
      [@@deriving irmin]

      type entry = { step : string; hash : kinded_hash } [@@deriving irmin]

      type inode_tree = {
        depth : int;
        length : int;
        pointers : (int * hash) list;
      }
      [@@deriving irmin]

      type v = Inode_tree of inode_tree | Inode_value of entry list
      [@@deriving irmin]

      type inode = { v : v; root : bool } [@@deriving irmin]

      type t = Inode of inode | Blob of Backend.Contents.Val.t
      [@@deriving irmin]

      val export :
        ?on_disk:[ `Path of Eio.Fs.dir_ty Eio.Path.t ] ->
        repo ->
        (t -> unit Lwt.t) ->
        root_key:Tree.kinded_key ->
        int Lwt.t

      module Import : sig
        type process

        val v :
          ?on_disk:[ `Path of Eio.Fs.dir_ty Eio.Path.t | `Reuse ] ->
          repo ->
          process Lwt.t

        val save_elt : process -> t -> node_key Lwt.t
        val close : process -> repo -> unit Lwt.t
      end
    end
  end
end
