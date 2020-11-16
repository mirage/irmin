(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = `V2

let pp_version = IO.pp_version

module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index

exception RO_Not_Allowed = IO.Unix.RO_Not_Allowed

exception Unsupported_version = Store.Unsupported_version

let () =
  Printexc.register_printer (function
    | Unsupported_version v ->
        Some (Fmt.str "Irmin_pack.Unsupported_version(%a)" IO.pp_version v)
    | _ -> None)

module I = IO
module IO = IO.Unix
open Lwt.Infix
module Atomic_write = Store.Atomic_write

let config = Config.v

module Pack_config = Config

module Make_ext
    (Config : Config.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S
              with type metadata = M.t
               and type hash = H.t
               and type step = P.step)
    (Commit : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module Index = Pack_index.Make (H)
  module Pack = Pack.File (Index) (H)

  module X = struct
    module Hash = H

    type 'a value = { hash : H.t; magic : char; v : 'a } [@@deriving irmin]

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value_t Val.t

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = decode_value s off in
            t.v

          let magic _ = magic
        end)

        include Closeable.Content_addressable (CA_Pack)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module CA = Inode.Make (Config) (H) (Pack) (Node)
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value_t Val.t

          let magic = 'C'

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = decode_value s off in
            v.v

          let magic _ = magic
        end)

        include Closeable.Content_addressable (CA_Pack)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Store.Atomic_write (Key) (Val)
      include Closeable.Atomic_write (AW)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        commit : [ `Read ] Commit.CA.t;
        branch : Branch.t;
        index : Index.t;
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.node)

      let commit_t t : 'a Commit.t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Commit.CA.batch t.commit (fun commit ->
            Node.CA.batch t.node (fun node ->
                Contents.CA.batch t.contents (fun contents ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

      let unsafe_v config =
        let root = Pack_config.root config in
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        let log_size = Pack_config.index_log_size config in
        let throttle = Pack_config.index_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~throttle ~log_size root
        in
        Contents.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun contents ->
        Node.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun node ->
        Commit.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun commit ->
        Branch.v ~fresh ~readonly root >|= fun branch ->
        (* Stores share instances in memory, one flush is enough. In case of a
           system crash, the flush_callback might not make with the disk. In
           this case, when the store is reopened, [integrity_check] needs to be
           called to repair the store. *)
        (f := fun () -> Contents.CA.flush ~index:false contents);
        { contents; node; commit; branch; config; index }

      let close t =
        Index.close t.index;
        Contents.CA.close (contents_t t) >>= fun () ->
        Node.CA.close (snd (node_t t)) >>= fun () ->
        Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch

      let v config =
        Lwt.catch
          (fun () -> unsafe_v config)
          (function
            | I.Invalid_version { expected; found }
              when expected = current_version ->
                Log.err (fun m ->
                    m "[%s] Attempted to open store of unsupported version %a"
                      (Pack_config.root config) pp_version found);
                Lwt.fail (Unsupported_version found)
            | e -> Lwt.fail e)

      (** Stores share instances in memory, one sync is enough. However each
          store has its own lru and all have to be cleared. *)
      let sync t =
        let on_generation_change () =
          Node.CA.clear_caches (snd (node_t t));
          Commit.CA.clear_caches (snd (commit_t t))
        in
        Contents.CA.sync ~on_generation_change (contents_t t)

      (** Stores share instances so one clear is enough. *)
      let clear t = Contents.CA.clear (contents_t t)

      let flush t =
        Contents.CA.flush (contents_t t);
        Branch.flush t.branch
    end
  end

  let integrity_check ?ppf ~auto_repair t =
    let module Checks = Store.Checks (Index) in
    let contents = X.Repo.contents_t t in
    let nodes = X.Repo.node_t t |> snd in
    let commits = X.Repo.commit_t t |> snd in
    let check ~kind ~offset ~length k =
      match kind with
      | `Contents -> X.Contents.CA.integrity_check ~offset ~length k contents
      | `Node -> X.Node.CA.integrity_check ~offset ~length k nodes
      | `Commit -> X.Commit.CA.integrity_check ~offset ~length k commits
    in
    Checks.integrity_check ?ppf ~auto_repair ~check t.index

  include Irmin.Of_private (X)

  let sync = X.Repo.sync

  let clear = X.Repo.clear

  let migrate = Store.migrate

  let flush = X.Repo.flush
end

module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Make
    (Config : Config.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module XNode = Irmin.Private.Node.Make (H) (P) (M)
  module XCommit = Irmin.Private.Commit.Make (H)
  include Make_ext (Config) (M) (C) (P) (B) (H) (XNode) (XCommit)
end

module KV (Config : Config.S) (C : Irmin.Contents.S) =
  Make (Config) (Metadata) (C) (Path) (Irmin.Branch.String) (Hash)
module Stats = Stats

module Private = struct
  module Utils = Utils
end

module Make_ext_layered = Irmin_pack_layers.Make_ext
module Make_layered = Irmin_pack_layers.Make

let config_layers = Irmin_pack_layers.config_layers

module Make_checks = Checks.Make
