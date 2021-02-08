(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)
module I = Irmin_pack.Private.IO

let pp_version = Irmin_pack.Private.IO.pp_version

exception Unsupported_version = Irmin_pack.Store.Unsupported_version

module Make
    (IO_version : Irmin_pack.VERSION)
    (Config : Irmin_pack.Config.S)
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
  module Pack = Pack_mem.File (H) (IO_version)
  module Dict = Irmin_pack.Dict.Make (IO_version)

  let current_version = IO_version.io_version

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

        include Irmin_pack.Private.Closeable.Content_addressable (CA_Pack)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module CA = Irmin_pack.Private.Inode.Make (Config) (H) (Pack) (Node)
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

        include Irmin_pack.Private.Closeable.Content_addressable (CA_Pack)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Irmin_pack.Store.Atomic_write (Key) (Val) (IO_version)
      include Irmin_pack.Private.Closeable.Atomic_write (AW)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : read Contents.CA.t;
        node : read Node.CA.t;
        commit : read Commit.CA.t;
        branch : Branch.t;
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
        let root = Irmin_pack.Config.root config in
        let fresh = Irmin_pack.Config.fresh config in
        let readonly = Irmin_pack.Config.readonly config in
        let* contents = Contents.CA.v ~fresh ~readonly root in
        let* node = Node.CA.v ~fresh ~readonly root in
        let* commit = Commit.CA.v ~fresh ~readonly root in
        let+ branch = Branch.v ~fresh ~readonly root in
        { contents; node; commit; branch; config }

      let close t =
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
                      (Irmin_pack.Config.root config)
                      pp_version found);
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

  include Irmin.Of_private (X)

  let integrity_check_inodes ?heads t =
    Log.debug (fun l -> l "Check integrity for inodes");
    let bar, (_, progress_nodes, progress_commits) =
      Irmin_pack.Private.Utils.Progress.increment ()
    in
    let errors = ref [] in
    let nodes = X.Repo.node_t t |> snd in
    let node k =
      progress_nodes ();
      X.Node.CA.integrity_check_inodes nodes k >|= function
      | Ok () -> ()
      | Error msg -> errors := msg :: !errors
    in
    let commit _ =
      progress_commits ();
      Lwt.return_unit
    in
    let* heads =
      match heads with None -> Repo.heads t | Some m -> Lwt.return m
    in
    let hashes = List.map (fun x -> `Commit (Commit.hash x)) heads in
    let+ () =
      Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~node ~commit t
    in
    Irmin_pack.Private.Utils.Progress.finalise bar;
    let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash in
    if !errors = [] then
      Fmt.kstrf (fun x -> Ok (`Msg x)) "Ok for heads %a" pp_commits heads
    else
      Fmt.kstrf
        (fun x -> Error (`Msg x))
        "Inconsistent inodes found for heads %a: %a" pp_commits heads
        Fmt.(list ~sep:comma string)
        !errors

  let sync = X.Repo.sync
  let clear = X.Repo.clear
  let migrate = Irmin_pack.Store.migrate
  let flush = X.Repo.flush
end

module Pack = Pack_mem
