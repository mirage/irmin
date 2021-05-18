(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Maker
    (V : Irmin_pack.Version.S)
    (Node : Irmin.Private.Node.Maker)
    (Commit : Irmin.Private.Commit.Maker)
    (Config : Irmin_pack.Conf.S) =
struct
  type endpoint = unit
  type info = Commit.Info.t

  module Make
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) :
    Irmin_pack.S
      with type key = P.t
       and type contents = C.t
       and type branch = B.t
       and type hash = H.t
       and type step = P.step
       and type metadata = M.t
       and type Key.step = P.step
       and type Private.Remote.endpoint = endpoint
       and type info = info = struct
    module Pack = Content_addressable.Maker (V) (H)
    module Dict = Irmin_pack.Dict.Make (V)

    module X = struct
      module Hash = H
      module Info = Commit.Info

      type 'a value = { hash : H.t; magic : char; v : 'a } [@@deriving irmin]

      module Contents = struct
        module CA = struct
          module CA_Pack = Pack.Make (Irmin_pack.Pack_value.Contents (H) (C))
          include Irmin_pack.Content_addressable.Closeable (CA_Pack)

          let v () = CA_Pack.v () >|= make_closeable
        end

        include Irmin.Contents.Store (CA) (H) (C)
      end

      module Node = struct
        module Node = Node (H) (P) (M)

        module CA = struct
          module Inter = Irmin_pack.Inode.Make_internal (Config) (H) (Node)
          module CA = Pack.Make (Inter.Raw)
          include Irmin_pack.Inode.Make_ext (H) (Node) (Inter) (CA)

          let v = CA.v
        end

        include Irmin.Private.Node.Store (Contents) (CA) (H) (CA.Val) (M) (P)
      end

      module Commit = struct
        module Commit = Commit.Make (H)

        module CA = struct
          module CA_Pack = Pack.Make (Irmin_pack.Pack_value.Commit (H) (Commit))
          include Irmin_pack.Content_addressable.Closeable (CA_Pack)

          let v () = CA_Pack.v () >|= make_closeable
        end

        include Irmin.Private.Commit.Store (Info) (Node) (CA) (H) (Commit)
      end

      module Branch = struct
        module Key = B
        module Val = H
        module AW = Atomic_write.Make (Key) (Val)
        include Irmin_pack.Atomic_write.Closeable (AW)
      end

      module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Private.Remote.None (H) (B)

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
          let root = Irmin_pack.Conf.root config in
          let fresh = Irmin_pack.Conf.fresh config in
          let readonly = Irmin_pack.Conf.readonly config in
          let* contents = Contents.CA.v () in
          let* node = Node.CA.v () in
          let* commit = Commit.CA.v () in
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
              | Irmin_pack.Version.Invalid { expected; found } as e
                when expected = V.version ->
                  Log.err (fun m ->
                      m "[%s] Attempted to open store of unsupported version %a"
                        (Irmin_pack.Conf.root config)
                        Irmin_pack.Version.pp found);
                  Lwt.fail e
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
        Irmin_pack.Utils.Progress.increment ()
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
      Irmin_pack.Utils.Progress.finalise bar;
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
    let migrate = Irmin_pack.migrate
    let flush = X.Repo.flush
    let integrity_check ?ppf:_ ~auto_repair:_ _t = Ok `No_error
    let reconstruct_index ?output:_ _ = ()
  end
end
