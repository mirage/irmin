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
include Ext_intf
module IO = IO.Unix

module Maker (V : Version.S) = struct
  type endpoint = unit

  module Make (Schema : Schema.S) = struct
    module X = struct
      module Schema = Schema
      module Hash = Schema.Hash
      module Index = Pack_index.Make (Hash)
      module Pack = Content_addressable.Maker (V) (Index) (Hash)
      module Dict = Pack_dict.Make (V)

      type 'a value = { hash : Hash.t; magic : char; v : 'a } [@@deriving irmin]

      open Schema

      module Contents = struct
        module CA = struct
          module CA_Pack = Pack.Make (struct
            include Contents
            module H = Irmin.Hash.Typed (Hash) (Contents)

            let hash = H.hash
            let magic = 'B'
            let value = value_t Contents.t
            let encode_value = Irmin.Type.(unstage (encode_bin value))
            let decode_value = Irmin.Type.(unstage (decode_bin value))

            let encode_bin ~dict:_ ~offset:_ v hash =
              encode_value { magic; hash; v }

            let decode_bin ~dict:_ ~hash:_ s off =
              let off, t = decode_value s off in
              (off, t.v)

            let magic _ = magic
          end)

          include Content_addressable.Closeable (CA_Pack)
        end

        include Irmin.Contents.Store (CA) (Hash) (Contents)
      end

      module Node = struct
        module Raw = Pack.Make (Schema.Node.Raw)
        include Inode.Store (Schema) (Contents) (Raw)
      end

      module Commit = struct
        module CA = struct
          module CA_Pack = Pack.Make (struct
            include Commit
            module H = Irmin.Hash.Typed (Hash) (Commit)

            let hash = H.hash
            let value = value_t Commit.t
            let magic = 'C'
            let encode_value = Irmin.Type.(unstage (encode_bin value))
            let decode_value = Irmin.Type.(unstage (decode_bin value))

            let encode_bin ~dict:_ ~offset:_ v hash =
              encode_value { magic; hash; v }

            let decode_bin ~dict:_ ~hash:_ s off =
              let off, v = decode_value s off in
              (off, v.v)

            let magic _ = magic
          end)

          include Content_addressable.Closeable (CA_Pack)
        end

        include Irmin.Commit.Store (Info) (Node) (CA) (Hash) (Commit)
      end

      module Branch = struct
        module Key = Branch
        module Val = Hash
        module AW = Atomic_write.Make (V) (Key) (Val)
        include Atomic_write.Closeable (AW)
      end

      module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Private.Remote.None (Hash) (Branch.Key)

      module Repo = struct
        type t = {
          config : Irmin.Private.Conf.t;
          contents : read Contents.CA.t;
          node : read Node.Raw.t;
          commit : read Commit.CA.t;
          branch : Branch.t;
          index : Index.t;
        }

        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch

        let batch t f =
          Commit.CA.batch t.commit (fun commit ->
              Node.Raw.batch t.node (fun node ->
                  Contents.CA.batch t.contents (fun contents ->
                      let contents : 'a Contents.t = contents in
                      let node : 'a Node.t = (contents, node) in
                      let commit : 'a Commit.t = (node, commit) in
                      f contents node commit)))

        let unsafe_v config =
          let root = Conf.root config in
          let fresh = Conf.fresh config in
          let lru_size = Conf.lru_size config in
          let readonly = Conf.readonly config in
          let log_size = Conf.index_log_size config in
          let throttle = Conf.merge_throttle config in
          let f = ref (fun () -> ()) in
          let index =
            Index.v
              ~flush_callback:(fun () -> !f ())
                (* backpatching to add pack flush before an index flush *)
              ~fresh ~readonly ~throttle ~log_size root
          in
          let* contents =
            Contents.CA.v ~fresh ~readonly ~lru_size ~index root
          in
          let* node = Node.Raw.v ~fresh ~readonly ~lru_size ~index root in
          let* commit = Commit.CA.v ~fresh ~readonly ~lru_size ~index root in
          let+ branch = Branch.v ~fresh ~readonly root in
          (* Stores share instances in memory, one flush is enough. In case of a
             system crash, the flush_callback might not make with the disk. In
             this case, when the store is reopened, [integrity_check] needs to be
             called to repair the store. *)
          (f := fun () -> Contents.CA.flush ~index:false contents);
          { contents; node; commit; branch; config; index }

        let close t =
          Index.close t.index;
          Contents.CA.close (contents_t t) >>= fun () ->
          Node.close (node_t t) >>= fun () ->
          Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch

        let v config =
          Lwt.catch
            (fun () -> unsafe_v config)
            (function
              | Version.Invalid { expected; found } as e
                when expected = V.version ->
                  Log.err (fun m ->
                      m "[%s] Attempted to open store of unsupported version %a"
                        (Conf.root config) Version.pp found);
                  Lwt.fail e
              | e -> Lwt.fail e)

        (** Stores share instances in memory, one sync is enough. However each
            store has its own lru and all have to be cleared. *)
        let sync t =
          let on_generation_change () =
            Node.Raw.clear_caches (snd (node_t t));
            Commit.CA.clear_caches (snd (commit_t t))
          in
          Contents.CA.sync ~on_generation_change (contents_t t)

        (** Stores share instances so one clear is enough. *)
        let clear t = Contents.CA.clear (contents_t t)

        let flush t =
          Contents.CA.flush (contents_t t);
          Branch.flush t.branch

        module Reconstruct_index = struct
          let pp_hash = Irmin.Type.pp Hash.t

          let decode_contents =
            Irmin.Type.(unstage (decode_bin (value_t Contents.Val.t)))

          let decode_commit =
            Irmin.Type.(unstage (decode_bin (value_t Commit.Val.t)))

          let decode_key = Irmin.Type.(unstage (decode_bin Hash.t))
          let decode_magic = Irmin.Type.(unstage (decode_bin char))

          let decode_buffer ~progress ~total pack dict index =
            let decode_len buf magic =
              try
                let len =
                  match magic with
                  | 'B' -> decode_contents buf 0 |> fst
                  | 'C' -> decode_commit buf 0 |> fst
                  | 'N' | 'I' ->
                      let hash off =
                        let buf =
                          IO.read_buffer ~chunk:Hash.hash_size ~off pack
                        in
                        decode_key buf 0 |> snd
                      in
                      let dict = Dict.find dict in
                      Schema.Node.Raw.decode_bin ~hash ~dict buf 0 |> fst
                  | _ -> failwith "unexpected magic char"
                in
                Some len
              with
              | Invalid_argument msg when msg = "index out of bounds" -> None
              | Invalid_argument msg
                when msg = "String.blit / Bytes.blit_string" ->
                  None
            in
            let decode_entry buf off =
              let off_k, k = decode_key buf 0 in
              assert (off_k = Hash.hash_size);
              let off_m, magic = decode_magic buf off_k in
              assert (off_m = Hash.hash_size + 1);
              match decode_len buf magic with
              | Some len ->
                  let new_off = off ++ Int63.of_int len in
                  Log.debug (fun l ->
                      l "k = %a (off, len, magic) = (%a, %d, %c)" pp_hash k
                        Int63.pp off len magic);
                  Index.add index k (off, len, magic);
                  progress (Int63.of_int len);
                  Some new_off
              | None -> None
            in
            let rec read_and_decode ?(retries = 1) off =
              Log.debug (fun l ->
                  l "read_and_decode retries %d off %a" retries Int63.pp off);
              let chunk = 64 * 10 * retries in
              let buf = IO.read_buffer ~chunk ~off pack in
              match decode_entry buf off with
              | Some new_off -> new_off
              | None ->
                  (* the biggest entry in a tezos store is a blob of 54801B *)
                  if retries > 90 then
                    failwith
                      "too many retries to read data, buffer size = 57600B"
                  else (read_and_decode [@tailcall]) ~retries:(retries + 1) off
            in
            let rec read_buffer off =
              if off >= total then ()
              else
                let new_off = read_and_decode off in
                (read_buffer [@tailcall]) new_off
            in
            read_buffer Int63.zero

          let reconstruct ?output config =
            if Conf.readonly config then raise S.RO_not_allowed;
            Log.info (fun l -> l "[%s] reconstructing index" (Conf.root config));
            let root = Conf.root config in
            let dest = match output with Some path -> path | None -> root in
            let log_size = Conf.index_log_size config in
            let index = Index.v ~fresh:true ~readonly:false ~log_size dest in
            let pack_file = Filename.concat root "store.pack" in
            let pack =
              IO.v ~fresh:false ~readonly:true ~version:(Some V.version)
                pack_file
            in
            let dict = Dict.v ~fresh:false ~readonly:true root in
            let total = IO.offset pack in
            let bar, progress =
              Utils.Progress.counter ~total ~sampling_interval:100
                ~message:"Reconstructing index" ~pp_count:Utils.pp_bytes ()
            in
            decode_buffer ~progress ~total pack dict index;
            Index.close index;
            IO.close pack;
            Dict.close dict;
            Utils.Progress.finalise bar
        end
      end
    end

    include Irmin.Of_private (X)
    module Schema = X.Schema
    module Index = Checks.Index (X.Index)

    let integrity_check_by_commit ~heads t =
      Log.debug (fun l -> l "Check integrity for inodes");
      let bar, (_, progress_nodes, progress_commits) =
        Utils.Progress.increment ()
      in
      let errors = ref [] in
      let nodes = X.Repo.node_t t in
      let node k =
        progress_nodes ();
        Private.Node.find nodes k >|= function
        | None -> assert false
        | Some v ->
            if Schema.Node.integrity_check v then ()
            else
              let msg =
                Fmt.str "Problematic inode %a" (Irmin.Type.pp Schema.Node.t) v
              in
              errors := msg :: !errors
      in
      let commit _ =
        progress_commits ();
        Lwt.return_unit
      in
      let hashes = List.map (fun x -> `Commit (Commit.hash x)) heads in
      let+ () =
        Repo.iter ~cache_size:1_000_000 ~min:[] ~max:hashes ~node ~commit t
      in
      Utils.Progress.finalise bar;
      let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash in
      if !errors = [] then Ok `No_error
      else (
        Log.err (fun l ->
            l "Inconsistent inodes found for heads %a: %a" pp_commits heads
              Fmt.(list ~sep:comma string)
              !errors);
        Error (`Corrupted (List.length !errors)))

    let integrity_check ?ppf ?heads ~auto_repair t =
      let contents = X.Repo.contents_t t in
      let nodes = X.Repo.node_t t in
      let commits = X.Repo.commit_t t |> snd in
      let no_check _ = true in
      let check ~kind ~offset ~length k =
        match kind with
        | `Contents ->
            X.Contents.CA.integrity_check ~offset ~length ~check_value:no_check
              k contents
        | `Node ->
            X.Node.integrity_check ~offset ~length ~check_value:no_check k nodes
        | `Commit ->
            X.Commit.CA.integrity_check ~offset ~length ~check_value:no_check k
              commits
      in
      match heads with
      | None ->
          Lwt.return (Index.integrity_check ?ppf ~auto_repair ~check t.index)
      | Some heads -> integrity_check_by_commit ~heads t

    let sync = X.Repo.sync
    let clear = X.Repo.clear
    let migrate = Migrate.run
    let flush = X.Repo.flush
    let reconstruct_index = X.Repo.Reconstruct_index.reconstruct
  end
end
