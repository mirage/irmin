(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "irmin.pack.layers" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = `V2

let pp_version = IO.pp_version

let ( // ) = Filename.concat

let newies_limit = 64L

let ( -- ) = Int64.sub

exception RO_Not_Allowed = IO.Unix.RO_Not_Allowed

exception Unsupported_version = Store.Unsupported_version

module I = IO
module IO = IO.Unix
open Lwt.Infix
module Atomic_write = Store.Atomic_write
module Pack_config = Config
module IO_layers = IO_layers.IO

module Default = struct
  let lower_root = "lower"

  let upper_root1 = "upper1"

  let upper_root0 = "upper0"

  let copy_in_upper = false

  let with_lower = true
end

module Conf = Irmin.Private.Conf

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Conf.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root1_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Conf.string Default.upper_root1

let upper_root1 conf = Conf.get conf upper_root1_key

let upper_root0_key =
  Conf.key ~doc:"The root directory for the secondary upper layer."
    "root_second" Conf.string Default.upper_root0

let upper_root0 conf = Conf.get conf upper_root0_key

let copy_in_upper_key =
  Conf.key ~doc:"Copy the max commits in upper after a freeze." "copy_in_upper"
    Conf.bool false

let get_copy_in_upper conf = Conf.get conf copy_in_upper_key

let with_lower_key =
  Conf.key ~doc:"Use a lower layer." "with-lower" Conf.bool Default.with_lower

let with_lower conf = Conf.get conf with_lower_key

let config_layers ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root1 = Default.upper_root1) ?(upper_root0 = Default.upper_root0)
    ?(copy_in_upper = Default.copy_in_upper) ?(with_lower = Default.with_lower)
    () =
  let config = Conf.add conf lower_root_key lower_root in
  let config = Conf.add config upper_root1_key upper_root1 in
  let config = Conf.add config upper_root0_key upper_root0 in
  let config = Conf.add config copy_in_upper_key copy_in_upper in
  let config = Conf.add config with_lower_key with_lower in
  config

let freeze_lock = Lwt_mutex.create ()

let add_lock = Lwt_mutex.create ()

let may f = function None -> Lwt.return_unit | Some bf -> f bf

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

  type store_handle =
    | Commit_t : H.t -> store_handle
    | Node_t : H.t -> store_handle
    | Content_t : H.t -> store_handle

  module X = struct
    module Hash = H

    type 'a value = { magic : char; hash : H.t; v : 'a }

    let value a =
      let open Irmin.Type in
      record "value" (fun hash magic v -> { magic; hash; v })
      |+ field "hash" H.t (fun v -> v.hash)
      |+ field "magic" char (fun v -> v.magic)
      |+ field "v" a (fun v -> v.v)
      |> sealr

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value Val.t

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = decode_value s off in
            t.v

          let magic _ = magic
        end)

        module CA = Closeable.Content_addressable (CA_Pack)
        include Layered.Content_addressable (H) (Index) (CA)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module Pa = Layered.Pack_Maker (H) (Index) (Pack)
      module CA = Inode_layers.Make (Config) (H) (Pa) (Node)
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

          let value = value Val.t

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

        module CA = Closeable.Content_addressable (CA_Pack)
        include Layered.Content_addressable (H) (Index) (CA)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Atomic_write (Key) (Val)
      module Closeable_AW = Closeable.Atomic_write (AW)
      include Layered.Atomic_write (Key) (Closeable_AW)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type upper_layer = {
        contents : [ `Read ] Contents.CA.U.t;
        node : [ `Read ] Node.CA.U.t;
        commit : [ `Read ] Commit.CA.U.t;
        branch : Branch.U.t;
        index : Index.t;
      }

      type lower_layer = {
        lcontents : [ `Read ] Contents.CA.L.t;
        lnode : [ `Read ] Node.CA.L.t;
        lcommit : [ `Read ] Commit.CA.L.t;
        lbranch : Branch.L.t;
        lindex : Index.t;
      }

      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        branch : Branch.t;
        commit : [ `Read ] Commit.CA.t;
        lower_index : Index.t option;
        uppers_index : Index.t * Index.t;
        mutable flip : bool;
        mutable closed : bool;
        flip_file : IO_layers.t;
      }

      let contents_t t = t.contents

      let node_t t = (contents_t t, t.node)

      let commit_t t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Contents.CA.batch t.contents (fun contents ->
            Node.CA.batch t.node (fun node ->
                Commit.CA.batch t.commit (fun commit ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

      let log_current_upper t = if t.flip then "upper1" else "upper0"

      let unsafe_v_upper root config =
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
        Contents.CA.U.v ~fresh ~readonly ~lru_size ~index root
        >>= fun contents ->
        Node.CA.U.v ~fresh ~readonly ~lru_size ~index root >>= fun node ->
        Commit.CA.U.v ~fresh ~readonly ~lru_size ~index root >>= fun commit ->
        Branch.U.v ~fresh ~readonly root >|= fun branch ->
        (f := fun () -> Contents.CA.U.flush ~index:false contents);
        ({ index; contents; node; commit; branch } : upper_layer)

      let unsafe_v_lower root config =
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        let log_size = Pack_config.index_log_size config in
        let throttle = Pack_config.index_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
            ~fresh ~readonly ~throttle ~log_size root
        in
        Contents.CA.L.v ~fresh ~readonly ~lru_size ~index root
        >>= fun lcontents ->
        Node.CA.L.v ~fresh ~readonly ~lru_size ~index root >>= fun lnode ->
        Commit.CA.L.v ~fresh ~readonly ~lru_size ~index root >>= fun lcommit ->
        Branch.L.v ~fresh ~readonly root >|= fun lbranch ->
        (f := fun () -> Contents.CA.L.flush ~index:false lcontents);
        ({ lindex = index; lcontents; lnode; lcommit; lbranch } : lower_layer)

      let v_layer ~v root config =
        Lwt.catch
          (fun () -> v root config)
          (function
            | I.Invalid_version { expected; found }
              when expected = current_version ->
                Log.err (fun m ->
                    m "[%s] Attempted to open store of unsupported version %a"
                      root pp_version found);
                Lwt.fail (Unsupported_version found)
            | e -> Lwt.fail e)

      let v config =
        let root = Pack_config.root config in
        let upper1 = Filename.concat root (upper_root1 config) in
        v_layer ~v:unsafe_v_upper upper1 config >>= fun upper1 ->
        let upper0 = Filename.concat root (upper_root0 config) in
        v_layer ~v:unsafe_v_upper upper0 config >>= fun upper0 ->
        let with_lower = with_lower config in
        let lower_root = Filename.concat root (lower_root config) in
        (if with_lower then
         v_layer ~v:unsafe_v_lower lower_root config >|= fun lower -> Some lower
        else Lwt.return_none)
        >>= fun lower ->
        let file = Filename.concat root "flip" in
        IO_layers.v file >>= fun flip_file ->
        IO_layers.read_flip flip_file >|= fun flip ->
        let lower_contents = Option.map (fun x -> x.lcontents) lower in
        let contents =
          Contents.CA.v upper1.contents upper0.contents lower_contents ~flip
            ~freeze_lock ~add_lock
        in
        let lower_node = Option.map (fun x -> x.lnode) lower in
        let node =
          Node.CA.v upper1.node upper0.node lower_node ~flip ~freeze_lock
            ~add_lock
        in
        let lower_commit = Option.map (fun x -> x.lcommit) lower in
        let commit =
          Commit.CA.v upper1.commit upper0.commit lower_commit ~flip
            ~freeze_lock ~add_lock
        in
        let lower_branch = Option.map (fun x -> x.lbranch) lower in
        let branch =
          Branch.v upper1.branch upper0.branch lower_branch ~flip ~freeze_lock
            ~add_lock
        in
        let lower_index = Option.map (fun x -> x.lindex) lower in
        {
          contents;
          node;
          commit;
          branch;
          config;
          lower_index;
          uppers_index = (upper1.index, upper0.index);
          flip;
          closed = false;
          flip_file;
        }

      let unsafe_close t =
        t.closed <- true;
        (match t.lower_index with Some x -> Index.close x | None -> ());
        Index.close (fst t.uppers_index);
        Index.close (snd t.uppers_index);
        IO_layers.close t.flip_file >>= fun () ->
        Contents.CA.close t.contents >>= fun () ->
        Node.CA.close t.node >>= fun () ->
        Commit.CA.close t.commit >>= fun () -> Branch.close t.branch

      let close t = Lwt_mutex.with_lock freeze_lock (fun () -> unsafe_close t)

      (** RO uses the generation to sync the stores, so to prevent races (async
          reads of flip and generation) the generation is used to update the
          flip. The first store reads the flip and syncs with the files on disk,
          the other stores only need to update the flip. *)
      let sync t =
        let on_generation_change () =
          Node.CA.clear_caches t.node;
          Commit.CA.clear_caches t.commit
        in
        let on_generation_change_next_upper () =
          Node.CA.clear_caches_next_upper t.node;
          Commit.CA.clear_caches_next_upper t.commit
        in
        let flip =
          Contents.CA.sync ~on_generation_change
            ~on_generation_change_next_upper t.contents
        in
        t.flip <- flip;
        Node.CA.update_flip ~flip t.node;
        Commit.CA.update_flip ~flip t.commit;
        Branch.update_flip ~flip t.branch

      let clear t = Contents.CA.clear t.contents

      (** migrate can be called on a layered store where only one layer exists
          on disk. As migration fails on an empty store, we check which layer is
          in the wrong version. *)
      let migrate config =
        if Pack_config.readonly config then raise RO_Not_Allowed;
        let root = Pack_config.root config in
        [ upper_root1; upper_root0; lower_root ]
        |> List.map (fun name ->
               let root = Filename.concat root (name config) in
               let config = Conf.add config Pack_config.root_key (Some root) in
               try
                 let io =
                   IO.v ~version:(Some current_version) ~fresh:false
                     ~readonly:true (root // "store.pack")
                 in
                 (config, Some io)
               with
               | I.Invalid_version _ -> (config, None)
               | e -> raise e)
        |> List.fold_left
             (fun to_migrate (config, io) ->
               match io with
               | None -> config :: to_migrate
               | Some io ->
                   IO.close io;
                   to_migrate)
             []
        |> List.iter (fun config -> Store.migrate config)

      let layer_id t store_handler =
        match store_handler with
        | Commit_t k -> Commit.CA.layer_id t.commit k
        | Node_t k -> Node.CA.layer_id t.node k
        | Content_t k -> Contents.CA.layer_id t.contents k

      let flush t =
        Contents.CA.flush t.contents;
        Branch.flush t.branch

      let flush_next_lower t =
        Contents.CA.flush_next_lower t.contents;
        Branch.flush_next_lower t.branch

      (** Store share instances of the underlying IO files, so it is enough to
          call clear on one store. However, each store has its own caches, which
          need to be cleared too. *)
      let clear_previous_upper t =
        Log.debug (fun l -> l "clear previous upper");
        Contents.CA.clear_previous_upper t.contents >>= fun () ->
        Node.CA.clear_caches_next_upper t.node;
        Commit.CA.clear_caches_next_upper t.commit;
        Branch.clear_previous_upper t.branch

      let flip_upper t =
        t.flip <- not t.flip;
        Contents.CA.flip_upper t.contents;
        Node.CA.flip_upper t.node;
        Commit.CA.flip_upper t.commit;
        Branch.flip_upper t.branch

      let write_flip t = IO_layers.write_flip t.flip t.flip_file

      let upper_in_use t = if t.flip then `Upper1 else `Upper0

      let offset t = Contents.CA.offset t.contents

      (** Newies are the objects added in current upper during the freeze. They
          are copied to the next upper before the freeze ends. We do not lock
          this operation if we have to copy more than [newies_limit] bytes. If
          there are fewer newies than that then [copy_last_newies_to_next_upper]
          is called right after. *)
      let rec copy_newies_to_next_upper t former_offset =
        let offset = offset t in
        if offset -- former_offset >= newies_limit then
          Contents.CA.copy_newies_to_next_upper t.contents >>= fun () ->
          Node.CA.copy_newies_to_next_upper t.node >>= fun () ->
          Commit.CA.copy_newies_to_next_upper t.commit >>= fun () ->
          Branch.copy_newies_to_next_upper t.branch >>= fun () ->
          copy_newies_to_next_upper t offset
        else Lwt.return_unit

      let copy_last_newies_to_next_upper t =
        Contents.CA.copy_last_newies_to_next_upper t.contents;
        Node.CA.copy_last_newies_to_next_upper t.node;
        Commit.CA.copy_last_newies_to_next_upper t.commit;
        Branch.copy_last_newies_to_next_upper t.branch
    end
  end

  let integrity_check ?ppf:_ ~auto_repair:_ _repo =
    Error (`Cannot_fix "Not implemented")

  include Irmin.Of_private (X)

  let sync = X.Repo.sync

  let clear = X.Repo.clear

  let migrate = X.Repo.migrate

  let flush = X.Repo.flush

  module Copy = struct
    let mem_commit_lower t = X.Commit.CA.mem_lower t.X.Repo.commit

    let mem_commit_upper t = X.Commit.CA.mem_next t.X.Repo.commit

    let mem_node_lower t = X.Node.CA.mem_lower t.X.Repo.node

    let mem_node_upper t = X.Node.CA.mem_next t.X.Repo.node

    let copy_branches t =
      X.Branch.copy ~mem_commit_lower:(mem_commit_lower t)
        ~mem_commit_upper:(mem_commit_upper t) t.X.Repo.branch

    let copy_tree ~skip nodes contents t root =
      (* if node are already in dst then they are skipped by Graph.iter; there
         is no need to check this again when the node is copied *)
      let node k = X.Node.CA.copy nodes t.X.Repo.node k in
      (* we need to check that the contents is not already in dst, to avoid
         copying it again *)
      let contents (k, _) =
        X.Contents.CA.check_and_copy contents t.X.Repo.contents "Contents" k
      in
      Repo.iter_nodes t ~min:[] ~max:[ root ] ~node ~contents ~skip ()

    let copy_commit ~skip contents nodes commits t k =
      let aux c = copy_tree ~skip nodes contents t (X.Commit.Val.node c) in
      X.Commit.CA.copy commits t.X.Repo.commit ~aux "Commit" k

    module CopyToLower = struct
      let on_lower t f =
        let contents = X.Contents.CA.lower t.X.Repo.contents in
        let nodes = X.Node.CA.lower t.X.Repo.node in
        let commits = X.Commit.CA.lower t.X.Repo.commit in
        f contents nodes commits

      let copy_commit contents nodes commits t =
        copy_commit ~skip:(mem_node_lower t)
          (X.Contents.CA.Lower, contents)
          (X.Node.CA.Lower, nodes)
          (X.Commit.CA.Lower, commits)
          t

      let copy_max_commits ~max t =
        Log.debug (fun f -> f "copy max commits to lower");
        Lwt_list.iter_s
          (fun k ->
            let h = Commit.hash k in
            on_lower t (fun contents nodes commits ->
                mem_commit_lower t h >>= function
                | true -> Lwt.return_unit
                | false -> copy_commit contents nodes commits t h))
          max

      let copy ~min ~max t =
        let max = List.map (fun x -> Commit.hash x) max in
        let min = List.map (fun x -> Commit.hash x) min in
        let skip = mem_commit_lower t in
        on_lower t (fun contents nodes commits ->
            let commit = copy_commit contents nodes commits t in
            Repo.iter_commits t ~min ~max ~commit ~skip ())
    end

    module CopyToUpper = struct
      let on_next_upper t f =
        let contents = X.Contents.CA.next_upper t.X.Repo.contents in
        let nodes = X.Node.CA.next_upper t.X.Repo.node in
        let commits = X.Commit.CA.next_upper t.X.Repo.commit in
        f contents nodes commits

      let copy_commit contents nodes commits t =
        copy_commit ~skip:(mem_node_upper t)
          (X.Contents.CA.Upper, contents)
          (X.Node.CA.Upper, nodes)
          (X.Commit.CA.Upper, commits)
          t

      let copy ~min ~max t =
        let skip = mem_commit_upper t in
        on_next_upper t (fun contents nodes commits ->
            let commit = copy_commit contents nodes commits t in
            Repo.iter_commits t ~min ~max ~commit ~skip ())

      let copy_commits ~min ~max t =
        Log.debug (fun f ->
            f "copy commits to next upper %s" (X.Repo.log_current_upper t));
        let max = List.map (fun x -> Commit.hash x) max in
        (* if min is empty then copy only the max commits *)
        let min =
          match min with
          | [] -> max
          | min -> List.map (fun x -> Commit.hash x) min
        in
        copy ~min ~max t

      let includes commits k =
        List.exists (fun k' -> Irmin.Type.equal Hash.t k k') commits

      let non_empty_intersection t min head =
        let skip _ = Lwt.return_false in
        let ok = ref false in
        let commit x =
          if includes min x then ok := true;
          Lwt.return_unit
        in
        Repo.iter_commits t ~min ~max:[ head ] ~commit ~skip () >|= fun () ->
        !ok

      (** Copy the heads that include a max commit *)
      let copy_heads ~max ~heads t =
        Log.debug (fun f ->
            f "copy heads to current %s" (X.Repo.log_current_upper t));
        let max = List.map (fun x -> Commit.hash x) max in
        let heads = List.map (fun x -> Commit.hash x) heads in
        Lwt_list.fold_left_s
          (fun acc head ->
            (*if a head is a max commit then already copied*)
            if includes max head then Lwt.return acc
            else
              (*if a head is after the max commit then copy it*)
              non_empty_intersection t max head >|= function
              | true -> head :: acc
              | false -> acc)
          [] heads
        >>= fun heads -> copy ~min:max ~max:heads t
    end
  end

  let copy ~min ~max ~squash ~copy_in_upper ~min_upper ~heads t =
    (* Copy commits to lower: if squash then copy only the max commits *)
    let with_lower = with_lower t.X.Repo.config in
    (if with_lower then
     if squash then Copy.CopyToLower.copy_max_commits ~max t
     else Copy.CopyToLower.copy ~min ~max t
    else Lwt.return_unit)
    >>= fun () ->
    (* Copy [min_upper, max] and [max, heads] to next_upper *)
    (if copy_in_upper then
     Copy.CopyToUpper.copy_commits ~min:min_upper ~max t >>= fun () ->
     Copy.CopyToUpper.copy_heads ~max ~heads t
    else Lwt.return_unit)
    >>= fun () ->
    (* Copy branches to both lower and next_upper *)
    Copy.copy_branches t

  let unsafe_freeze ~min ~max ~squash ~copy_in_upper ~min_upper ~heads ?hook t =
    let pp_commits ppf = List.iter (Commit.pp_hash ppf) in
    Log.debug (fun l ->
        l
          "unsafe_freeze min = %a max = %a squash = %b copy_in_upper = %b \
           min_upper = %a heads = %a"
          pp_commits min pp_commits max squash copy_in_upper pp_commits
          min_upper pp_commits heads);
    Irmin_layers.Stats.freeze ();
    let offset = X.Repo.offset t in
    Lwt.async (fun () ->
        Lwt.pause () >>= fun () ->
        may (fun f -> f `Before_Copy) hook >>= fun () ->
        copy ~min ~max ~squash ~copy_in_upper ~min_upper ~heads t >>= fun () ->
        X.Repo.flush_next_lower t;
        may (fun f -> f `Before_Copy_Newies) hook >>= fun () ->
        X.Repo.copy_newies_to_next_upper t offset >>= fun () ->
        may (fun f -> f `Before_Copy_Last_Newies) hook >>= fun () ->
        Lwt_mutex.with_lock add_lock (fun () ->
            X.Repo.copy_last_newies_to_next_upper t >>= fun () ->
            may (fun f -> f `Before_Flip) hook >>= fun () ->
            X.Repo.flip_upper t;
            may (fun f -> f `Before_Clear) hook >>= fun () ->
            X.Repo.clear_previous_upper t)
        >>= fun () ->
        (* RO reads generation from pack file to detect a flip change, so it's
           ok to write the flip file outside the lock *)
        X.Repo.write_flip t >>= fun () ->
        Lwt_mutex.unlock freeze_lock;
        Log.debug (fun l -> l "free lock");
        may (fun f -> f `After_Clear) hook);
    Log.debug (fun l -> l "after async called to copy");
    Lwt.return_unit

  (** main thread takes the lock at the begining of freeze and async thread
      releases it at the end. This is to ensure that no two freezes can run
      simultaneously. *)
  let freeze' ?(min = []) ?(max = []) ?(squash = false) ?copy_in_upper
      ?(min_upper = []) ?(heads = []) ?hook t =
    let copy_in_upper =
      match copy_in_upper with
      | None -> get_copy_in_upper t.X.Repo.config
      | Some b -> b
    in
    (match max with [] -> Repo.heads t | m -> Lwt.return m) >>= fun max ->
    (match heads with [] -> Repo.heads t | m -> Lwt.return m) >>= fun heads ->
    if t.X.Repo.closed then Lwt.fail_with "store is closed"
    else
      Lwt_mutex.lock freeze_lock >>= fun () ->
      unsafe_freeze ~min ~max ~squash ~copy_in_upper ~min_upper ~heads ?hook t

  let layer_id = X.Repo.layer_id

  let freeze = freeze' ?hook:None

  let async_freeze () = Lwt_mutex.is_locked freeze_lock

  let upper_in_use = X.Repo.upper_in_use

  module PrivateLayer = struct
    module Hook = struct
      type 'a t = 'a -> unit Lwt.t

      let v f = f
    end

    let wait_for_freeze () =
      Lwt_mutex.with_lock freeze_lock (fun () -> Lwt.return_unit)

    let freeze' = freeze'

    let upper_in_use = upper_in_use
  end
end

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
