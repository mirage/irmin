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

let src = Logs.Src.create "irmin-pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = `V2

let pp_version = IO.pp_version

let ( -- ) = Int64.sub

exception RO_Not_Allowed = IO.Unix.RO_Not_Allowed

exception Unsupported_version = Store.Unsupported_version

module I = IO
module IO = IO.Unix
open Lwt.Infix
module Atomic_write = Store.Atomic_write
module Pack_config = Config
module Lock = IO_layers.Lock
module IO_layers = IO_layers.IO

module Default = struct
  let lower_root = Irmin_layers.Layer_id.to_string `Lower

  let upper0_root = Irmin_layers.Layer_id.to_string `Upper0

  let upper1_root = Irmin_layers.Layer_id.to_string `Upper1

  let copy_in_upper = false

  let with_lower = true

  let blocking_copy_size = 64
end

module Conf = Irmin.Private.Conf

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Conf.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root1_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Conf.string Default.upper1_root

let upper_root1 conf = Conf.get conf upper_root1_key

let upper_root0_key =
  Conf.key ~doc:"The root directory for the secondary upper layer."
    "root_second" Conf.string Default.upper0_root

let upper_root0 conf = Conf.get conf upper_root0_key

let copy_in_upper_key =
  Conf.key ~doc:"Copy the max commits in upper after a freeze." "copy_in_upper"
    Conf.bool false

let get_copy_in_upper conf = Conf.get conf copy_in_upper_key

let with_lower_key =
  Conf.key ~doc:"Use a lower layer." "with-lower" Conf.bool Default.with_lower

let with_lower conf = Conf.get conf with_lower_key

let blocking_copy_size_key =
  Conf.key
    ~doc:
      "Specify the maximum size (in bytes) that can be copied in the blocking \
       portion of the freeze."
    "blocking-copy" Conf.int Default.blocking_copy_size

let blocking_copy_size conf = Conf.get conf blocking_copy_size_key

let config_layers ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root1 = Default.upper1_root) ?(upper_root0 = Default.upper0_root)
    ?(copy_in_upper = Default.copy_in_upper) ?(with_lower = Default.with_lower)
    ?(blocking_copy_size = Default.blocking_copy_size) () =
  let config = Conf.add conf lower_root_key lower_root in
  let config = Conf.add config upper_root1_key upper_root1 in
  let config = Conf.add config upper_root0_key upper_root0 in
  let config = Conf.add config copy_in_upper_key copy_in_upper in
  let config = Conf.add config with_lower_key with_lower in
  let config = Conf.add config blocking_copy_size_key blocking_copy_size in
  config

let may f = function None -> Lwt.return_unit | Some bf -> f bf

let lock_path config =
  let root = Pack_config.root config in
  Filename.concat root "lock"

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
        include Layered_store.Content_addressable (H) (Index) (CA) (CA)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module Pa = Layered_store.Pack_Maker (H) (Index) (Pack)
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
        include Layered_store.Content_addressable (H) (Index) (CA) (CA)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Atomic_write (Key) (Val)
      module Closeable_AW = Closeable.Atomic_write (AW)
      include Layered_store.Atomic_write (Key) (Closeable_AW) (Closeable_AW)
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

      type freeze_throttle = [ `Overcommit_memory | `Block_writes ]

      type freeze_info = {
        throttle : freeze_throttle;
        lock : Lwt_mutex.t;
        mutable state : [ `None | `Running ];
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
        add_lock : Lwt_mutex.t;
        freeze : freeze_info;
      }

      let contents_t t = t.contents

      let node_t t = (contents_t t, t.node)

      let commit_t t = (node_t t, t.commit)

      let branch_t t = t.branch

      module Iterate = struct
        module Contents = struct
          include Contents.CA

          type t = [ `Read ] Contents.CA.t
        end

        module Nodes = struct
          include Node.CA

          type t = [ `Read ] Node.CA.t
        end

        module Commits = struct
          include Commit.CA

          type t = [ `Read ] Commit.CA.t
        end

        type 'a store_fn = {
          f : 't. (module S.LAYERED with type t = 't) -> 't -> 'a;
        }
        [@@ocaml.unboxed]

        let iter_lwt (f : unit Lwt.t store_fn) t : unit Lwt.t =
          f.f (module Contents) t.contents >>= fun () ->
          f.f (module Nodes) t.node >>= fun () ->
          f.f (module Commits) t.commit >>= fun () ->
          f.f (module Branch) t.branch

        let iter (f : unit store_fn) t : unit =
          f.f (module Contents) t.contents;
          f.f (module Nodes) t.node;
          f.f (module Commits) t.commit;
          f.f (module Branch) t.branch
      end

      let batch t f =
        Contents.CA.batch t.contents (fun contents ->
            Node.CA.batch t.node (fun node ->
                Commit.CA.batch t.commit (fun commit ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

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

      let freeze_info throttle =
        { throttle; state = `None; lock = Lwt_mutex.create () }

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
        let file = Layout.flip ~root in
        IO_layers.v file >>= fun flip_file ->
        IO_layers.read_flip flip_file >>= fun flip ->
        (* A fresh store has to unlink the lock file as well. *)
        let fresh = Pack_config.fresh config in
        let freeze = freeze_info (Pack_config.freeze_throttle config) in
        let lock_file = lock_path config in
        let freeze_in_progress () = freeze.state = `Running in
        let add_lock = Lwt_mutex.create () in
        (if fresh && Lock.test lock_file then Lock.unlink lock_file
        else Lwt.return_unit)
        >|= fun () ->
        let lower_contents = Option.map (fun x -> x.lcontents) lower in
        let contents =
          Contents.CA.v upper1.contents upper0.contents lower_contents ~flip
            ~freeze_in_progress ~add_lock
        in
        let lower_node = Option.map (fun x -> x.lnode) lower in
        let node =
          Node.CA.v upper1.node upper0.node lower_node ~flip ~freeze_in_progress
            ~add_lock
        in
        let lower_commit = Option.map (fun x -> x.lcommit) lower in
        let commit =
          Commit.CA.v upper1.commit upper0.commit lower_commit ~flip
            ~freeze_in_progress ~add_lock
        in
        let lower_branch = Option.map (fun x -> x.lbranch) lower in
        let branch =
          Branch.v upper1.branch upper0.branch lower_branch ~flip
            ~freeze_in_progress ~add_lock
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
          freeze;
          add_lock;
        }

      let unsafe_close t =
        t.closed <- true;
        (match t.lower_index with Some x -> Index.close x | None -> ());
        Index.close (fst t.uppers_index);
        Index.close (snd t.uppers_index);
        IO_layers.close t.flip_file >>= fun () ->
        let f : unit Lwt.t Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.close x);
          }
        in
        Iterate.iter_lwt f t

      let close t = Lwt_mutex.with_lock t.freeze.lock (fun () -> unsafe_close t)

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
        let f : unit Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.update_flip ~flip x);
          }
        in
        Iterate.iter f t

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
                     ~readonly:true (Layout.pack ~root)
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
      let clear_previous_upper ?keep_generation t =
        Log.debug (fun l -> l "clear previous upper");
        Contents.CA.clear_previous_upper ?keep_generation t.contents
        >>= fun () ->
        Node.CA.clear_caches_next_upper t.node;
        Commit.CA.clear_caches_next_upper t.commit;
        Branch.clear_previous_upper t.branch

      let flip_upper t =
        t.flip <- not t.flip;
        let f : unit Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.flip_upper x);
          }
        in
        Iterate.iter f t

      let write_flip t = IO_layers.write_flip t.flip t.flip_file

      let upper_in_use t = if t.flip then `Upper1 else `Upper0

      let offset t = Contents.CA.offset t.contents
    end
  end

  let integrity_check ?ppf ~auto_repair t =
    let module Checks = Store.Checks (Index) in
    let contents = X.Repo.contents_t t in
    let nodes = X.Repo.node_t t |> snd in
    let commits = X.Repo.commit_t t |> snd in
    let integrity_check_layer ~layer index =
      let check ~kind ~offset ~length k =
        match kind with
        | `Contents ->
            X.Contents.CA.integrity_check ~offset ~length ~layer k contents
        | `Node -> X.Node.CA.integrity_check ~offset ~length ~layer k nodes
        | `Commit ->
            X.Commit.CA.integrity_check ~offset ~length ~layer k commits
      in
      Checks.integrity_check ?ppf ~auto_repair ~check index
    in
    [
      (`Upper1, Some (fst t.X.Repo.uppers_index));
      (`Upper0, Some (snd t.X.Repo.uppers_index));
      (`Lower, t.lower_index);
    ]
    |> List.map (fun (layer, index) ->
           match index with
           | Some index -> (integrity_check_layer ~layer index, layer)
           | None -> (Ok `No_error, layer))

  include Irmin.Of_private (X)

  let sync = X.Repo.sync

  let clear = X.Repo.clear

  let migrate = X.Repo.migrate

  let flush = X.Repo.flush

  let pause = Lwt.pause

  let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash

  module Copy = struct
    let mem_commit_lower t = X.Commit.CA.mem_lower t.X.Repo.commit

    let mem_commit_next t = X.Commit.CA.mem_next t.X.Repo.commit

    let mem_node_lower t = X.Node.CA.mem_lower t.X.Repo.node

    let mem_node_next t = X.Node.CA.mem_next t.X.Repo.node

    let mem_contents_lower t = X.Contents.CA.mem_lower t.X.Repo.contents

    let mem_contents_next t = X.Contents.CA.mem_next t.X.Repo.contents

    let copy_branches t =
      X.Branch.copy ~mem_commit_lower:(mem_commit_lower t)
        ~mem_commit_upper:(mem_commit_next t) t.X.Repo.branch

    let skip_with_stats ~skip h =
      pause () >>= fun () ->
      skip h >|= function
      | true ->
          Irmin_layers.Stats.skip ();
          true
      | false -> false

    let no_skip _ = Lwt.return false

    let pred_node t k =
      let n = snd (X.Repo.node_t t) in
      X.Node.CA.find n k >|= function
      | None -> []
      | Some v ->
          List.rev_map
            (function `Inode x -> `Node x | (`Node _ | `Contents _) as x -> x)
            (X.Node.CA.Val.pred v)

    let iter_copy (contents, nodes, commits) ?(skip_commits = no_skip)
        ?(skip_nodes = no_skip) ?(skip_contents = no_skip) t ?(min = []) max =
      (* if node or contents are already in dst then they are skipped by
         Graph.iter; there is no need to check this again when the object is
         copied *)
      let commit k =
        X.Commit.CA.copy commits t.X.Repo.commit "Commit" k;
        Lwt.pause ()
      in
      let node k =
        X.Node.CA.copy nodes t.X.Repo.node k;
        Lwt.return_unit
      in
      let contents k =
        X.Contents.CA.copy contents t.X.Repo.contents "Contents" k;
        Lwt.return_unit
      in
      let skip_node h = skip_with_stats ~skip:skip_nodes h in
      let skip_contents h = skip_with_stats ~skip:skip_contents h in
      let skip_commit h = skip_with_stats ~skip:skip_commits h in
      Repo.iter t ~min ~max ~commit ~node ~contents ~skip_node ~skip_contents
        ~pred_node ~skip_commit ()
      >|= fun () -> X.Repo.flush t

    module CopyToLower = struct
      let on_lower t f =
        let contents =
          (X.Contents.CA.Lower, X.Contents.CA.lower t.X.Repo.contents)
        in
        let nodes = (X.Node.CA.Lower, X.Node.CA.lower t.X.Repo.node) in
        let commits = (X.Commit.CA.Lower, X.Commit.CA.lower t.X.Repo.commit) in
        f (contents, nodes, commits)

      let copy ?(min = []) t commits =
        Log.debug (fun f ->
            f "@[<2>copy to lower:@ min=%a,@ max=%a@]" pp_commits min pp_commits
              commits);
        let max = List.map (fun x -> `Commit (Commit.hash x)) commits in
        let min = List.map (fun x -> `Commit (Commit.hash x)) min in
        on_lower t (fun l ->
            iter_copy l ~skip_commits:(mem_commit_lower t)
              ~skip_nodes:(mem_node_lower t)
              ~skip_contents:(mem_contents_lower t) t ~min max)
    end

    module CopyToUpper = struct
      let on_next_upper t f =
        let contents =
          (X.Contents.CA.Upper, X.Contents.CA.next_upper t.X.Repo.contents)
        in
        let nodes = (X.Node.CA.Upper, X.Node.CA.next_upper t.X.Repo.node) in
        let commits =
          (X.Commit.CA.Upper, X.Commit.CA.next_upper t.X.Repo.commit)
        in
        f (contents, nodes, commits)

      let copy ?(min = []) t commits =
        Log.debug (fun f ->
            f "@[<2>copy to next upper:@ min=%a,@ max=%a@]" pp_commits min
              pp_commits commits);
        let max = List.map (fun x -> `Commit (Commit.hash x)) commits in
        (* When copying to next upper, if the min is empty then we copy only the
           max. *)
        let min =
          List.map (fun x -> `Commit (Commit.hash x)) min |> function
          | [] -> max
          | min -> min
        in
        on_next_upper t (fun u ->
            iter_copy u ~skip_commits:(mem_commit_next t)
              ~skip_nodes:(mem_node_next t) ~skip_contents:(mem_contents_next t)
              ~min t max)

      (** Newies are the objects added in current upper during the freeze. They
          are copied to the next upper before the freeze ends. When copying the
          newies we have to traverse them as well, to ensure that all objects
          used by a newies are also copied in the next upper. Newies can be
          nodes (or contents) not yet attached to a commit (or a node resp.), so
          we have to iter over the graph of commits, iter over the graph of
          nodes, and lastly iter over contents. *)
      let copy_newies_aux ~with_lock t =
        let newies_commits =
          if with_lock then
            X.Commit.CA.unsafe_consume_newies t.X.Repo.commit
            |> List.rev
            |> Lwt.return
          else X.Commit.CA.consume_newies t.X.Repo.commit >|= List.rev
        in
        let newies_nodes =
          if with_lock then
            X.Node.CA.unsafe_consume_newies t.X.Repo.node
            |> List.rev
            |> Lwt.return
          else X.Node.CA.consume_newies t.X.Repo.node >|= List.rev
        in
        let newies_contents =
          if with_lock then
            X.Contents.CA.unsafe_consume_newies t.X.Repo.contents
            |> List.rev
            |> Lwt.return
          else X.Contents.CA.consume_newies t.X.Repo.contents >|= List.rev
        in
        newies_commits >>= fun newies_commits ->
        newies_nodes >>= fun newies_nodes ->
        newies_contents >>= fun newies_contents ->
        let newies_commits = List.rev_map (fun x -> `Commit x) newies_commits in
        let newies_nodes = List.rev_map (fun x -> `Node x) newies_nodes in
        let newies_contents =
          List.rev_map (fun x -> `Contents x) newies_contents
        in
        let newies =
          (* mewies_node can grow very large so do not traverse it (it
             should always be the 2nd argument of rev_append) *)
          List.rev_append newies_commits
            (List.rev_append newies_contents newies_nodes)
        in
        Log.debug (fun l -> l "copy newies");
        (* we want to copy all the new commits; stop whenever one
           commmit already in the other upper or in lower. *)
        let skip_commits k =
          mem_commit_next t k >>= function
          | true -> Lwt.return true
          | false -> mem_commit_lower t k
        in
        (* FIXME(samoht): do we need to traverse the newies
           recursively if we don't need self-contained uppers. *)
        on_next_upper t (fun u ->
            iter_copy u ~skip_commits ~skip_nodes:(mem_node_next t)
              ~skip_contents:(mem_contents_next t) t newies)
        >>= fun () ->
        if with_lock then X.Branch.copy_last_newies_to_next_upper t.branch
        else X.Branch.copy_newies_to_next_upper t.branch

      (** If there are too many newies (more than newies_limit bytes added) then
          copy them concurrently. *)
      let rec copy_newies_to_next_upper t former_offset =
        let newies_limit = blocking_copy_size t.X.Repo.config |> Int64.of_int in
        let offset = X.Repo.offset t in
        if offset -- former_offset >= newies_limit then
          copy_newies_aux ~with_lock:false t >>= fun () ->
          (copy_newies_to_next_upper t offset [@tail])
        else Lwt.return_unit

      (** If there are only a few newies left (less than newies_limit bytes
          added) then copy them inside a lock. *)
      let copy_last_newies_to_next_upper t = copy_newies_aux ~with_lock:true t
    end

    module CopyFromLower = struct
      (* FIXME(samoht): copy/paste from iter_copy with s/copy/copy_from_lower *)
      let iter_copy (contents, nodes, commits) ?(skip_commits = no_skip)
          ?(skip_nodes = no_skip) ?(skip_contents = no_skip) t ?(min = []) cs =
        (* if node or contents are already in dst then they are skipped by
           Graph.iter; there is no need to check this again when the object is
           copied *)
        let commit k =
          X.Commit.CA.copy_from_lower ~dst:commits t.X.Repo.commit "Commit" k
        in
        let node k = X.Node.CA.copy_from_lower ~dst:nodes t.X.Repo.node k in
        let contents k =
          X.Contents.CA.copy_from_lower ~dst:contents t.X.Repo.contents
            "Contents" k
        in
        let skip_node h = skip_with_stats ~skip:skip_nodes h in
        let skip_contents h = skip_with_stats ~skip:skip_contents h in
        let skip_commit h = skip_with_stats ~skip:skip_commits h in
        let max = List.map (fun c -> `Commit c) cs in
        let min = List.map (fun c -> `Commit c) min in
        Repo.iter t ~min ~max ~commit ~node ~contents ~skip_node ~skip_contents
          ~pred_node ~skip_commit ()
        >|= fun () -> X.Repo.flush t

      let on_current_upper t f =
        let contents = X.Contents.CA.current_upper t.X.Repo.contents in
        let nodes = X.Node.CA.current_upper t.X.Repo.node in
        let commits = X.Commit.CA.current_upper t.X.Repo.commit in
        f (contents, nodes, commits)

      (** The commits can be in either lower or upper. We don't skip an object
          already in upper as its predecessors could be in lower. *)
      let self_contained ?min ~max t =
        let max = List.map (fun x -> Commit.hash x) max in
        let min =
          match min with
          | None -> max (* if min is empty then copy only the max commits *)
          | Some min -> List.map (fun x -> Commit.hash x) min
        in
        (* FIXME(samoht): do this in 2 steps: 1/ find the shallow
           hashes in upper 2/ iterates with max=shallow *)
        Log.debug (fun l ->
            l
              "self_contained: copy commits min:%a; max:%a from lower into \
               upper to make the upper self contained"
              (Fmt.list (Irmin.Type.pp H.t))
              min
              (Fmt.list (Irmin.Type.pp H.t))
              max);
        on_current_upper t (fun u -> iter_copy u ~min t max)
    end
  end

  let dump_stats msg =
    let stats = Irmin_layers.Stats.get () in
    Log.debug (fun l ->
        l "%s: contents=%d, nodes=%d, commits=%d, skips=%d" msg
          (List.hd stats.copied_contents)
          (List.hd stats.copied_nodes)
          (List.hd stats.copied_commits)
          stats.skips)

  let with_stats msg f = f >|= fun () -> dump_stats msg

  let copy ~min ~max ~squash ~upper:(copy_in_upper, min_upper) t =
    (* Copy commits to lower: if squash then copy only the max commits *)
    let with_lower = with_lower t.X.Repo.config in
    (if with_lower then
     let min = if squash then max else min in
     with_stats "copied in lower" (Copy.CopyToLower.copy t ~min max)
    else Lwt.return_unit)
    >>= fun () ->
    (* Copy [min_upper, max] to next_upper *)
    (if copy_in_upper then
     with_stats "copied in upper" (Copy.CopyToUpper.copy t ~min:min_upper max)
    else Lwt.return_unit)
    >>= fun () ->
    (* Copy branches to both lower and next_upper *)
    Copy.copy_branches t

  module Field = struct
    type t = F : 'a Fmt.t * string * 'a -> t | E

    let pp ppf = function E -> () | F (pp, k, v) -> Fmt.pf ppf "%s=%a" k pp v

    let pps ppf t =
      Fmt.list ~sep:(Fmt.unit "; ") pp ppf (List.filter (fun x -> x <> E) t)

    let commits k = function [] -> E | v -> F (pp_commits, k, v)

    let bool k v = if not v then E else F (Fmt.bool, k, v)

    let int k v = F (Fmt.int, k, v)

    let upper k = function false, _ | true, [] -> E | _, v -> commits k v
  end

  let unsafe_freeze ~min ~max ~squash ~upper ?hook ~id ~start_time t =
    Log.info (fun l ->
        l "freeze starts { %a }" Field.pps
          [
            Field.int "id" id;
            Field.commits "min" min;
            Field.commits "max" max;
            Field.bool "squash" squash;
            Field.upper "min_upper" upper;
          ]);
    Irmin_layers.Stats.freeze ();
    let offset = X.Repo.offset t in
    let lock_file = lock_path t.X.Repo.config in
    (* we take a lock here to signal that a freeze was in progess in
       case of crash, to trigger the recovery path. *)
    Lock.v lock_file >>= fun lock_file ->
    let copy () =
      may (fun f -> f `Before_Copy) hook >>= fun () ->
      copy ~min ~max ~squash ~upper t >>= fun () ->
      X.Repo.flush_next_lower t;
      may (fun f -> f `Before_Copy_Newies) hook >>= fun () ->
      Copy.CopyToUpper.copy_newies_to_next_upper t offset >>= fun () ->
      may (fun f -> f `Before_Copy_Last_Newies) hook >>= fun () ->
      let before_add_lock = Mtime_clock.count start_time in
      Lwt_mutex.with_lock t.add_lock (fun () ->
          let after_add_lock = Mtime_clock.count start_time in
          Copy.CopyToUpper.copy_last_newies_to_next_upper t >>= fun () ->
          may (fun f -> f `Before_Flip) hook >>= fun () ->
          X.Repo.flip_upper t;
          may (fun f -> f `Before_Clear) hook >>= fun () ->
          X.Repo.clear_previous_upper t >|= fun () ->
          Mtime.Span.abs_diff after_add_lock before_add_lock)
      >>= fun waiting_add ->
      (* RO reads generation from pack file to detect a flip change, so it's
         ok to write the flip file outside the lock *)
      X.Repo.write_flip t >|= fun () -> waiting_add
    in
    let finalize waiting_freeze waiting_add =
      t.freeze.state <- `None;
      Lock.close lock_file >>= fun () ->
      Lwt_mutex.unlock t.freeze.lock;
      may (fun f -> f `After_Clear) hook >|= fun () ->
      let duration = Mtime_clock.count start_time in
      Log.info (fun l ->
          l
            "freeze ends   { id=%d; total-duration=%a; freeze-lock=%a; \
             add-lock=%a }"
            id Mtime.Span.pp duration Mtime.Span.pp waiting_freeze Mtime.Span.pp
            waiting_add)
    in
    let async () =
      let waiting_freeze = Mtime_clock.count start_time in
      copy () >>= fun waiting_add -> finalize waiting_freeze waiting_add
    in
    Lwt.async (fun () -> Irmin_layers.Stats.with_timer `Freeze async);
    Lwt.return_unit

  let freeze_counter =
    let n = ref 0 in
    fun () ->
      incr n;
      !n

  (** main thread takes the lock at the begining of freeze and async thread
      releases it at the end. This is to ensure that no two freezes can run
      simultaneously. *)
  let freeze' ?(min = []) ?(max = []) ?(squash = false) ?copy_in_upper
      ?(min_upper = []) ?(recovery = false) ?hook t =
    (if recovery then X.Repo.clear_previous_upper ~keep_generation:() t
    else Lwt.return_unit)
    >>= fun () ->
    let freeze () =
      let id = freeze_counter () in
      let start_time = Mtime_clock.counter () in
      let upper =
        ( (match copy_in_upper with
          | None -> get_copy_in_upper t.X.Repo.config
          | Some b -> b),
          min_upper )
      in
      Irmin_layers.Stats.with_timer `Waiting (fun () ->
          Lwt_mutex.lock t.freeze.lock >|= fun () -> t.freeze.state <- `Running)
      >>= fun () ->
      (match max with [] -> Repo.heads t | m -> Lwt.return m) >>= fun max ->
      unsafe_freeze ~min ~max ~squash ~upper ?hook ~start_time ~id t
    in
    if t.X.Repo.closed then Lwt.fail_with "store is closed"
    else if Pack_config.readonly t.X.Repo.config then raise RO_Not_Allowed
    else
      match (t.freeze.state, t.freeze.throttle) with
      | `Running, `Overcommit_memory -> Lwt.return ()
      | _ -> freeze ()

  let layer_id = X.Repo.layer_id

  let freeze = freeze' ?hook:None

  let async_freeze (t : Repo.t) = t.freeze.state = `Running

  let upper_in_use = X.Repo.upper_in_use

  let self_contained = Copy.CopyFromLower.self_contained

  let needs_recovery t = lock_path t.X.Repo.config |> Lock.test

  let check_self_contained ?heads t =
    Log.debug (fun l -> l "Check that the upper layer is self contained");
    let errors = ref 0 in
    let none () =
      incr errors;
      Lwt.return_unit
    in
    let node k = X.Node.CA.check t.X.Repo.node ~none k in
    let contents k = X.Contents.CA.check t.X.Repo.contents ~none k in
    let commit k = X.Commit.CA.check t.X.Repo.commit ~none k in
    (match heads with None -> Repo.heads t | Some m -> Lwt.return m)
    >>= fun heads ->
    let hashes = List.map (fun x -> `Commit (Commit.hash x)) heads in
    Repo.iter t ~min:[] ~max:hashes ~commit ~node ~contents () >|= fun () ->
    let pp_commits = Fmt.list ~sep:Fmt.comma Commit.pp_hash in
    if !errors = 0 then
      Fmt.kstrf
        (fun x -> Ok (`Msg x))
        "Upper layer is self contained for heads %a" pp_commits heads
    else
      Fmt.kstrf
        (fun x -> Error (`Msg x))
        "Upper layer is not self contained for heads %a: %n phantom objects \
         detected"
        pp_commits heads !errors

  module PrivateLayer = struct
    module Hook = struct
      type 'a t = 'a -> unit Lwt.t

      let v f = f
    end

    let wait_for_freeze (t : Repo.t) =
      Lwt_mutex.with_lock t.freeze.lock (fun () -> Lwt.return_unit)

    let freeze' = freeze'

    let upper_in_use = upper_in_use
  end
end

module type S = sig
  include Irmin_layers.S

  include Store.S with type repo := repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    (( [> `Fixed of int | `No_error ],
       [> `Cannot_fix of string | `Corrupted of int ] )
     result
    * Irmin_layers.Layer_id.t)
    list
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
