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

(* TODO(craigfe): better namespacing of modules shared with [irmin-pack] *)
module Layout_layered = Layout
module V = Irmin_pack.Version.V2

let cache_size = 10_000

exception Cancelled

module IO = Irmin_pack.IO.Unix
module Lock = IO_layers.Lock
module IO_layers = IO_layers.IO

let may f = function None -> Lwt.return_unit | Some bf -> f bf
let lock_path root = Filename.concat root "lock"

module Maker' (Config : Conf.Pack.S) (Schema : Irmin.Schema.S) = struct
  open struct
    module C = Schema.Contents
    module M = Schema.Metadata
    module P = Schema.Path
    module B = Schema.Branch
  end

  module H = Schema.Hash
  module Index = Irmin_pack.Index.Make (H)
  module Pack = Irmin_pack.Pack_store.Maker (V) (Index) (H)

  type store_handle =
    | Commit_t : H.t -> store_handle
    | Node_t : H.t -> store_handle
    | Content_t : H.t -> store_handle

  module X = struct
    module Schema = Schema
    module Hash = H

    module Contents = struct
      module Pack_value = Irmin_pack.Pack_value.Of_contents (H) (C)

      (* FIXME: remove duplication with irmin-pack/ext.ml *)
      module CA = struct
        module CA = Pack.Make (Pack_value)
        include Layered_store.Content_addressable (H) (Index) (CA) (CA)
      end

      include Irmin.Contents.Store (CA) (H) (C)
    end

    module Node = struct
      module Pa = Layered_store.Pack_maker (H) (Index) (Pack)
      module CA = Inode_layers.Make (Config) (H) (Pa) (Schema.Node)
      include Irmin.Node.Store (Contents) (CA) (H) (CA.Val) (M) (P)
    end

    module Commit = struct
      module Pack_value =
        Irmin_pack.Pack_value.Of_commit
          (H)
          (struct
            module Info = Schema.Info
            include Schema.Commit
          end)

      module CA = struct
        module CA = Pack.Make (Pack_value)
        include Layered_store.Content_addressable (H) (Index) (CA) (CA)
      end

      include Irmin.Commit.Store (Schema.Info) (Node) (CA) (H) (Schema.Commit)
    end

    module Branch = struct
      module Key = B
      module Val = H

      module Atomic_write = struct
        module AW = Irmin_pack.Atomic_write.Make_persistent (V) (Key) (Val)
        include Irmin_pack.Atomic_write.Closeable (AW)

        let v ?fresh ?readonly path =
          AW.v ?fresh ?readonly path >|= make_closeable
      end

      include Layered_store.Atomic_write (Key) (Atomic_write) (Atomic_write)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Remote = Irmin.Private.Remote.None (H) (B)

    module Repo = struct
      type upper_layer = {
        contents : read Contents.CA.U.t;
        node : read Node.CA.U.t;
        commit : read Commit.CA.U.t;
        branch : Branch.U.t;
        index : Index.t;
      }

      type lower_layer = {
        lcontents : read Contents.CA.L.t;
        lnode : read Node.CA.L.t;
        lcommit : read Commit.CA.L.t;
        lbranch : Branch.L.t;
        lindex : Index.t;
      }

      type freeze_info = {
        throttle : Conf.Pack.freeze_throttle;
        lock : Lwt_mutex.t;
        mutable state : [ `None | `Running | `Cancel ];
      }

      type t = {
        root : string;
        readonly : bool;
        blocking_copy_size : int;
        with_lower : bool;
        contents : read Contents.CA.t;
        node : read Node.CA.t;
        branch : Branch.t;
        commit : read Commit.CA.t;
        lower_index : Index.t option;
        uppers_index : Index.t * Index.t;
        mutable flip : bool;
        mutable closed : bool;
        flip_file : IO_layers.t;
        batch_lock : Lwt_mutex.t;
        freeze : freeze_info;
      }

      let contents_t t = t.contents
      let node_t t = (contents_t t, t.node)
      let commit_t t = (node_t t, t.commit)
      let branch_t t = t.branch

      module Iterate = struct
        module Contents = struct
          include Contents.CA

          type t = read Contents.CA.t
        end

        module Nodes = struct
          include Node.CA

          type t = read Node.CA.t
        end

        module Commits = struct
          include Commit.CA

          type t = read Commit.CA.t
        end

        type 'a store_fn = {
          f : 't. (module S.Layered with type t = 't) -> 't -> 'a;
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
        Lwt_mutex.with_lock t.batch_lock @@ fun () ->
        Contents.CA.batch t.contents (fun contents ->
            Node.CA.batch t.node (fun node ->
                Commit.CA.batch t.commit (fun commit ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

      let unsafe_v_upper root config =
        let fresh = Conf.Pack.fresh config in
        let lru_size = Conf.Pack.lru_size config in
        let readonly = Conf.Pack.readonly config in
        let log_size = Conf.Pack.index_log_size config in
        let throttle = Conf.Pack.merge_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~throttle ~log_size root
        in
        let* contents =
          Contents.CA.U.v ~fresh ~readonly ~lru_size ~index root
        in
        let* node = Node.CA.U.v ~fresh ~readonly ~lru_size ~index root in
        let* commit = Commit.CA.U.v ~fresh ~readonly ~lru_size ~index root in
        let+ branch = Branch.U.v ~fresh ~readonly root in
        (f := fun () -> Contents.CA.U.flush ~index:false contents);
        ({ index; contents; node; commit; branch } : upper_layer)

      let unsafe_v_lower root config =
        let fresh = Conf.Pack.fresh config in
        let lru_size = Conf.Pack.lru_size config in
        let readonly = Conf.Pack.readonly config in
        let log_size = Conf.Pack.index_log_size config in
        let throttle = Conf.Pack.merge_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
            ~fresh ~readonly ~throttle ~log_size root
        in
        let* lcontents =
          Contents.CA.L.v ~fresh ~readonly ~lru_size ~index root
        in
        let* lnode = Node.CA.L.v ~fresh ~readonly ~lru_size ~index root in
        let* lcommit = Commit.CA.L.v ~fresh ~readonly ~lru_size ~index root in
        let+ lbranch = Branch.L.v ~fresh ~readonly root in
        (f := fun () -> Contents.CA.L.flush ~index:false lcontents);
        ({ lindex = index; lcontents; lnode; lcommit; lbranch } : lower_layer)

      let v_layer ~v root config =
        Lwt.catch
          (fun () -> v root config)
          (function
            | Irmin_pack.Version.Invalid { expected; found } as e
              when expected = V.version ->
                Log.err (fun m ->
                    m "[%s] Attempted to open store of unsupported version %a"
                      root Irmin_pack.Version.pp found);
                Lwt.fail e
            | e -> Lwt.fail e)

      let freeze_info throttle =
        { throttle; state = `None; lock = Lwt_mutex.create () }

      let v config =
        let root = Conf.Pack.(get config root_key) in
        let upper1 = Filename.concat root (Conf.upper_root1 config) in
        let* upper1 = v_layer ~v:unsafe_v_upper upper1 config in
        let upper0 = Filename.concat root (Conf.upper_root0 config) in
        let* upper0 = v_layer ~v:unsafe_v_upper upper0 config in
        let with_lower = Conf.with_lower config in
        let lower_root = Filename.concat root (Conf.lower_root config) in
        let* lower =
          if with_lower then
            v_layer ~v:unsafe_v_lower lower_root config >|= Option.some
          else Lwt.return_none
        in
        let file = Layout_layered.flip ~root in
        let* flip_file = IO_layers.v file in
        let* flip = IO_layers.read_flip flip_file in
        (* A fresh store has to unlink the lock file as well. *)
        let fresh = Conf.Pack.fresh config in
        let freeze = freeze_info (Conf.Pack.freeze_throttle config) in
        let lock_file = lock_path root in
        let freeze_in_progress () = freeze.state = `Running in
        let always_false () = false in
        let batch_lock = Lwt_mutex.create () in
        let+ () =
          if fresh && Lock.test lock_file then Lock.unlink lock_file
          else Lwt.return_unit
        in
        let lower_contents = Option.map (fun x -> x.lcontents) lower in
        let contents =
          Contents.CA.v upper1.contents upper0.contents lower_contents ~flip
            ~freeze_in_progress:always_false
        in
        let lower_node = Option.map (fun x -> x.lnode) lower in
        let node =
          Node.CA.v upper1.node upper0.node lower_node ~flip
            ~freeze_in_progress:always_false
        in
        let lower_commit = Option.map (fun x -> x.lcommit) lower in
        let commit =
          Commit.CA.v upper1.commit upper0.commit lower_commit ~flip
            ~freeze_in_progress
        in
        let lower_branch = Option.map (fun x -> x.lbranch) lower in
        let branch =
          Branch.v upper1.branch upper0.branch lower_branch ~flip
            ~freeze_in_progress
        in
        let lower_index = Option.map (fun x -> x.lindex) lower in
        let readonly = Conf.Pack.readonly config in
        let blocking_copy_size = Conf.blocking_copy_size config in
        {
          contents;
          node;
          commit;
          branch;
          root;
          readonly;
          with_lower;
          blocking_copy_size;
          lower_index;
          uppers_index = (upper1.index, upper0.index);
          flip;
          closed = false;
          flip_file;
          freeze;
          batch_lock;
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
              (fun (type a) (module C : S.Layered with type t = a) (x : a) ->
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
              (fun (type a) (module C : S.Layered with type t = a) (x : a) ->
                C.update_flip ~flip x);
          }
        in
        Iterate.iter f t

      let clear t = Contents.CA.clear t.contents

      (** migrate can be called on a layered store where only one layer exists
          on disk. As migration fails on an empty store, we check which layer is
          in the wrong version. *)
      let migrate config =
        if Conf.Pack.readonly config then raise Irmin_pack.RO_not_allowed;
        let root = Conf.Pack.(get config root_key) in
        Conf.[ upper_root1; upper_root0; lower_root ]
        |> List.map (fun name ->
               let root = Filename.concat root (name config) in
               let config = Conf.Pack.add config Conf.Pack.root_key root in
               try
                 let io =
                   IO.v ~version:(Some V.version) ~fresh:false ~readonly:true
                     (Layout.pack ~root)
                 in
                 (config, Some io)
               with
               | Irmin_pack.Version.Invalid _ -> (config, None)
               | e -> raise e)
        |> List.fold_left
             (fun to_migrate (config, io) ->
               match io with
               | None -> config :: to_migrate
               | Some io ->
                   IO.close io;
                   to_migrate)
             []
        |> List.iter (fun config -> Irmin_pack.migrate config)

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
              (fun (type a) (module C : S.Layered with type t = a) (x : a) ->
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
    let module Checks = Irmin_pack.Checks.Index (Index) in
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
      skip h >|= fun should_skip ->
      Irmin_layers.Stats.skip_test should_skip;
      should_skip

    let no_skip _ = Lwt.return false

    let pred_node t k =
      let n = snd (X.Repo.node_t t) in
      X.Node.CA.find n k >|= function
      | None -> []
      | Some v ->
          List.rev_map
            (function `Inode x -> `Node x | (`Node _ | `Contents _) as x -> x)
            (X.Node.CA.Val.pred v)

    let always_false _ = false
    let with_cancel cancel f = if cancel () then Lwt.fail Cancelled else f ()

    let iter_copy (contents, nodes, commits) ?(skip_commits = no_skip)
        ?(cancel = always_false) ?(skip_nodes = no_skip)
        ?(skip_contents = no_skip) t ?(min = []) max =
      (* if node or contents are already in dst then they are skipped by
         Graph.iter; there is no need to check this again when the object is
         copied *)
      let commit k =
        with_cancel cancel @@ fun () ->
        X.Commit.CA.copy commits t.X.Repo.commit "Commit" k;
        Irmin_layers.Stats.freeze_yield ();
        let* () = Lwt.pause () in
        Irmin_layers.Stats.freeze_yield_end ();
        Lwt.return_unit
      in
      let node k =
        with_cancel cancel @@ fun () ->
        X.Node.CA.copy nodes t.X.Repo.node k;
        Lwt.return_unit
      in
      let contents k =
        with_cancel cancel @@ fun () ->
        X.Contents.CA.copy contents t.X.Repo.contents "Contents" k;
        Lwt.return_unit
      in
      let skip_node h = skip_with_stats ~skip:skip_nodes h in
      let skip_contents h = skip_with_stats ~skip:skip_contents h in
      let skip_commit h = skip_with_stats ~skip:skip_commits h in
      let+ () =
        Repo.iter ~cache_size ~min ~max ~commit ~node ~contents ~skip_node
          ~skip_contents ~pred_node ~skip_commit t
      in
      X.Repo.flush t

    module CopyToLower = struct
      let on_lower t f =
        let contents =
          (X.Contents.CA.Lower, X.Contents.CA.lower t.X.Repo.contents)
        in
        let nodes = (X.Node.CA.Lower, X.Node.CA.lower t.X.Repo.node) in
        let commits = (X.Commit.CA.Lower, X.Commit.CA.lower t.X.Repo.commit) in
        f (contents, nodes, commits)

      let copy ?cancel ?(min = []) t commits =
        Log.debug (fun f ->
            f "@[<2>copy to lower:@ min=%a,@ max=%a@]" pp_commits min pp_commits
              commits);
        let max = List.map (fun x -> `Commit (Commit.hash x)) commits in
        let min = List.map (fun x -> `Commit (Commit.hash x)) min in
        on_lower t (fun l ->
            iter_copy ?cancel l ~skip_commits:(mem_commit_lower t)
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

      let copy ?cancel ?(min = []) t commits =
        Log.debug (fun f ->
            f "@[<2>copy to next upper:@ min=%a,@ max=%a@]" pp_commits min
              pp_commits commits);
        let max = List.map (fun x -> `Commit (Commit.hash x)) commits in
        let min = List.map (fun x -> `Commit (Commit.hash x)) min in
        on_next_upper t (fun u ->
            iter_copy ?cancel u ~skip_commits:(mem_commit_next t)
              ~skip_nodes:(mem_node_next t) ~skip_contents:(mem_contents_next t)
              ~min t max)

      (** Newies are the objects added in current upper during the freeze. They
          are copied to the next upper before the freeze ends. When copying the
          newies we have to traverse them as well, to ensure that all objects
          used by a newies are also copied in the next upper. We only keep track
          of commit newies and rely on `Repo.iter` to compute the transitive
          closures of all the newies. *)
      let copy_newies ~cancel t =
        let newies = X.Commit.CA.consume_newies t.X.Repo.commit in
        let newies = List.rev_map (fun x -> `Commit x) newies in
        Log.debug (fun l -> l "copy newies");
        (* we want to copy all the new commits; stop whenever one
           commmit already in the other upper or in lower. *)
        let skip_commits k =
          mem_commit_next t k >>= function
          | true -> Lwt.return true
          | false -> mem_commit_lower t k
        in
        on_next_upper t (fun u ->
            iter_copy u ?cancel ~skip_commits ~skip_nodes:(mem_node_next t)
              ~skip_contents:(mem_contents_next t) t newies)
        >>= fun () -> X.Branch.copy_newies_to_next_upper t.branch

      (** Repeatedly call [copy_newies] as long as there are many newies (more
          than newies_limit bytes added). *)
      let rec copy_newies_to_next_upper ~cancel t former_offset =
        let newies_limit = Int63.of_int t.X.Repo.blocking_copy_size in
        let offset = X.Repo.offset t in
        if offset -- former_offset >= newies_limit then (
          Irmin_layers.Stats.copy_newies_loop ();
          copy_newies ~cancel t >>= fun () ->
          (copy_newies_to_next_upper ~cancel t offset [@tail]))
        else Lwt.return_unit
    end

    module CopyFromLower = struct
      (* FIXME(samoht): copy/paste from iter_copy with s/copy/copy_from_lower *)
      let iter_copy (contents, nodes, commits) ?(skip_commits = no_skip)
          ?(cancel = always_false) ?(skip_nodes = no_skip)
          ?(skip_contents = no_skip) t ?(min = []) cs =
        (* if node or contents are already in dst then they are skipped by
           Graph.iter; there is no need to check this again when the object is
           copied *)
        let commit k =
          with_cancel cancel @@ fun () ->
          X.Commit.CA.copy_from_lower ~dst:commits t.X.Repo.commit "Commit" k
        in
        let node k =
          with_cancel cancel @@ fun () ->
          X.Node.CA.copy_from_lower ~dst:nodes t.X.Repo.node k
        in
        let contents k =
          with_cancel cancel @@ fun () ->
          X.Contents.CA.copy_from_lower ~dst:contents t.X.Repo.contents
            "Contents" k
        in
        let skip_node h = skip_with_stats ~skip:skip_nodes h in
        let skip_contents h = skip_with_stats ~skip:skip_contents h in
        let skip_commit h = skip_with_stats ~skip:skip_commits h in
        let max = List.map (fun c -> `Commit c) cs in
        let min = List.map (fun c -> `Commit c) min in
        let+ () =
          Repo.iter ~cache_size ~min ~max ~commit ~node ~contents ~skip_node
            ~skip_contents ~pred_node ~skip_commit t
        in
        X.Repo.flush t

      let on_current_upper t f =
        let contents = X.Contents.CA.current_upper t.X.Repo.contents in
        let nodes = X.Node.CA.current_upper t.X.Repo.node in
        let commits = X.Commit.CA.current_upper t.X.Repo.commit in
        f (contents, nodes, commits)

      (** An object can be in either lower or upper or both. We can't skip an
          object already in upper as some predecessors could still be in lower. *)
      let self_contained ?min ~max t =
        let max = List.map (fun x -> Commit.hash x) max in
        let min =
          match min with
          | None -> max (* if min is empty then copy only the max commits *)
          | Some min -> List.map (fun x -> Commit.hash x) min
        in
        (* FIXME(samoht): do this in 2 steps: 1/ find the shallow
           hashes in upper 2/ iterates with max=shallow

           (ngoguey): we could stop at the uppers directly following a lower.
        *)
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

  let copy ~cancel ~min_lower ~max_lower ~min_upper ~max_upper t =
    (* Copy commits to lower.
       In case cancellation of the freeze, copies to the lower layer will not
       be reverted. Since the copying is performed in the [rev] order, the next
       freeze will resume copying where the previous freeze stopped. *)
    Irmin_layers.Stats.freeze_section "copy to lower";
    (if t.X.Repo.with_lower then
     Copy.CopyToLower.copy ~cancel t ~min:min_lower max_lower
    else Lwt.return_unit)
    >>= fun () ->
    (* Copy [min_upper, max_upper] to next_upper. In case of cancellation of the
       freeze, the next upper will be cleared. *)
    Irmin_layers.Stats.freeze_section "copy to next upper";
    Copy.CopyToUpper.copy t ~cancel ~min:min_upper max_upper >>= fun () ->
    Irmin_layers.Stats.freeze_section "copy branches";
    (* Copy branches to both lower and next_upper *)
    Copy.copy_branches t

  module Field = struct
    type t = F : 'a Fmt.t * string * 'a -> t | E

    let pp ppf = function E -> () | F (pp, k, v) -> Fmt.pf ppf "%s=%a" k pp v

    let pps ppf t =
      Fmt.list ~sep:(Fmt.unit "; ") pp ppf (List.filter (fun x -> x <> E) t)

    let commits k = function [] -> E | v -> F (pp_commits, k, v)
  end

  let pp_repo ppf t =
    Fmt.pf ppf "%a" Layered_store.pp_current_upper t.X.Repo.flip

  let unsafe_freeze ~min_lower ~max_lower ~min_upper ~max_upper ?hook t =
    Log.info (fun l ->
        l "[%a] freeze starts { %a }" pp_repo t Field.pps
          [
            Field.commits "min_lower" min_lower;
            Field.commits "max_lower" max_lower;
            Field.commits "min_upper" min_upper;
            Field.commits "max_upper" max_upper;
          ]);
    let offset = X.Repo.offset t in
    let lock_file = lock_path t.root in
    (* We take a file lock here to signal that a freeze was in progess in
       case of crash, to trigger the recovery path. *)
    let* lock_file = Lock.v lock_file in
    let cancel () = t.freeze.state = `Cancel in
    let copy () =
      may (fun f -> f `Before_Copy) hook >>= fun () ->
      copy ~cancel ~min_lower ~max_lower ~min_upper ~max_upper t >>= fun () ->
      Irmin_layers.Stats.freeze_section "flush lower";
      X.Repo.flush_next_lower t;
      may (fun f -> f `Before_Copy_Newies) hook >>= fun () ->
      Irmin_layers.Stats.freeze_section "copy newies (loop)";
      Copy.CopyToUpper.copy_newies_to_next_upper ~cancel:(Some cancel) t offset
      >>= fun () ->
      may (fun f -> f `Before_Copy_Last_Newies) hook >>= fun () ->
      (* Let's finish the freeze under the batch lock so that no concurrent
         modifications occur until the uppers are flipped. No more cancellations
         from this point on. There are only a few newies left (less than
         [newies_limit] bytes) so this lock should be quickly released. *)
      Irmin_layers.Stats.freeze_section "wait for batch lock";
      Irmin_layers.Stats.freeze_yield ();
      Lwt_mutex.with_lock t.batch_lock (fun () ->
          Irmin_layers.Stats.freeze_yield_end ();
          Irmin_layers.Stats.freeze_section "copy newies (last)";
          Copy.CopyToUpper.copy_newies ~cancel:None t >>= fun () ->
          Irmin_layers.Stats.freeze_section "misc";
          may (fun f -> f `Before_Flip) hook >>= fun () ->
          X.Repo.flip_upper t;
          may (fun f -> f `Before_Clear) hook >>= fun () ->
          X.Repo.clear_previous_upper t)
      >>= fun () ->
      (* RO reads generation from pack file to detect a flip change, so it's
         ok to write the flip file outside the lock *)
      X.Repo.write_flip t
    in
    let finalize cancelled () =
      Irmin_layers.Stats.freeze_section "finalize";
      t.freeze.state <- `None;
      (if cancelled then X.Repo.clear_previous_upper ~keep_generation:() t
      else Lwt.return_unit)
      >>= fun () ->
      Lock.close lock_file >>= fun () ->
      Lwt_mutex.unlock t.freeze.lock;
      may (fun f -> f `After_Clear) hook >|= fun () ->
      Irmin_layers.Stats.freeze_stop ();
      (* Fmt.pr "\n%a%!" Irmin_layers.Stats.pp_latest (); *)
      ()
    in
    let async () =
      Lwt.try_bind copy (finalize false) (function
        | Cancelled -> finalize true ()
        | e -> Lwt.fail e)
    in
    Lwt.async async;
    Lwt.return_unit

  (** Main thread takes the [t.freeze.lock] at the begining of freeze and async
      thread releases it at the end. This is to ensure that no two freezes can
      run simultaneously. *)
  let freeze' ?min_lower ?max_lower ?min_upper ?max_upper ?(recovery = false)
      ?hook t =
    let* () =
      if recovery then X.Repo.clear_previous_upper ~keep_generation:() t
      else Lwt.return_unit
    in
    let freeze () =
      let t0 = Mtime_clock.now () in
      Lwt_mutex.lock t.freeze.lock >>= fun () ->
      t.freeze.state <- `Running;
      Irmin_layers.Stats.freeze_start t0 "wait for freeze lock";
      Irmin_layers.Stats.freeze_section "misc";
      let min_lower = Option.value min_lower ~default:[] in
      let* max_lower =
        match max_lower with Some l -> Lwt.return l | None -> Repo.heads t
      in
      let max_upper = Option.value max_upper ~default:max_lower in
      let min_upper = Option.value min_upper ~default:max_upper in
      unsafe_freeze ~min_lower ~max_lower ~min_upper ~max_upper ?hook t
    in
    if t.X.Repo.closed then Lwt.fail_with "store is closed"
    else if t.readonly then raise Irmin_pack.RO_not_allowed
    else
      match (t.freeze.state, t.freeze.throttle) with
      | `Running, `Overcommit_memory -> Lwt.return ()
      | `Running, `Cancel_existing ->
          t.freeze.state <- `Cancel;
          freeze ()
      | _ -> freeze ()

  let layer_id = X.Repo.layer_id
  let freeze = freeze' ?hook:None
  let async_freeze (t : Repo.t) = Lock.test (lock_path t.X.Repo.root)
  let upper_in_use = X.Repo.upper_in_use
  let self_contained = Copy.CopyFromLower.self_contained
  let needs_recovery t = Lock.test (lock_path t.X.Repo.root)

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
    let* heads =
      match heads with None -> Repo.heads t | Some m -> Lwt.return m
    in
    let hashes = List.map (fun x -> `Commit (Commit.hash x)) heads in
    let+ () =
      Repo.iter ~cache_size ~min:[] ~max:hashes ~commit ~node ~contents t
    in
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

  module Private_layer = struct
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

module Maker (C : Conf.Pack.S) = struct
  type endpoint = unit

  module Make (S : Irmin.Schema.S) = Maker' (C) (S)
end
