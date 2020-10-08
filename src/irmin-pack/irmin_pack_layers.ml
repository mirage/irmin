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
  module Pack_mem = Pack_mem.File (Index) (H)

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

    let decode_magic = Irmin.Type.(unstage (decode_bin char))

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        module Elt = struct
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
        end

        module CA_Pack = Pack.Make (Elt)
        module CA = Closeable.Content_addressable (CA_Pack)
        module CA_mem = Pack_mem.Make (Elt)
        include Layered_store.Content_addressable (H) (Index) (CA_mem) (CA)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module Pack_maker = Layered_store.Pack_Maker (H) (Index) (Pack_mem) (Pack)
      module CA = Inode_layers.Make (Config) (H) (Pack_maker) (Node)
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        module Elt = struct
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
        end

        module CA_Pack = Pack.Make (Elt)
        module CA = Closeable.Content_addressable (CA_Pack)
        module CA_mem = Pack_mem.Make (Elt)
        include Layered_store.Content_addressable (H) (Index) (CA_mem) (CA)
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
        index : Index.t option;
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
        uppers_index : Index.t option * Index.t option;
        mutable flip : bool;
        mutable closed : bool;
        flip_file : IO_layers.t;
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

      let iter ~former_offset ~offset ~io t =
        Log.debug (fun l ->
            l "iter over io %s former offset %Ld offset %Ld" (IO.name io)
              former_offset offset);
        let len = Int64.to_int (offset -- former_offset) in
        let raw = Bytes.create len in
        let n = IO.read io ~off:former_offset raw in
        assert (n = len);
        let raw = Bytes.unsafe_to_string raw in
        let decode_key = Irmin.Type.(unstage (decode_bin H.t)) in
        let rec read_entry page off =
          if off >= len then ()
          else
            let off_k, k = decode_key page off in
            assert (off_k - off = H.hash_size);
            let new_off, m = decode_magic page off_k in
            assert (new_off - off = H.hash_size + 1);
            let new_off =
              match m with
              | 'B' ->
                  let new_off, v = Contents.CA.U.decode_value page new_off in
                  Contents.CA.add_in_mem t.contents k v;
                  new_off
              | 'N' | 'I' ->
                  let new_off, v = Node.CA.U.decode_value page new_off in
                  Node.CA.add_in_mem t.node k v;
                  new_off
              | 'C' ->
                  let new_off, v = Commit.CA.U.decode_value page new_off in
                  Commit.CA.add_in_mem t.commit k v;
                  new_off
              | _ -> invalid_arg "unknown content type"
            in
            read_entry page new_off
        in
        (read_entry [@tailcall]) raw 0

      let refill ?(from_scratch = false) t =
        let former_offset, offset, io = Contents.CA.refill t.contents in
        let former_offset = if from_scratch then 0L else former_offset in
        iter ~former_offset ~offset ~io t

      let unsafe_v_upper root config =
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        Contents.CA.U.v ~fresh ~readonly ~lru_size ~index:None root
        >>= fun contents ->
        Node.CA.U.v ~fresh ~readonly ~lru_size ~index:None root >>= fun node ->
        Commit.CA.U.v ~fresh ~readonly ~lru_size ~index:None root
        >>= fun commit ->
        Branch.U.v ~fresh ~readonly root >|= fun branch ->
        ({ index = None; contents; node; commit; branch } : upper_layer)

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
        Contents.CA.L.v ~fresh ~readonly ~lru_size ~index:(Some index) root
        >>= fun lcontents ->
        Node.CA.L.v ~fresh ~readonly ~lru_size ~index:(Some index) root
        >>= fun lnode ->
        Commit.CA.L.v ~fresh ~readonly ~lru_size ~index:(Some index) root
        >>= fun lcommit ->
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
        let t =
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
        in
        refill ~from_scratch:true t;
        t

      let unsafe_close t =
        t.closed <- true;
        (match t.lower_index with Some x -> Index.close x | None -> ());
        Option.iter Index.close (fst t.uppers_index);
        Option.iter Index.close (snd t.uppers_index);
        IO_layers.close t.flip_file >>= fun () ->
        let f : unit Lwt.t Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.close x);
          }
        in
        Iterate.iter_lwt f t

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
        let f : unit Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.update_flip ~flip x);
          }
        in
        Iterate.iter f t;
        refill t

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

      (** Newies are the objects added in current upper during the freeze. They
          are copied to the next upper before the freeze ends. We do not lock
          this operation if we have to copy more than [newies_limit] bytes. If
          there are fewer newies than that then [copy_last_newies_to_next_upper]
          is called right after. *)
      let rec copy_newies_to_next_upper t former_offset =
        let offset = offset t in
        if offset -- former_offset >= newies_limit then
          let f : unit Lwt.t Iterate.store_fn =
            {
              f =
                (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                  C.copy_newies_to_next_upper x);
            }
          in
          Iterate.iter_lwt f t >>= fun () -> copy_newies_to_next_upper t offset
        else Lwt.return_unit

      let copy_last_newies_to_next_upper t =
        let f : unit Lwt.t Iterate.store_fn =
          {
            f =
              (fun (type a) (module C : S.LAYERED with type t = a) (x : a) ->
                C.copy_last_newies_to_next_upper x);
          }
        in
        Iterate.iter_lwt f t
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
    let lower =
      match t.lower_index with
      | None -> Ok `No_error
      | Some index ->
          integrity_check_layer ~layer:`Lower index
          |> Result.map_error (function e -> (e, `Lower))
    in
    [ lower; Ok `No_error; Ok `No_error ]

  include Irmin.Of_private (X)

  let sync = X.Repo.sync

  let clear = X.Repo.clear

  let migrate = X.Repo.migrate

  let flush = X.Repo.flush

  let pause = Lwt.pause

  module Copy = struct
    let mem_commit_lower t = X.Commit.CA.mem_lower t.X.Repo.commit

    let mem_commit_upper t = X.Commit.CA.mem_next t.X.Repo.commit

    let mem_commit_current t = X.Commit.CA.mem_current t.X.Repo.commit

    let mem_node_lower t = X.Node.CA.mem_lower t.X.Repo.node

    let mem_node_upper t = X.Node.CA.mem_next t.X.Repo.node

    let mem_node_current t = X.Node.CA.mem_current t.X.Repo.node

    let mem_contents_lower t = X.Contents.CA.mem_lower t.X.Repo.contents

    let mem_contents_upper t = X.Contents.CA.mem_next t.X.Repo.contents

    let mem_contents_current t = X.Contents.CA.mem_current t.X.Repo.contents

    let copy_branches t =
      X.Branch.copy ~mem_commit_lower:(mem_commit_lower t)
        ~mem_commit_upper:(mem_commit_upper t) t.X.Repo.branch

    let skip_with_stats ~skip h =
      pause () >>= fun () ->
      skip h >|= function
      | true ->
          Irmin_layers.Stats.skip ();
          true
      | false -> false

    let copy_tree ~skip ~skip_contents nodes contents t root =
      (* if node are already in dst then they are skipped by Graph.iter; there
         is no need to check this again when the node is copied *)
      let node k =
        pause () >>= fun () -> X.Node.CA.copy nodes t.X.Repo.node k >>= pause
      in
      (* we need to check that the contents is not already in dst, to avoid
         copying it again *)
      let contents (k, _) =
        skip_with_stats ~skip:skip_contents k >>= function
        | false -> X.Contents.CA.copy contents t.X.Repo.contents "Contents" k
        | true -> Lwt.return_unit
      in
      let skip h = skip_with_stats ~skip h in
      Repo.iter_nodes t ~min:[] ~max:[ root ] ~node ~contents ~skip ()

    let copy_commit ~skip ~skip_contents contents nodes commits t k =
      let aux c =
        copy_tree ~skip ~skip_contents nodes contents t (X.Commit.Val.node c)
      in
      X.Commit.CA.copy commits t.X.Repo.commit ~aux "Commit" k

    module CopyToLower = struct
      let on_lower t f =
        let contents = X.Contents.CA.lower t.X.Repo.contents in
        let nodes = X.Node.CA.lower t.X.Repo.node in
        let commits = X.Commit.CA.lower t.X.Repo.commit in
        f contents nodes commits

      let copy_commit contents nodes commits t =
        copy_commit ~skip:(mem_node_lower t)
          ~skip_contents:(mem_contents_lower t)
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
        let skip h = skip_with_stats ~skip:(mem_commit_lower t) h in
        on_lower t (fun contents nodes commits ->
            let commit = copy_commit contents nodes commits t in
            Repo.iter_commits t ~min ~max ~commit ~skip ())
    end

    module CopyToUpper = struct
      (** Skip if object is not in current; this check is cheap with the mem
          upper layer. *)
      let skip_node t h =
        mem_node_current t h >>= function
        | false -> Lwt.return_true
        | true -> mem_node_upper t h

      let skip_commit t h =
        mem_commit_current t h >>= function
        | false -> Lwt.return_true
        | true -> mem_commit_upper t h

      let skip_contents t h =
        mem_contents_current t h >>= function
        | false -> Lwt.return_true
        | true -> mem_contents_upper t h

      let on_next_upper t f =
        let contents = X.Contents.CA.next_upper t.X.Repo.contents in
        let nodes = X.Node.CA.next_upper t.X.Repo.node in
        let commits = X.Commit.CA.next_upper t.X.Repo.commit in
        f contents nodes commits

      let copy_commit contents nodes commits t =
        copy_commit ~skip:(skip_node t) ~skip_contents:(skip_contents t)
          (X.Contents.CA.Upper, contents)
          (X.Node.CA.Upper, nodes)
          (X.Commit.CA.Upper, commits)
          t

      let copy ~min ~max t =
        let skip h = skip_with_stats ~skip:(skip_commit t) h in
        on_next_upper t (fun contents nodes commits ->
            let commit = copy_commit contents nodes commits t in
            Repo.iter_commits t ~min ~max ~commit ~skip ())

      let copy_commits ~min ~max t =
        Log.debug (fun f -> f "copy commits to next upper");
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
        let ok = ref false in
        let commit x =
          if includes min x then ok := true;
          Lwt.return_unit
        in
        Repo.iter_commits t ~min ~max:[ head ] ~commit () >|= fun () -> !ok

      (** Copy the heads that include a max commit *)
      let copy_heads ~max ~heads t =
        Log.debug (fun f ->
            f "copy heads %a to next upper" (Fmt.list Commit.pp_hash) heads);
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

    module CopyFromLower = struct
      let on_current_upper t f =
        let contents = X.Contents.CA.current_upper t.X.Repo.contents in
        let nodes = X.Node.CA.current_upper t.X.Repo.node in
        let commits = X.Commit.CA.current_upper t.X.Repo.commit in
        f contents nodes commits

      let copy_tree contents nodes t root =
        let node k =
          X.Node.CA.copy_from_lower ~dst:nodes t.X.Repo.node k >|= fun () ->
          (* We have to ensure that a node is entirely flushed to disk.
             If we rely on the implicit flush of io, it could be that
             only a part of a node if flushed to disk when RO tries to
             read it. *)
          X.Node.CA.flush t.node
        in
        let contents (k, _) =
          X.Contents.CA.copy_from_lower ~dst:contents t.X.Repo.contents
            "Contents" k
        in
        Repo.iter_nodes t ~min:[] ~max:[ root ] ~node ~contents ()

      let copy_commit contents nodes commits t hash =
        let aux c = copy_tree contents nodes t (X.Commit.Val.node c) in
        X.Commit.CA.copy_from_lower t.X.Repo.commit ~dst:commits ~aux "Commit"
          hash
        >|= fun () -> X.Repo.flush t

      (** The commits can be in either lower or upper. We don't skip an object
          already in upper as its predecessors could be in lower. *)
      let self_contained ?min ~max t =
        let max = List.map (fun x -> Commit.hash x) max in
        let min =
          match min with
          | None -> max (* if min is empty then copy only the max commits *)
          | Some min -> List.map (fun x -> Commit.hash x) min
        in
        Log.debug (fun l ->
            l
              "self_contained: copy commits min:%a; max:%a from lower into \
               upper to make the upper self contained"
              (Fmt.list (Irmin.Type.pp H.t))
              min
              (Fmt.list (Irmin.Type.pp H.t))
              max);
        on_current_upper t (fun contents nodes commits ->
            let commit h = copy_commit contents nodes commits t h in
            Repo.iter_commits t ~min ~max ~commit ())
    end
  end

  let pp_stats msg =
    let stats = Irmin_layers.Stats.get () in
    Log.app (fun l ->
        l "%s contents = %d, nodes = %d, commits = %d, skips = %d" msg
          (List.hd stats.copied_contents)
          (List.hd stats.copied_nodes)
          (List.hd stats.copied_commits)
          stats.skips)

  let copy ~min ~max ~squash ~copy_in_upper ~min_upper ~heads t =
    (* Copy commits to lower: if squash then copy only the max commits *)
    let with_lower = with_lower t.X.Repo.config in
    (if with_lower then
     if squash then Copy.CopyToLower.copy_max_commits ~max t
     else Copy.CopyToLower.copy ~min ~max t
    else Lwt.return_unit)
    >>= fun () ->
    pp_stats "end of copied in lower";
    (* Copy [min_upper, max] and [max, heads] to next_upper *)
    (if copy_in_upper then
     Copy.CopyToUpper.copy_commits ~min:min_upper ~max t >>= fun () ->
     Copy.CopyToUpper.copy_heads ~max ~heads t
    else Lwt.return_unit)
    >>= fun () ->
    pp_stats "end of copied in upper";
    (* Copy branches to both lower and next_upper *)
    Copy.copy_branches t

  let unsafe_freeze ~min ~max ~squash ~copy_in_upper ~min_upper ~heads ?hook t =
    let pp_commits = Fmt.list Commit.pp_hash in
    Log.app (fun l ->
        l
          "unsafe_freeze min = %a max = %a squash = %b copy_in_upper = %b \
           min_upper = %a heads = %a"
          pp_commits min pp_commits max squash copy_in_upper pp_commits
          min_upper pp_commits heads);
    Irmin_layers.Stats.freeze ();
    let offset = X.Repo.offset t in
    let async () =
      let waiting =
        (Irmin_layers.Stats.get ()).waiting_freeze |> List.hd |> fun w ->
        w *. 0.000001
      in
      if waiting >= 1.0 then
        Log.warn (fun l ->
            l
              "freeze blocked for %f seconds due to a previous unfinished \
               freeze"
              waiting);
      pause () >>= fun () ->
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
      Log.app (fun l -> l "end freeze");
      may (fun f -> f `After_Clear) hook
    in
    Lwt.async (fun () -> Irmin_layers.Stats.with_timer `Freeze async);
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
      Irmin_layers.Stats.with_timer `Waiting (fun () ->
          Lwt_mutex.lock freeze_lock)
      >>= fun () ->
      unsafe_freeze ~min ~max ~squash ~copy_in_upper ~min_upper ~heads ?hook t

  let layer_id = X.Repo.layer_id

  let freeze = freeze' ?hook:None

  let async_freeze () = Lwt_mutex.is_locked freeze_lock

  let upper_in_use = X.Repo.upper_in_use

  let self_contained = Copy.CopyFromLower.self_contained

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
