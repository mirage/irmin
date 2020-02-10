(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *                         Ioana Cristescu <ioana@tarides.com>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.layered" ~doc:"Irmin layered store"

module Log = (val Logs.src_log src : Logs.LOG)

module Conf = Irmin.Private.Conf

module Default = struct
  let lower_root = "lower"

  let upper_root1 = "upper1"

  let upper_root0 = "upper0"
end

let root_key = Conf.root

let root conf = match Conf.get conf Conf.root with None -> "" | Some r -> r

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

let config ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root1 = Default.upper_root1) ?(upper_root0 = Default.upper_root0)
    root =
  let config = Conf.add conf lower_root_key lower_root in
  let config = Conf.add config upper_root0_key upper_root0 in
  let config = Conf.add config root_key (Some root) in
  let config = Conf.add config upper_root1_key upper_root1 in
  config

let reset_lock = Lwt_mutex.create ()

let freeze_lock = Lwt_mutex.create ()

module type STORE = sig
  include Irmin.S

  val freeze :
    ?min:commit list -> ?max:commit list -> ?squash:bool -> repo -> unit Lwt.t

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  val layer_id : repo -> store_handle -> string Lwt.t

  val async_freeze : unit -> bool

  val upper_in_use : repo -> string

  module PrivateLayer : sig
    module Hook : sig
      type 'a t

      val v : ('a -> unit) -> 'a t
    end

    val wait_for_freeze : unit -> unit Lwt.t
  end
end

module Make_ext
    (L : Irmin.S)
    (U : Irmin.S
           with type step = L.step
            and type key = L.key
            and type metadata = L.metadata
            and type contents = L.contents
            and type hash = L.hash
            and type branch = L.branch
            and type Private.Node.value = L.Private.Node.value
            and type Private.Commit.value = L.Private.Commit.value) :
  STORE
    with type step = U.step
     and type key = U.key
     and type metadata = U.metadata
     and type contents = U.contents
     and type hash = U.hash
     and type branch = U.branch = struct
  type store_handle =
    | Commit_t : U.Private.Hash.t -> store_handle
    | Node_t : U.Private.Hash.t -> store_handle
    | Content_t : U.Private.Hash.t -> store_handle

  module X = struct
    module Hash = U.Private.Hash

    module Contents = struct
      module L = L.Private.Contents
      module U = U.Private.Contents

      module CA = struct
        module Key = Hash
        module Val = L.Val
        include Layered.Content_addressable (Key) (Val) (L) (U)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module L = L.Private.Node
      module U = U.Private.Node
      module P = U.Path
      module M = U.Metadata

      module CA = struct
        module Key = Hash
        module Val = L.Val
        include Layered.Content_addressable (Key) (Val) (L) (U)
      end

      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module L = L.Private.Commit
      module U = U.Private.Commit

      module CA = struct
        module Key = Hash
        module Val = L.Val
        include Layered.Content_addressable (Key) (Val) (L) (U)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module L = L.Private.Branch
      module U = U.Private.Branch
      module Key = U.Key
      module Val = Hash
      include Layered.Atomic_write (Key) (Val) (L) (U)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (Hash) (U.Branch)

    module Repo = struct
      module Upper = U
      module UP = U.Private
      module LP = L.Private
      module U = UP.Repo
      module L = LP.Repo

      type t = {
        contents : [ `Read ] Contents.CA.t;
        nodes : [ `Read ] Node.CA.t;
        commits : [ `Read ] Commit.CA.t;
        branch : Branch.t;
        lower : L.t;
        uppers : U.t * U.t;
        mutable flip : bool;
        mutable closed : bool;
        conf : Conf.t;
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.nodes)

      let commit_t t : 'a Commit.t = (node_t t, t.commits)

      let branch_t t = t.branch

      let current_upper t = if t.flip then fst t.uppers else snd t.uppers

      let previous_upper t = if t.flip then snd t.uppers else fst t.uppers

      let batch t f =
        U.batch (fst t.uppers) @@ fun b n c ->
        U.batch (snd t.uppers) @@ fun nb nn nc ->
        let contents_t = Contents.CA.project b nb t.contents in
        let node_t = Node.CA.project n nn t.nodes in
        let commit_t = Commit.CA.project c nc t.commits in
        let node_t = (contents_t, node_t) in
        let commit_t = (node_t, commit_t) in
        f contents_t node_t commit_t

      let unsafe_close t =
        t.closed <- true;
        L.close t.lower >>= fun () ->
        U.close (fst t.uppers) >>= fun () -> U.close (snd t.uppers)

      let close t = Lwt_mutex.with_lock freeze_lock (fun () -> unsafe_close t)

      let v conf =
        let upper_name = Filename.concat (root conf) (upper_root1 conf) in
        let conf_upper = Conf.add conf Conf.root (Some upper_name) in
        U.v conf_upper >>= fun upper1 ->
        let lower_name = Filename.concat (root conf) (lower_root conf) in
        let conf_lower = Conf.add conf Conf.root (Some lower_name) in
        L.v conf_lower >>= fun lower ->
        let upper_name = root conf ^ upper_root0 conf in
        let conf_upper = Conf.add conf Conf.root (Some upper_name) in
        U.v conf_upper >|= fun upper0 ->
        let contents =
          Contents.CA.v (U.contents_t upper1) (U.contents_t upper0)
            (L.contents_t lower) reset_lock
        in
        let nodes =
          Node.CA.v (U.node_t upper1) (U.node_t upper0) (L.node_t lower)
            reset_lock
        in
        let commits =
          Commit.CA.v (U.commit_t upper1) (U.commit_t upper0) (L.commit_t lower)
            reset_lock
        in
        let branch =
          Branch.v (U.branch_t upper1) (U.branch_t upper0) (L.branch_t lower)
            reset_lock
        in
        {
          contents;
          nodes;
          commits;
          branch;
          lower;
          uppers = (upper1, upper0);
          conf;
          flip = true;
          closed = false;
        }

      let flip_upper t =
        t.flip <- not t.flip;
        Contents.CA.flip_upper t.contents;
        Node.CA.flip_upper t.nodes;
        Commit.CA.flip_upper t.commits;
        Branch.flip_upper t.branch;
        Lwt.return_unit

      let layer_id t store_handler =
        ( match store_handler with
        | Commit_t k -> Commit.CA.layer_id t.commits k
        | Node_t k -> Node.CA.layer_id t.nodes k
        | Content_t k -> Contents.CA.layer_id t.contents k )
        >|= fun a ->
        match a with
        | 0 -> upper_root0 t.conf
        | 1 -> upper_root1 t.conf
        | 2 -> lower_root t.conf
        | _ -> failwith "unexpected layer id"

      let clear_upper t =
        Contents.CA.clear_upper t.contents >>= fun () ->
        Node.CA.clear_upper t.nodes >>= fun () ->
        Commit.CA.clear_upper t.commits >>= fun () ->
        Branch.clear_upper t.branch

      let copy_contents t contents k =
        Contents.CA.check_and_copy t.contents ~dst:contents
          ~aux:(fun _ -> Lwt.return_unit)
          "Contents" k

      (* [root] is the root of the tree pointed by a commit *)
      let copy_nodes t nodes contents root =
        Node.CA.already_in_dst ~dst:nodes root (fun root ->
            let aux v =
              Lwt_list.iter_p
                (function
                  | _, `Contents (k, _) -> copy_contents t contents k
                  | _ -> Lwt.return_unit)
                (UP.Node.Val.list v)
            in
            let node k = Node.CA.copy t.nodes ~dst:nodes ~aux "Node" k in
            let skip k = LP.Node.mem nodes k in
            Upper.Repo.iter (previous_upper t) ~min:[] ~max:[ root ] ~node ~skip)

      let copy_branches t dst = Branch.copy t.branch (LP.Commit.mem dst)

      let copy_commit t contents nodes commits k =
        let aux c = copy_nodes t nodes contents (UP.Commit.Val.node c) in
        Commit.CA.check_and_copy t.commits ~dst:commits ~aux "Commit" k
        >>= fun () -> copy_branches t commits

      let copy t ?(squash = false) ?(min = []) ?(max = []) () =
        Log.debug (fun f -> f "copy");
        ( match max with
        | [] -> Upper.Repo.heads (previous_upper t)
        | m -> Lwt.return m )
        >>= fun max ->
        Lwt.catch
          (fun () ->
            (* If squash then copy only the commits in [max]. Otherwise copy each
               commit individually. *)
            ( if squash then
              Lwt_list.iter_p
                (fun k ->
                  L.batch t.lower (fun contents nodes commits ->
                      copy_commit t contents nodes commits (Upper.Commit.hash k)))
                max
            else
              Upper.Repo.export ~full:false ~min ~max:(`Max max)
                (previous_upper t)
              >>= fun slice ->
              L.batch t.lower (fun contents nodes commits ->
                  UP.Slice.iter slice (function
                    | `Commit (k, _) -> copy_commit t contents nodes commits k
                    | _ -> Lwt.return_unit)) )
            >|= fun () -> Ok ())
          (function
            | Layered.Copy_error e -> Lwt.return_error (`Msg e)
            | e -> Fmt.kstrf Lwt.fail_invalid_arg "copy error: %a" Fmt.exn e)

      let clear t =
        L.clear t.lower >>= fun () ->
        U.clear (fst t.uppers) >>= fun () -> U.clear (snd t.uppers)

      let pre_copy t = Lwt_mutex.with_lock reset_lock (fun () -> flip_upper t)

      let post_copy t =
        clear_upper t >|= fun () ->
        Lwt_mutex.unlock freeze_lock;
        Log.debug (fun l -> l "free lock")

      let async_freeze () = Lwt_mutex.is_locked freeze_lock

      let upper_in_use t =
        if t.flip then upper_root1 t.conf else upper_root0 t.conf
    end
  end

  include Irmin.Of_private (X)

  let conv_commit upper c =
    let hash = Commit.hash c in
    U.Commit.of_hash upper hash

  let unsafe_freeze ~min ~max ~squash t =
    Log.debug (fun l -> l "unsafe_freeze");
    X.Repo.pre_copy t >|= fun () ->
    Lwt.async (fun () ->
        Lwt.pause () >>= fun () ->
        X.Repo.copy t ~squash ~min ~max () >>= function
        | Ok () -> X.Repo.post_copy t
        | Error (`Msg e) ->
            Fmt.kstrf Lwt.fail_with "[gc_store]: import error %s" e);
    Log.debug (fun l -> l "after async called to copy")

  let freeze ?(min : commit list = []) ?(max : commit list = [])
      ?(squash = false) t =
    let upper = X.Repo.current_upper t in
    let conv_commits =
      Lwt_list.fold_left_s
        (fun acc m ->
          conv_commit upper m >|= function None -> acc | Some c -> c :: acc)
        []
    in
    conv_commits min >>= fun min ->
    conv_commits max >>= fun max ->
    (* main thread takes the lock at the begining of freeze and async thread
       releases it at the end. This is to ensure that no two freezes can run
       simultaneously. *)
    Lwt_mutex.lock freeze_lock >>= fun () ->
    if t.closed then failwith "store is closed";
    unsafe_freeze ~min ~max ~squash t

  let layer_id = X.Repo.layer_id

  let async_freeze = X.Repo.async_freeze

  let upper_in_use = X.Repo.upper_in_use

  module PrivateLayer = struct
    module Hook = struct
      type 'a t = 'a -> unit

      let v f = f
    end

    let wait_for_freeze () =
      Lwt_mutex.with_lock freeze_lock (fun () -> Lwt.return_unit)
  end
end

module Make
    (Make : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module L = Make (M) (C) (P) (B) (H)
  module U = Make (M) (C) (P) (B) (H)
  include Make_ext (L) (U)
end

module type L_MAKER = functor
  (M : Irmin.Metadata.S)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  (H : Irmin.Hash.S)
  ->
  STORE
    with type step = P.step
     and type contents = C.t
     and type key = P.t
     and type branch = B.t
     and type metadata = M.t
     and type hash = H.t
