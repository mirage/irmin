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

  let upper_root = "upper"
end

let root_key = Conf.root

let root conf = match Conf.get conf Conf.root with None -> "" | Some r -> r

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Conf.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Conf.string Default.upper_root

let upper_root conf = Conf.get conf upper_root_key

let config ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root = Default.upper_root) root =
  let config = Conf.add conf lower_root_key lower_root in
  let config = Conf.add config root_key (Some root) in
  let config = Conf.add config upper_root_key upper_root in
  config

module type STORE = sig
  include Irmin.S

  val freeze :
    ?min:commit list -> ?max:commit list -> ?squash:bool -> repo -> unit Lwt.t

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  val layer_id : repo -> store_handle -> string Lwt.t
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
        upper : U.t;
        lower : L.t;
        conf : Conf.t;
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.nodes)

      let commit_t t : 'a Commit.t = (node_t t, t.commits)

      let branch_t t = t.branch

      let batch t f =
        U.batch t.upper @@ fun b n c ->
        let contents_t = Contents.CA.project b t.contents in
        let node_t = Node.CA.project n t.nodes in
        let commit_t = Commit.CA.project c t.commits in
        let node_t = (contents_t, node_t) in
        let commit_t = (node_t, commit_t) in
        f contents_t node_t commit_t

      let close t = U.close t.upper >>= fun () -> L.close t.lower

      let v conf =
        let upper_name = Filename.concat (root conf) (upper_root conf) in
        let conf_upper = Conf.add conf Conf.root (Some upper_name) in
        U.v conf_upper >>= fun upper ->
        let lower_name = Filename.concat (root conf) (lower_root conf) in
        let conf_lower = Conf.add conf Conf.root (Some lower_name) in
        L.v conf_lower >|= fun lower ->
        let contents =
          Contents.CA.v (U.contents_t upper) (L.contents_t lower)
        in
        let nodes = Node.CA.v (U.node_t upper) (L.node_t lower) in
        let commits = Commit.CA.v (U.commit_t upper) (L.commit_t lower) in
        let branch = Branch.v (U.branch_t upper) (L.branch_t lower) in
        { contents; nodes; commits; branch; upper; lower; conf }

      let layer_id t store_handler =
        ( match store_handler with
        | Commit_t k -> Commit.CA.layer_id t.commits k
        | Node_t k -> Node.CA.layer_id t.nodes k
        | Content_t k -> Contents.CA.layer_id t.contents k )
        >|= function
        | 1 -> upper_root t.conf
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
            Upper.Repo.iter t.upper ~min:[] ~max:[ root ] ~node ~skip)

      let copy_branches t dst = Branch.copy t.branch (LP.Commit.mem dst)

      let copy_commit t contents nodes commits k =
        let aux c = copy_nodes t nodes contents (UP.Commit.Val.node c) in
        Commit.CA.check_and_copy t.commits ~dst:commits ~aux "Commit" k
        >>= fun () -> copy_branches t commits

      let copy t ?(squash = false) ?(min = []) ?(max = []) () =
        Log.debug (fun f -> f "copy");
        (match max with [] -> Upper.Repo.heads t.upper | m -> Lwt.return m)
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
              Upper.Repo.export ~full:false ~min ~max:(`Max max) t.upper
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
        Contents.CA.clear t.contents >>= fun () ->
        Node.CA.clear t.nodes >>= fun () ->
        Commit.CA.clear t.commits >>= fun () -> Branch.clear t.branch
    end
  end

  include Irmin.Of_private (X)

  let conv_commit upper c =
    let hash = Commit.hash c in
    U.Commit.of_hash upper hash

  let freeze ?(min : commit list = []) ?(max : commit list = [])
      ?(squash = false) t =
    let upper = t.X.Repo.upper in
    let conv_commits =
      Lwt_list.fold_left_s
        (fun acc m ->
          conv_commit upper m >|= function None -> acc | Some c -> c :: acc)
        []
    in
    conv_commits min >>= fun min ->
    conv_commits max >>= fun max ->
    (* the lower branch store is cleared before a copy, so that branches removed
       between two freezes do not persist in the lower store *)
    X.Repo.copy t ~squash ~min ~max () >>= function
    | Ok () -> X.Repo.clear_upper t
    | Error (`Msg e) -> Fmt.kstrf Lwt.fail_with "[gc_store]: import error %s" e

  let layer_id t = X.Repo.layer_id t
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
