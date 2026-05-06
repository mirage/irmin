(*
 * Copyright (c) 2026 Tarides
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

(** Reusable Lwt-vs-Eio wrapping of an Irmin 4 [Generic_key.S] store. Threads
    [Schema_eio] explicitly so that [Inner.Schema = Schema_eio] binds the user's
    types through the whole [Backend] surface, including [Node_portable] /
    [Commit_portable] sub-modules. *)

let run = Lwt_eio.run_eio

module Make
    (S : Schema.S)
    (Schema_eio :
      Irmin.Schema.S
        with module Hash = S.Hash
         and module Branch = S.Branch
         and module Info = S.Info
         and module Path = S.Path
         and type Metadata.t = S.Metadata.t
         and type Contents.t = S.Contents.t)
    (Inner : Irmin.Generic_key.S with module Schema = Schema_eio) =
struct
  (* === Schema ===
     Expose the user's original Lwt-typed schema, not the Eio-adapted
     [Schema_eio] used internally to feed Irmin.Maker. *)
  module Schema = S

  (* === Top-level types === *)
  type repo = Inner.repo
  type t = Inner.t
  type step = Inner.step
  type path = Inner.path
  type metadata = Inner.metadata
  type contents = Inner.contents
  type node = Inner.node
  type tree = Inner.tree
  type hash = Inner.hash
  type commit = Inner.commit
  type branch = Inner.branch
  type slice = Inner.slice
  type info = Inner.info
  type lca_error = Inner.lca_error
  type ff_error = Inner.ff_error
  type contents_key = Inner.contents_key
  type node_key = Inner.node_key
  type commit_key = Inner.commit_key

  let step_t = Inner.step_t
  let path_t = Inner.path_t
  let metadata_t = Inner.metadata_t
  let contents_t = Inner.contents_t
  let node_t = Inner.node_t
  let tree_t = Inner.tree_t
  let hash_t = Inner.hash_t
  let commit_t = Inner.commit_t
  let branch_t = Inner.branch_t
  let slice_t = Inner.slice_t
  let info_t = Inner.info_t
  let lca_error_t = Inner.lca_error_t
  let ff_error_t = Inner.ff_error_t
  let contents_key_t = Inner.contents_key_t
  let node_key_t = Inner.node_key_t
  let commit_key_t = Inner.commit_key_t

  (* === Info === *)
  module Info = struct
    include (Inner.Info : module type of Inner.Info with type t = info)

    let pp = Inner.Info.pp
  end

  (* === Status === *)
  module Status = struct
    type t = Inner.Status.t

    let t = Inner.Status.t
    let pp = Inner.Status.pp
  end

  let status t = Inner.status t

  (* === Hash (pure) === *)
  module Hash = Inner.Hash

  (* === Top-level store builders === *)
  let empty r = run (fun () -> Inner.empty r)
  let main r = run (fun () -> Inner.main r)
  let of_branch r b = run (fun () -> Inner.of_branch r b)
  let of_commit c = run (fun () -> Inner.of_commit c)
  let repo t = Inner.repo t
  let tree t = run (fun () -> Inner.tree t)

  (* === Repo === *)
  module Repo = struct
    type t = repo

    let v c = run (fun () -> Inner.Repo.v c)
    let config = Inner.Repo.config
    let close r = run (fun () -> Inner.Repo.close r)
    let heads r = run (fun () -> Inner.Repo.heads r)
    let branches r = run (fun () -> Inner.Repo.branches r)

    let export ?full ?depth ?min ?max r =
      run (fun () -> Inner.Repo.export ?full ?depth ?min ?max r)

    let import r s = run (fun () -> Inner.Repo.import r s)

    type elt = Inner.Repo.elt

    let elt_t = Inner.Repo.elt_t

    let default_pred_commit r k =
      run (fun () -> Inner.Repo.default_pred_commit r k)

    let default_pred_node r k = run (fun () -> Inner.Repo.default_pred_node r k)

    let default_pred_contents r k =
      run (fun () -> Inner.Repo.default_pred_contents r k)

    let iter ?cache_size ~min ~max ?edge ?branch ?commit ?node ?contents
        ?skip_branch ?skip_commit ?skip_node ?skip_contents ?pred_branch
        ?pred_commit ?pred_node ?pred_contents ?rev r =
      let bridge1 f =
        Option.map (fun f x -> Lwt_eio.Promise.await_lwt (f x)) f
      in
      let bridge2 f =
        Option.map (fun f x y -> Lwt_eio.Promise.await_lwt (f x y)) f
      in
      run (fun () ->
          Inner.Repo.iter ?cache_size ~min ~max ?edge:(bridge2 edge)
            ?branch:(bridge1 branch) ?commit:(bridge1 commit)
            ?node:(bridge1 node) ?contents:(bridge1 contents)
            ?skip_branch:(bridge1 skip_branch)
            ?skip_commit:(bridge1 skip_commit) ?skip_node:(bridge1 skip_node)
            ?skip_contents:(bridge1 skip_contents)
            ?pred_branch:(bridge2 pred_branch)
            ?pred_commit:(bridge2 pred_commit) ?pred_node:(bridge2 pred_node)
            ?pred_contents:(bridge2 pred_contents) ?rev r)

    let breadth_first_traversal ?cache_size ~max ?branch ?commit ?node ?contents
        ?pred_branch ?pred_commit ?pred_node ?pred_contents r =
      let bridge1 f =
        Option.map (fun f x -> Lwt_eio.Promise.await_lwt (f x)) f
      in
      let bridge2 f =
        Option.map (fun f x y -> Lwt_eio.Promise.await_lwt (f x y)) f
      in
      run (fun () ->
          Inner.Repo.breadth_first_traversal ?cache_size ~max
            ?branch:(bridge1 branch) ?commit:(bridge1 commit)
            ?node:(bridge1 node) ?contents:(bridge1 contents)
            ?pred_branch:(bridge2 pred_branch)
            ?pred_commit:(bridge2 pred_commit) ?pred_node:(bridge2 pred_node)
            ?pred_contents:(bridge2 pred_contents) r)
  end

  (* === Head === *)
  module Head = struct
    let list r = run (fun () -> Inner.Head.list r)
    let find t = run (fun () -> Inner.Head.find t)
    let get t = run (fun () -> Inner.Head.get t)
    let set t c = run (fun () -> Inner.Head.set t c)

    let fast_forward t ?max_depth ?n c =
      run (fun () -> Inner.Head.fast_forward t ?max_depth ?n c)

    let test_and_set t ~test ~set =
      run (fun () -> Inner.Head.test_and_set t ~test ~set)

    let merge ~into ~info ?max_depth ?n c =
      run (fun () -> Inner.Head.merge ~into ~info ?max_depth ?n c)
  end

  (* === Commit === *)
  module Commit = struct
    type t = commit

    let t = Inner.Commit.t
    let pp_hash = Inner.Commit.pp_hash
    let pp = Inner.Commit.pp

    let v ?clear r ~info ~parents tree =
      run (fun () -> Inner.Commit.v ?clear r ~info ~parents tree)

    let tree = Inner.Commit.tree
    let parents = Inner.Commit.parents
    let info = Inner.Commit.info
    let hash = Inner.Commit.hash
    let key = Inner.Commit.key
    let of_key r k = run (fun () -> Inner.Commit.of_key r k)
    let of_hash r h = run (fun () -> Inner.Commit.of_hash r h)
  end

  (* === Contents === *)
  module Contents = struct
    type t = contents

    let t = Inner.Contents.t

    let merge =
      Lwt_to_eio.merge_of_eio
        Type.(option Inner.Contents.t)
        Inner.Contents.merge

    let hash = Inner.Contents.hash
    let of_key r k = run (fun () -> Inner.Contents.of_key r k)
    let of_hash r h = run (fun () -> Inner.Contents.of_hash r h)
  end

  (* === Branch === *)
  type watch = Inner.watch

  module Branch = struct
    let mem r b = run (fun () -> Inner.Branch.mem r b)
    let find r b = run (fun () -> Inner.Branch.find r b)
    let get r b = run (fun () -> Inner.Branch.get r b)
    let set r b c = run (fun () -> Inner.Branch.set r b c)
    let remove r b = run (fun () -> Inner.Branch.remove r b)
    let list r = run (fun () -> Inner.Branch.list r)

    let watch r b ?init f =
      run (fun () ->
          Inner.Branch.watch r b ?init (fun d ->
              Lwt_eio.Promise.await_lwt (f d)))

    let watch_all r ?init f =
      run (fun () ->
          Inner.Branch.watch_all r ?init (fun b d ->
              Lwt_eio.Promise.await_lwt (f b d)))

    let pp = Inner.Branch.pp

    (* Branch.S re-exports: Inner.Branch already satisfies the basic
       Branch.S (no Lwt in those fields), so we delegate the field set. *)
    type t = branch

    let t = Inner.Branch.t
    let main = Inner.Branch.main
    let is_valid = Inner.Branch.is_valid
  end

  (* === Path === *)
  module Path : Path.S with type t = path and type step = step = Inner.Path

  (* === Metadata === *)
  module Metadata = struct
    type t = metadata

    let t = Inner.Metadata.t
    let merge = Lwt_to_eio.merge_of_eio Inner.Metadata.t Inner.Metadata.merge
    let default = Inner.Metadata.default
  end

  (* === Watches (top-level) === *)
  let watch t ?init f =
    run (fun () ->
        Inner.watch t ?init (fun d -> Lwt_eio.Promise.await_lwt (f d)))

  let watch_key t k ?init f =
    run (fun () ->
        Inner.watch_key t k ?init (fun d -> Lwt_eio.Promise.await_lwt (f d)))

  let unwatch w = run (fun () -> Inner.unwatch w)

  (* === Merges === *)
  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result Lwt.t

  let merge_into ~into ~info ?max_depth ?n t =
    run (fun () -> Inner.merge_into ~into ~info ?max_depth ?n t)

  let merge_with_branch t ~info ?max_depth ?n b =
    run (fun () -> Inner.merge_with_branch t ~info ?max_depth ?n b)

  let merge_with_commit t ~info ?max_depth ?n c =
    run (fun () -> Inner.merge_with_commit t ~info ?max_depth ?n c)

  let lcas ?max_depth ?n t1 t2 = run (fun () -> Inner.lcas ?max_depth ?n t1 t2)

  let lcas_with_branch t ?max_depth ?n b =
    run (fun () -> Inner.lcas_with_branch t ?max_depth ?n b)

  let lcas_with_commit t ?max_depth ?n c =
    run (fun () -> Inner.lcas_with_commit t ?max_depth ?n c)

  (* === Clone === *)
  let clone ~src ~dst = run (fun () -> Inner.clone ~src ~dst)

  (* === History === *)
  module History = Inner.History

  let history ?depth ?min ?max t =
    run (fun () -> Inner.history ?depth ?min ?max t)

  (* === Backend ===
     Re-export pure sub-modules from Inner; wrap the Lwt-flavoured stores
     (Contents / Node / Commit / Branch) through Lwt_to_eio.{Indexable,
     Atomic_write}_of_eio, and wrap Repo's Lwt-typed v / close / batch
     through Lwt_eio.run_eio. *)
  module Backend = struct
    module Schema = S
    module Hash = Inner.Backend.Hash

    module Contents = struct
      include Lwt_to_eio.Indexable_of_eio (Inner.Backend.Contents)

      let merge t =
        Lwt_to_eio.merge_of_eio
          Type.(option Inner.Backend.Contents.Key.t)
          (Inner.Backend.Contents.merge t)

      (* Val needs the Lwt-typed Contents.S the user supplied, not the
         Eio-adapted [Schema_eio.Contents] that Inner sees. *)
      module Val = S.Contents
      module Hash = Inner.Backend.Contents.Hash
    end

    module Node = struct
      include Lwt_to_eio.Indexable_of_eio (Inner.Backend.Node)

      let merge t =
        Lwt_to_eio.merge_of_eio
          Type.(option Inner.Backend.Node.Key.t)
          (Inner.Backend.Node.merge t)

      (* Path and Metadata are exposed as the user's Lwt-typed versions
         (Metadata.merge differs in shape from Inner's Eio version). *)
      module Path = S.Path
      module Metadata = S.Metadata

      module Val = struct
        include Inner.Backend.Node.Val
        module Metadata = S.Metadata
        module Path = S.Path

        (* Bridge Val.merge (~contents ~node) Lwt <-> Eio. *)
        let merge ~contents ~node =
          let contents_eio =
            Lwt_to_eio.merge_to_eio
              Type.(option Inner.Backend.Contents.Key.t)
              contents
          in
          let node_eio =
            Lwt_to_eio.merge_to_eio Type.(option Inner.Backend.Node.Key.t) node
          in
          Lwt_to_eio.merge_of_eio Inner.Backend.Node.Val.t
            (Inner.Backend.Node.Val.merge ~contents:contents_eio ~node:node_eio)
      end

      module Hash = Inner.Backend.Node.Hash
      module Contents = Contents
    end

    module Node_portable = struct
      include Inner.Backend.Node_portable
      module Metadata = S.Metadata

      (* In Node.Portable.S [contents_key = node_key = hash]; the merge
         args are hashes, not the user's [node_key]. *)
      let merge ~contents ~node =
        let contents_eio =
          Lwt_to_eio.merge_to_eio Type.(option Inner.hash_t) contents
        in
        let node_eio =
          Lwt_to_eio.merge_to_eio Type.(option Inner.hash_t) node
        in
        Lwt_to_eio.merge_of_eio Inner.Backend.Node_portable.t
          (Inner.Backend.Node_portable.merge ~contents:contents_eio
             ~node:node_eio)
    end

    module Commit = struct
      include Lwt_to_eio.Indexable_of_eio (Inner.Backend.Commit)

      let merge t ~info =
        Lwt_to_eio.merge_of_eio
          Type.(option Inner.Backend.Commit.Key.t)
          (Inner.Backend.Commit.merge t ~info)

      module Info = S.Info
      module Val = Inner.Backend.Commit.Val
      module Hash = Inner.Backend.Commit.Hash
      module Node = Node
    end

    module Commit_portable = Inner.Backend.Commit_portable

    module Branch = struct
      include Lwt_to_eio.Atomic_write_of_eio (Inner.Backend.Branch)
      module Key = Inner.Backend.Branch.Key
      module Val = Inner.Backend.Branch.Val
    end

    module Slice = struct
      type t = Inner.Backend.Slice.t
      type contents = Inner.Backend.Slice.contents
      type node = Inner.Backend.Slice.node
      type commit = Inner.Backend.Slice.commit
      type value = Inner.Backend.Slice.value

      let t = Inner.Backend.Slice.t
      let contents_t = Inner.Backend.Slice.contents_t
      let node_t = Inner.Backend.Slice.node_t
      let commit_t = Inner.Backend.Slice.commit_t
      let value_t = Inner.Backend.Slice.value_t
      let empty () = run (fun () -> Inner.Backend.Slice.empty ())
      let add t v = run (fun () -> Inner.Backend.Slice.add t v)

      let iter t f =
        run (fun () ->
            Inner.Backend.Slice.iter t (fun v ->
                Lwt_eio.Promise.await_lwt (f v)))
    end

    module Repo = struct
      type t = Inner.Backend.Repo.t

      let v c = run (fun () -> Inner.Backend.Repo.v c)
      let close r = run (fun () -> Inner.Backend.Repo.close r)
      let contents_t = Inner.Backend.Repo.contents_t
      let node_t = Inner.Backend.Repo.node_t
      let commit_t = Inner.Backend.Repo.commit_t
      let config = Inner.Backend.Repo.config

      let batch r f =
        run (fun () ->
            Inner.Backend.Repo.batch r (fun c n c2 ->
                Lwt_eio.Promise.await_lwt (f c n c2)))

      let branch_t = Inner.Backend.Repo.branch_t
    end

    module Remote = struct
      type t = Inner.Backend.Remote.t
      type commit = Inner.Backend.Remote.commit
      type branch = Inner.Backend.Remote.branch
      type endpoint = Inner.Backend.Remote.endpoint

      let fetch t ?depth e b =
        run (fun () -> Inner.Backend.Remote.fetch t ?depth e b)

      let push t ?depth e b =
        run (fun () -> Inner.Backend.Remote.push t ?depth e b)

      let v r = run (fun () -> Inner.Backend.Remote.v r)
    end
  end

  (* === Top-level reads === *)
  let kind t p = run (fun () -> Inner.kind t p)
  let list t p = run (fun () -> Inner.list t p)
  let mem t p = run (fun () -> Inner.mem t p)
  let mem_tree t p = run (fun () -> Inner.mem_tree t p)
  let find_all t p = run (fun () -> Inner.find_all t p)
  let find t p = run (fun () -> Inner.find t p)
  let get_all t p = run (fun () -> Inner.get_all t p)
  let get t p = run (fun () -> Inner.get t p)
  let find_tree t p = run (fun () -> Inner.find_tree t p)
  let get_tree t p = run (fun () -> Inner.get_tree t p)
  let key t p = run (fun () -> Inner.key t p)
  let hash t p = run (fun () -> Inner.hash t p)

  (* === Top-level writes === *)
  type write_error = Inner.write_error

  let write_error_t = Inner.write_error_t

  let set ?clear ?retries ?allow_empty ?parents ~info t p c =
    run (fun () -> Inner.set ?clear ?retries ?allow_empty ?parents ~info t p c)

  let set_exn ?clear ?retries ?allow_empty ?parents ~info t p c =
    run (fun () ->
        Inner.set_exn ?clear ?retries ?allow_empty ?parents ~info t p c)

  let set_tree ?clear ?retries ?allow_empty ?parents ~info t p tr =
    run (fun () ->
        Inner.set_tree ?clear ?retries ?allow_empty ?parents ~info t p tr)

  let set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p tr =
    run (fun () ->
        Inner.set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p tr)

  let remove ?clear ?retries ?allow_empty ?parents ~info t p =
    run (fun () -> Inner.remove ?clear ?retries ?allow_empty ?parents ~info t p)

  let remove_exn ?clear ?retries ?allow_empty ?parents ~info t p =
    run (fun () ->
        Inner.remove_exn ?clear ?retries ?allow_empty ?parents ~info t p)

  let test_and_set ?clear ?retries ?allow_empty ?parents ~info t p ~test ~set =
    run (fun () ->
        Inner.test_and_set ?clear ?retries ?allow_empty ?parents ~info t p ~test
          ~set)

  let test_and_set_exn ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run (fun () ->
        Inner.test_and_set_exn ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_set_and_get ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run (fun () ->
        Inner.test_set_and_get ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_set_and_get_exn ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run (fun () ->
        Inner.test_set_and_get_exn ?clear ?retries ?allow_empty ?parents ~info t
          p ~test ~set)

  let test_and_set_tree ?clear ?retries ?allow_empty ?parents ~info t p ~test
      ~set =
    run (fun () ->
        Inner.test_and_set_tree ?clear ?retries ?allow_empty ?parents ~info t p
          ~test ~set)

  let test_and_set_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run (fun () ->
        Inner.test_and_set_tree_exn ?clear ?retries ?allow_empty ?parents ~info
          t p ~test ~set)

  let test_set_and_get_tree ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run (fun () ->
        Inner.test_set_and_get_tree ?clear ?retries ?allow_empty ?parents ~info
          t p ~test ~set)

  let test_set_and_get_tree_exn ?clear ?retries ?allow_empty ?parents ~info t p
      ~test ~set =
    run (fun () ->
        Inner.test_set_and_get_tree_exn ?clear ?retries ?allow_empty ?parents
          ~info t p ~test ~set)

  let merge ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run (fun () ->
        Inner.merge ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run (fun () ->
        Inner.merge_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_tree ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run (fun () ->
        Inner.merge_tree ?clear ?retries ?allow_empty ?parents ~info ~old t p v)

  let merge_tree_exn ?clear ?retries ?allow_empty ?parents ~info ~old t p v =
    run (fun () ->
        Inner.merge_tree_exn ?clear ?retries ?allow_empty ?parents ~info ~old t
          p v)

  let with_tree ?clear ?retries ?allow_empty ?parents ?strategy ~info t p f =
    run (fun () ->
        Inner.with_tree ?clear ?retries ?allow_empty ?parents ?strategy ~info t
          p (fun tr -> Lwt_eio.Promise.await_lwt (f tr)))

  let with_tree_exn ?clear ?retries ?allow_empty ?parents ?strategy ~info t p f
      =
    run (fun () ->
        Inner.with_tree_exn ?clear ?retries ?allow_empty ?parents ?strategy
          ~info t p (fun tr -> Lwt_eio.Promise.await_lwt (f tr)))

  let last_modified ?depth ?n t p =
    run (fun () -> Inner.last_modified ?depth ?n t p)

  (* === Tree === *)
  module Tree = struct
    (* Type rebindings *)
    type t = tree
    type kinded_hash = Inner.Tree.kinded_hash
    type kinded_key = Inner.Tree.kinded_key
    type elt = Inner.Tree.elt
    type error = Inner.Tree.error
    type 'a or_error = ('a, error) result
    type marks = Inner.Tree.marks
    type 'a force = [ `True | `False of path -> 'a -> 'a Lwt.t ]
    type uniq = [ `False | `True | `Marks of marks ]
    type ('a, 'b) folder = path -> 'b -> 'a -> 'a Lwt.t

    type depth =
      [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]

    type concrete = Inner.Tree.concrete

    (* [counters] is rebound to the top-level [Tree_intf.counters] hoisted
       out of [Tree.S], so that Maker_v2.Make.Tree.counters has the same
       nominal identity as the [counters] expected by [Tree.S with ...]
       in [Store.S]. *)
    type counters = Tree_intf.counters = {
      mutable contents_hash : int;
      mutable contents_find : int;
      mutable contents_add : int;
      mutable contents_mem : int;
      mutable node_hash : int;
      mutable node_mem : int;
      mutable node_index : int;
      mutable node_add : int;
      mutable node_find : int;
      mutable node_val_v : int;
      mutable node_val_find : int;
      mutable node_val_list : int;
    }

    type stats = Tree_intf.stats = {
      nodes : int;
      leafs : int;
      skips : int;
      depth : int;
      width : int;
    }

    type verifier_error = Inner.Tree.verifier_error

    let t = Inner.Tree.t
    let kinded_hash_t = Inner.Tree.kinded_hash_t
    let kinded_key_t = Inner.Tree.kinded_key_t
    let depth_t = Inner.Tree.depth_t
    let stats_t = Tree_intf.stats_t
    let concrete_t = Inner.Tree.concrete_t
    let verifier_error_t = Inner.Tree.verifier_error_t

    (* Type descriptors for the included path/step/... types *)
    let path_t = Inner.Tree.path_t
    let step_t = Inner.Tree.step_t
    let metadata_t = Inner.Tree.metadata_t
    let contents_t = Inner.Tree.contents_t
    let contents_key_t = Inner.Tree.contents_key_t
    let node_t = Inner.Tree.node_t
    let hash_t = Inner.Tree.hash_t

    (* Exceptions thrown by Tree (re-bound from Inner) *)
    exception Dangling_hash = Inner.Tree.Dangling_hash
    exception Pruned_hash = Inner.Tree.Pruned_hash
    exception Portable_value = Inner.Tree.Portable_value

    (* Constructors (sync, no wrapping needed) *)
    let empty = Inner.Tree.empty
    let singleton = Inner.Tree.singleton
    let of_contents = Inner.Tree.of_contents
    let of_node = Inner.Tree.of_node
    let v = Inner.Tree.v
    let pruned = Inner.Tree.pruned

    (* Reads *)
    let kind tr p = run (fun () -> Inner.Tree.kind tr p)
    let is_empty = Inner.Tree.is_empty
    let diff a b = run (fun () -> Inner.Tree.diff a b)
    let mem tr p = run (fun () -> Inner.Tree.mem tr p)
    let find_all tr p = run (fun () -> Inner.Tree.find_all tr p)
    let length tr ?cache p = run (fun () -> Inner.Tree.length tr ?cache p)
    let find tr p = run (fun () -> Inner.Tree.find tr p)
    let get_all tr p = run (fun () -> Inner.Tree.get_all tr p)

    let list tr ?offset ?length ?cache p =
      run (fun () -> Inner.Tree.list tr ?offset ?length ?cache p)

    let seq tr ?offset ?length ?cache p =
      run (fun () -> Inner.Tree.seq tr ?offset ?length ?cache p)

    let get tr p = run (fun () -> Inner.Tree.get tr p)
    let mem_tree tr p = run (fun () -> Inner.Tree.mem_tree tr p)
    let find_tree tr p = run (fun () -> Inner.Tree.find_tree tr p)
    let get_tree tr p = run (fun () -> Inner.Tree.get_tree tr p)

    (* Writes *)
    let add tr p ?metadata c = run (fun () -> Inner.Tree.add tr p ?metadata c)

    let update tr p ?metadata f =
      run (fun () -> Inner.Tree.update tr p ?metadata f)

    let remove tr p = run (fun () -> Inner.Tree.remove tr p)
    let add_tree tr p sub = run (fun () -> Inner.Tree.add_tree tr p sub)
    let update_tree tr p f = run (fun () -> Inner.Tree.update_tree tr p f)
    let merge = Lwt_to_eio.merge_of_eio Inner.Tree.t Inner.Tree.merge
    let destruct = Inner.Tree.destruct
    let pp = Inner.Tree.pp

    (* Identity / hashing *)
    let key = Inner.Tree.key
    let find_key r tr = run (fun () -> Inner.Tree.find_key r tr)
    let of_key r k = run (fun () -> Inner.Tree.of_key r k)
    let shallow = Inner.Tree.shallow
    let hash = Inner.Tree.hash
    let kinded_hash = Inner.Tree.kinded_hash
    let of_hash r kh = run (fun () -> Inner.Tree.of_hash r kh)

    (* Folds *)
    let empty_marks = Inner.Tree.empty_marks

    let fold (type a) ?order ?(force : a force option) ?cache ?uniq
        ?(pre : (a, step list) folder option)
        ?(post : (a, step list) folder option) ?depth
        ?(contents : (a, contents) folder option)
        ?(node : (a, node) folder option) ?(tree : (a, t) folder option) tr
        (acc : a) =
      let force_eio =
        match force with
        | None -> None
        | Some `True -> Some `True
        | Some (`False f) ->
            Some (`False (fun p a -> Lwt_eio.Promise.await_lwt (f p a)))
      in
      let bridge f =
        Option.map (fun f p b a -> Lwt_eio.Promise.await_lwt (f p b a)) f
      in
      run (fun () ->
          Inner.Tree.fold ?order ?force:force_eio ?cache ?uniq ?pre:(bridge pre)
            ?post:(bridge post) ?depth ?contents:(bridge contents)
            ?node:(bridge node) ?tree:(bridge tree) tr acc)

    (* Stats / concrete *)
    let stats ?force tr =
      run (fun () ->
          let s = Inner.Tree.stats ?force tr in
          {
            nodes = s.Inner.Tree.nodes;
            leafs = s.leafs;
            skips = s.skips;
            depth = s.depth;
            width = s.width;
          })

    let of_concrete = Inner.Tree.of_concrete
    let to_concrete tr = run (fun () -> Inner.Tree.to_concrete tr)

    (* Caches / counters *)
    let clear = Inner.Tree.clear

    let counters () =
      let c = Inner.Tree.counters () in
      {
        contents_hash = c.Inner.Tree.contents_hash;
        contents_find = c.contents_find;
        contents_add = c.contents_add;
        contents_mem = c.contents_mem;
        node_hash = c.node_hash;
        node_mem = c.node_mem;
        node_index = c.node_index;
        node_add = c.node_add;
        node_find = c.node_find;
        node_val_v = c.node_val_v;
        node_val_find = c.node_val_find;
        node_val_list = c.node_val_list;
      }

    let dump_counters = Inner.Tree.dump_counters
    let reset_counters = Inner.Tree.reset_counters
    let inspect = Inner.Tree.inspect

    (* Proof producer / verifier *)
    let produce_proof r kk f =
      run (fun () ->
          Inner.Tree.produce_proof r kk (fun tr ->
              Lwt_eio.Promise.await_lwt (f tr)))

    let verify_proof p f =
      run (fun () ->
          Inner.Tree.verify_proof p (fun tr -> Lwt_eio.Promise.await_lwt (f tr)))

    let hash_of_proof_state = Inner.Tree.hash_of_proof_state

    (* Sub-modules: Contents (lazy), Proof, Private *)
    module Contents = struct
      type t = Inner.Tree.Contents.t

      let hash = Inner.Tree.Contents.hash
      let key = Inner.Tree.Contents.key
      let force tc = run (fun () -> Inner.Tree.Contents.force tc)
      let force_exn tc = run (fun () -> Inner.Tree.Contents.force_exn tc)
      let clear = Inner.Tree.Contents.clear
    end

    module Proof = Inner.Tree.Proof
    module Private = Inner.Tree.Private
  end

  (* Now that our Remote.t is aliased to Irmin.remote, Inner.E is the same
     extensible variant constructor and can be re-exported directly. *)
  type Remote.t += E = Inner.E

  (* === Backend converters === *)
  let of_backend_node = Inner.of_backend_node
  let to_backend_node n = run (fun () -> Inner.to_backend_node n)

  let to_backend_portable_node n =
    run (fun () -> Inner.to_backend_portable_node n)

  let to_backend_commit = Inner.to_backend_commit
  let of_backend_commit = Inner.of_backend_commit
  let save_contents t c = run (fun () -> Inner.save_contents t c)

  let save_tree ?clear r ct nt tr =
    run (fun () -> Inner.save_tree ?clear r ct nt tr)

  (* === Deprecated alias === *)
  let master r = run (fun () -> Inner.master r) [@@warning "-3"]
end
