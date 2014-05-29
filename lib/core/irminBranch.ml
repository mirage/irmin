(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std
open Lwt
open IrminMerge.OP

type origin = IrminOrigin.t
type path = IrminPath.t

module Log = Log.Make(struct let section = "BRANCH" end)

module type STORE = sig
  include IrminStore.RW with type key = path
  type branch
  val create: ?branch:branch -> unit -> t Lwt.t
  val branch: t -> branch
  val with_branch: t -> branch -> t
  val update: t -> ?origin:origin -> key -> value -> unit Lwt.t
  val remove: t -> ?origin:origin -> key -> unit Lwt.t
  val clone: t -> branch -> t option Lwt.t
  val clone_force: t -> branch -> t Lwt.t
  val switch: t -> branch -> unit Lwt.t
  val merge: t -> ?origin:origin -> branch -> unit IrminMerge.result Lwt.t
  val merge_exn: t -> ?origin:origin -> branch -> unit Lwt.t
  module Block: IrminBlock.STORE with type contents = value
  module Tag: IrminTag.STORE with type key = branch and type value = Block.key
  val block_t: t -> Block.t
  val contents_t: t -> Block.Contents.t
  val node_t: t -> Block.Node.t
  val commit_t: t -> Block.Commit.t
  val tag_t: t -> Tag.t
  module Key: IrminKey.S with type t = key
  module Value: IrminContents.S with type t = value
  module Branch: IrminTag.S with type t = branch
  module Graph: IrminGraph.S with type V.t = (Block.key, Tag.key) IrminGraph.vertex
end

module type INTERNAL = sig
  include STORE
  val read_node: t -> key -> Block.node option Lwt.t
  val update_node: t -> origin -> key -> Block.node -> unit Lwt.t
  val watch_node: t -> key -> (key * Block.key) Lwt_stream.t
  val update_commit: t -> Block.key -> unit Lwt.t
  val merge_commit: t -> ?origin:origin -> Block.key -> unit IrminMerge.result Lwt.t
end

module Make
    (Block: IrminBlock.STORE)
    (Tag  : IrminTag.STORE with type value = Block.key) =
struct

  module Block = Block

  module Tag = Tag
  module T = Tag.Key
  module Branch = T

  module Key = IrminPath
  module K = Block.Key

  module Value = Block.Contents.Value
  module Contents = Block.Contents
  module C = Value

  module Node = Block.Node
  module N = Node.Value

  module Commit = Block.Commit

  type key = IrminPath.t

  type value = C.t

  type branch = T.t

  module Graph = IrminGraph.Make(K)(T)

  type t = {
    block: Block.t;
    tag: Tag.t;
    branch: T.t;
  }

  let branch t = t.branch
  let with_branch t branch = { t with branch }

  let block_t    t = t.block
  let tag_t      t = t.tag
  let commit_t   t = Block.commit_t t.block
  let node_t     t = Block.node_t t.block
  let contents_t t = Block.contents_t t.block

  let create ?(branch=T.master) () =
    Block.create () >>= fun block ->
    Tag.create ()   >>= fun tag ->
    return { block; tag; branch }

  let read_head_commit t =
    Tag.read t.tag t.branch >>= function
    | None   -> return_none
    | Some k -> Commit.read (commit_t t) k

  let node_of_commit t c =
    match Commit.node (commit_t t) c with
    | None   -> return IrminNode.empty
    | Some n -> n

  let node_of_opt_commit t = function
    | None   -> return IrminNode.empty
    | Some c -> node_of_commit t c

  let read_head_node t =
    read_head_commit t >>=
    node_of_opt_commit t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let read_node t path =
    read_head_commit t          >>= fun commit ->
    node_of_opt_commit t commit >>= fun node ->
    Node.sub (node_t t) node path

  let apply t origin ~f =
    read_head_commit t          >>= fun commit ->
    node_of_opt_commit t commit >>= fun old_node ->
    f old_node                  >>= fun node ->
    if N.equal node old_node then return_unit
    else (
      let parents = parents_of_commit commit in
      Commit.commit (commit_t t) origin ~node ~parents >>= fun (key, _) ->
      (* XXX: the head might have changed since we started the operation *)
      Tag.update t.tag t.branch key
    )

  let update_node t origin path node =
    apply t origin ~f:(fun head ->
        Node.map (node_t t) head path (fun _ -> node)
      )

  let map t path ~f =
    read_head_node t >>= fun node ->
    f (node_t t) node path

  let read t path =
    map t path ~f:Node.find

  let update t ?origin path contents =
    let origin = match origin with
      | None   -> IrminOrigin.create "Update %s." (IrminPath.to_string path)
      | Some o -> o in
    Log.debugf "update %s" (IrminPath.to_string path);
    apply t origin ~f:(fun node ->
        Node.update (node_t t) node path contents
      )

  let remove t ?origin path =
    let origin = match origin with
      | None   -> IrminOrigin.create "Remove %s." (IrminPath.to_string path)
      | Some o -> o in
    apply t origin ~f:(fun node ->
        Node.remove (node_t t) node path
      )

  let read_exn t path =
    map t path ~f:Node.find_exn

  let mem t path =
    map t path ~f:Node.valid

  (* Return the subpaths. *)
  let list t paths =
    Log.debugf "list";
    let one path =
      read_head_node t >>= fun n ->
      Node.sub (node_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = Node.succ (node_t t) node in
        let c = Map.keys c in
        let paths = List.map ~f:(fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = IrminPath.Set.of_list paths in
        return (IrminPath.Set.union set paths)
      ) IrminPath.Set.empty paths
    >>= fun paths ->
    return (IrminPath.Set.to_list paths)

  let dump t =
    Log.debugf "dump";
    read_head_node t >>= fun node ->
    let rec aux seen = function
      | []       -> return (List.sort compare seen)
      | path::tl ->
        list t [path] >>= fun childs ->
        let todo = childs @ tl in
        Node.find (node_t t) node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin Node.find (node_t t) node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [[]] >>= aux init

  (* Merge two commits:
     - Search for a common ancestor
     - Perform a 3-way merge *)
  let three_way_merge t ?origin c1 c2 =
    Log.debugf "3-way merge between %s and %s" (K.to_string c1) (K.to_string c2);
    Commit.find_common_ancestor (commit_t t) c1 c2 >>= function
    | None     -> conflict "no common ancestor"
    | Some old ->
      let origin = match origin with
        | None   -> IrminOrigin.create "Merge commits %s and %s.\n\n\
                                        The common ancestor was %s."
                      (K.to_string c1) (K.to_string c2) (K.to_string old)
        | Some o -> o in
      let m = Commit.merge (commit_t t) in
      IrminMerge.merge m ~origin ~old c1 c2

  let update_commit t c =
    Tag.update t.tag t.branch c

  let switch t branch =
    Log.debugf "switch %s" (T.to_string branch);
    Tag.read t.tag branch >>= function
    | Some c -> Tag.update t.tag t.branch c
    | None   -> Tag.remove t.tag t.branch

  let merge_commit t ?origin c1 =
    Tag.read t.tag t.branch >>= function
    | None    -> Tag.update t.tag t.branch c1 >>= ok
    | Some c2 ->
      three_way_merge t ?origin c1 c2 >>| fun c3 ->
      Tag.update t.tag t.branch c3   >>=
      ok

  let clone_force t branch =
    Log.debugf "clone %s" (T.to_string branch);
    begin Tag.read t.tag t.branch >>= function
      | None   -> Tag.remove t.tag branch
      | Some c -> Tag.update t.tag branch c
    end >>= fun () ->
    return { t with branch }

  let clone t branch =
    Tag.mem t.tag branch >>= function
    | true  -> return_none
    | false -> clone_force t branch >>= fun t -> return (Some t)

  let merge t ?origin branch =
    Log.debugf "merge %s" (Branch.to_string branch);
    let origin = match origin with
      | Some o -> o
      | None   -> IrminOrigin.create "Merge branch %s."
                    (T.to_string t.branch) in
    Tag.read_exn t.tag branch >>= fun c ->
    merge_commit t ~origin c

  let merge_exn t ?origin tag =
    merge t ?origin tag >>=
    IrminMerge.exn

  let watch_node t path =
    Log.infof "Adding a watch on %s" (IrminPath.to_string path);
    let stream = Tag.watch t.tag t.branch in
    IrminMisc.lift_stream (
      read_node t path >>= fun node ->
      let old_node = ref node in
      let stream = Lwt_stream.filter_map_s (fun key ->
          Log.debugf "watch: %s" (Block.Key.to_string key);
          Commit.read_exn (commit_t t) key >>= fun commit ->
          begin match Commit.node (commit_t t) commit with
            | None      -> return IrminNode.empty
            | Some node -> node
          end >>= fun node ->
          Node.sub (node_t t) node path >>= fun node ->
          if node = !old_node then return_none
          else (
            old_node := node;
            return (Some (path, key))
          )
        ) stream in
      return stream
    )

  (* watch contents changes. *)
  let watch t path =
    let stream = watch_node t path in
    Lwt_stream.filter_map_s (fun (p, k) ->
        if IrminPath.(p = path) then
          Commit.read (commit_t t) k >>= function
          | None   -> return_none
          | Some c ->
            node_of_commit t c >>= fun n ->
            Node.find (node_t t) n p
        else
          return_none
      ) stream

end

module type MAKER =
  functor (K: IrminKey.S) ->
  functor (C: IrminContents.S) ->
  functor (T: IrminTag.S) ->
    STORE with type Block.key = K.t
           and type value = C.t
           and type branch = T.t
