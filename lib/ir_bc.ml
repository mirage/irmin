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

open Lwt
open Ir_merge.OP
open Ir_misc.OP

module Log = Log.Make(struct let section = "BRANCH" end)
module StringMap = Map.Make(String)
module PathSet = Misc.Set(Ir_path)

module type S = sig
  include Ir_rw.S
  type tag
  val create: ?tag:tag -> unit -> t Lwt.t
  val detach: t -> unit Lwt.t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val retag: t -> tag -> unit
  val clone: t -> tag -> t option Lwt.t
  val clone_force: t -> tag -> t Lwt.t
  val switch: t -> tag -> unit Lwt.t
  val merge: t -> ?origin:origin -> tag -> unit Ir_merge.result Lwt.t
  val merge_exn: t -> ?origin:origin -> tag -> unit Lwt.t
end

module type STORE = sig
  include S
  type uid
  module Block: Ir_block.STORE with type key = uid and type contents = value
  module Tag: Ir_tag.STORE with type key = tag and type value = uid
  val block_t: t -> Block.t
  val commit_t: t -> Block.Commit.t
  val node_t: t -> Block.Node.t
  val contents_t: t -> Block.Contents.t
  val tag_t: t -> Tag.t
  val create_head: Block.Commit.key -> t Lwt.t
  val head: t -> Block.Commit.key option Lwt.t
  val head_exn: t -> Block.Commit.key Lwt.t
  val set_head: t -> Block.Commit.key -> unit
  val update_head: t -> Block.Commit.key -> unit Lwt.t
  val merge_head: t -> ?origin:origin -> Block.Commit.key -> unit Ir_merge.result Lwt.t
  val read_node: t -> key -> Block.Node.value option Lwt.t
  val update_node: t -> origin -> key -> Block.Node.value -> unit Lwt.t
  val watch_node: t -> key -> (key * Block.Node.key) Lwt_stream.t
  module Graph: Ir_graph.S with type V.t =
    [ `Contents of Block.Contents.key
    | `Node of Block.Node.key
    | `Commit of Block.Commit.key
    | `Tag of Tag.key ]
end

module type MAKER =
  functor (U: Ir_uid.S) -> functor (C: Ir_contents.S) -> functor (T: Ir_tag.S) ->
    STORE with type key = Ir_path.t
           and type value = C.t
           and type origin = Ir_origin.t
           and type tag = T.t
           and type commit = U.t
           and type node = U.t Ir_node.t
(** Signature of functors to create branch-consistent stores. *)

type ('a, 'b) t =
  [ `Tag of 'a
  | `Key of 'b ]

module Make (Block: Ir_block.STORE) (Tag: Ir_tag.STORE with type value = Block.key) =
struct

  module Block = Block

  type origin = Ir_origin.t

  module Tag = Tag
  module T = Tag.Key

  module Key = Ir_path
  module K = Block.Key

  module Value = Block.Contents.Value
  module XContents = Block.Contents
  module C = Value

  module XNode = Block.Node
  module N = XNode.Value

  module XCommit = Block.Commit

  type key = Path.t

  type value = C.t

  module Branch = T

  type branch = Branch.t

  module TK = struct
    include Tc.App2(TK2)(T)(K)
    let pretty = function
      | `Tag t -> T.pretty t
      | `Key k -> K.pretty k
  end

  module Graph = Digraph.Make(K)(T)

  type t = {
    block : Block.t;
    tag   : Tag.t;
    mutable branch: TK.t;
  }

  let branch t = match t.branch with
    | `Tag t -> Some t
    | `Key _ -> None

  let branch_exn t = match t.branch with
    | `Tag t -> t
    | `Key _ -> raise Not_found

  let set_branch t branch =
    t.branch <- `Tag branch

  let head t = match t.branch with
    | `Key key -> return (Some key)
    | `Tag tag -> Tag.read t.tag tag

  let head_exn t =
    head t >>= function
    | None   -> fail Not_found
    | Some k -> return k

  let set_head t key =
    t.branch <- `Key key

  let detach t =
    match t.branch with
    | `Key _   -> return_unit
    | `Tag tag ->
      Tag.read_exn t.tag tag >>= fun key ->
      t.branch <- `Key key;
      return_unit

  let block_t    t = t.block
  let tag_t      t = t.tag
  let commit_t   t = Block.commit_t t.block
  let node_t     t = Block.node_t t.block
  let contents_t t = Block.contents_t t.block

  let create ?(branch=Branch.master) () =
    Block.create () >>= fun block ->
    Tag.create ()   >>= fun tag ->
    let branch = `Tag branch in
    return { block; tag; branch }

  let create_head key =
    create () >>= fun t ->
    set_head t key;
    return t

  let read_head_commit t =
    match t.branch with
    | `Key key ->
      Log.debugf "read detached head: %a" force (show (module K) key);
      XCommit.read (commit_t t) key
    | `Tag tag ->
      Log.debugf "read head: %a" force (show (module T) tag);
      Tag.read t.tag tag >>= function
      | None   -> return_none
      | Some k -> XCommit.read (commit_t t) k


  let node_of_commit t c =
    match XCommit.node (commit_t t) c with
    | None   -> return Node.empty
    | Some n -> n

  let node_of_opt_commit t = function
    | None   -> return Node.empty
    | Some c -> node_of_commit t c

  let read_head_node t =
    Log.debug (lazy "read_head_node");
    read_head_commit t >>=
    node_of_opt_commit t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let read_node t path =
    read_head_commit t          >>= fun commit ->
    node_of_opt_commit t commit >>= fun node ->
    XNode.sub (node_t t) node path

  let apply t origin ~f =
    read_head_commit t          >>= fun commit ->
    node_of_opt_commit t commit >>= fun old_node ->
    f old_node                  >>= fun node ->
    if N.equal node old_node then return_unit
    else (
      let parents = parents_of_commit commit in
      XCommit.commit (commit_t t) origin ~node ~parents >>= fun (key, _) ->
      (* XXX: the head might have changed since we started the operation *)
      match t.branch with
      | `Tag tag -> Tag.update t.tag tag key
      | `Key _   -> t.branch <- `Key key; return_unit
    )

  let update_node t origin path node =
    apply t origin ~f:(fun head ->
        XNode.map (node_t t) head path (fun _ -> node)
      )

  let map t path ~f =
    read_head_node t >>= fun node ->
    f (node_t t) node path

  let read t path =
    map t path ~f:XNode.find

  let update t ?origin path contents =
    let origin = match origin with
      | None   -> Origin.create "Update %s." (Path.pretty path)
      | Some o -> o in
    Log.debugf "update %a" force (show (module Path) path);
    apply t origin ~f:(fun node ->
        XNode.update (node_t t) node path contents
      )

  let remove t ?origin path =
    let origin = match origin with
      | None   -> Origin.create "Remove %s." (Path.pretty path)
      | Some o -> o in
    apply t origin ~f:(fun node ->
        XNode.remove (node_t t) node path
      )

  let read_exn t path =
    Log.debugf "read_exn %a" force (show (module Path) path);
    map t path ~f:XNode.find_exn

  let mem t path =
    map t path ~f:XNode.valid

  (* Return the subpaths. *)
  let list t paths =
    Log.debugf "list";
    let one path =
      read_head_node t >>= fun n ->
      XNode.sub (node_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = XNode.succ (node_t t) node in
        let c = Misc.StringMap.keys c in
        let paths = List.map (fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = PathSet.of_list paths in
        return (PathSet.union set paths)
      ) PathSet.empty paths
    >>= fun paths ->
    return (PathSet.to_list paths)

  let dump t =
    Log.debugf "dump";
    read_head_node t >>= fun node ->
    let rec aux seen = function
      | []       -> return (List.sort Pervasives.compare seen)
      | path::tl ->
        list t [path] >>= fun childs ->
        let todo = childs @ tl in
        XNode.find (node_t t) node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin XNode.find (node_t t) node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [[]] >>= aux init

  (* Merge two commits:
     - Search for a common ancestor
     - Perform a 3-way merge *)
  let three_way_merge t ?origin c1 c2 =
    Log.debugf "3-way merge between %a and %a"
      force (show (module K) c1)
      force (show (module K) c2);
    XCommit.find_common_ancestor (commit_t t) c1 c2 >>= function
    | None     -> conflict "no common ancestor"
    | Some old ->
      let origin = match origin with
        | None   -> Origin.create "Merge commits %s and %s.\n\n\
                                   The common ancestor was %s."
                      (K.pretty c1) (K.pretty c2) (K.pretty old)
        | Some o -> o in
      let m = XCommit.merge (commit_t t) in
      Merge.merge m ~origin ~old c1 c2

  let update_commit t c =
    match t.branch with
    | `Tag tag -> Tag.update t.tag tag c
    | `Key _   -> t.branch <- `Key c; return_unit

  let switch t branch =
    Log.debugf "switch %a" force (show (module Branch) branch);
    Tag.read t.tag branch >>= function
    | Some c -> update_commit t c
    | None   -> fail Not_found

  let merge_commit t ?origin c1 =
    let aux c2 =
      three_way_merge t ?origin c1 c2 >>| fun c3 ->
      update_commit t c3 >>= ok
    in
    match t.branch with
    | `Key c2  -> aux c2
    | `Tag tag ->
      Tag.read t.tag tag >>= function
      | None    -> update_commit t c1 >>= ok
      | Some c2 -> aux c2

  let clone_force t branch =
    Log.debugf "clone %a" force (show (module Branch) branch);
    begin match t.branch with
      | `Key c -> Tag.update t.tag branch c
      | `Tag tag ->
        Tag.read t.tag tag >>= function
        | None   -> fail Not_found
        | Some c -> Tag.update t.tag branch c
    end  >>= fun () ->
    return { t with branch = `Tag branch }

  let clone t branch =
    Tag.mem t.tag branch >>= function
    | true  -> return_none
    | false -> clone_force t branch >>= fun t -> return (Some t)

  let merge t ?origin branch =
    Log.debugf "merge %a" force (show (module Branch) branch);
    let origin = match origin with
      | Some o -> o
      | None   -> Origin.create "Merge branch %s." (TK.pretty t.branch) in
    Tag.read_exn t.tag branch >>= fun c ->
    merge_commit t ~origin c

  let merge_exn t ?origin tag =
    merge t ?origin tag >>=
    Merge.exn

  let watch_node t path =
    Log.infof "Adding a watch on %a" force (show (module Path) path);
    match t.branch with
    | `Key _   -> Lwt_stream.of_list []
    | `Tag tag ->
      let stream = Tag.watch t.tag tag in
      Misc.Lwt_stream.lift (
        read_node t path >>= fun node ->
        let old_node = ref node in
        let stream = Lwt_stream.filter_map_s (fun key ->
            Log.debugf "watch: %a" force (show (module Block.Key) key);
            XCommit.read_exn (commit_t t) key >>= fun commit ->
            begin match XCommit.node (commit_t t) commit with
              | None      -> return Node.empty
              | Some node -> node
            end >>= fun node ->
            XNode.sub (node_t t) node path >>= fun node ->
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
        if Path.(p = path) then
          XCommit.read (commit_t t) k >>= function
          | None   -> return_none
          | Some c ->
            node_of_commit t c >>= fun n ->
            XNode.find (node_t t) n p
        else
          return_none
      ) stream

end
