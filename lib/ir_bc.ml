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

module type STORE = sig
  include Ir_rw.STORE
  type tag
  val of_tag: Ir_config.t -> Ir_task.t -> tag -> t Lwt.t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val update_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  val update_tag_force: t -> tag -> unit Lwt.t
  val switch: t -> tag -> unit Lwt.t
  type head
  val of_head: Ir_config.t -> Ir_task.t -> head -> t Lwt.t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val heads: t -> head list Lwt.t
  val detach: t -> unit Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val merge_head: t -> head -> unit Ir_merge.result Lwt.t
  val watch_head: t -> key -> (key * head) Lwt_stream.t
  val clone: t -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  val clone_force: t -> tag -> t Lwt.t
  val merge: t -> tag -> unit Ir_merge.result Lwt.t
  type slice
  module Slice: Tc.S0 with type t = slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  val import_force: t -> slice -> unit Lwt.t
end

module type STORE_EXT = sig
  type step
  include STORE with type key = step list
  module Block: Ir_block.STORE
    with type step = step
     and type contents = value
     and type head = head
  val contents_t: t -> Block.Contents.t
  val node_t: t -> Block.Node.t
  val commit_t: t -> Block.Commit.t
  module Tag: Ir_tag.STORE
    with type key = tag
     and type value = head
  val tag_t: t -> Tag.t
  module Key: Ir_path.S with type step = Block.step
  module Val: Ir_contents.S with type t = value
  val read_node: t -> key -> Block.Node.value option Lwt.t
  val mem_node: t -> key -> bool Lwt.t
  val update_node: t -> key -> Block.Node.value -> unit Lwt.t
  val slice_contents: slice -> (Block.Contents.key * Block.contents) list
  val slice_nodes: slice -> (Block.Node.key * Block.node) list
  val slice_commits: slice -> (Block.Commit.key * Block.commit) list
  val slice_tags: slice -> (Tag.key * Tag.value) list
  module Graph: Ir_graph.S with type V.t =
    [ `Contents of Block.Contents.key
    | `Node of Block.Node.key
    | `Commit of Block.Commit.key
    | `Tag of Tag.key ]
end

module type PRIVATE = sig
  module Contents: Ir_contents.STORE
  module Node: Ir_node.STORE with type Val.contents = Contents.key
  module Commit: Ir_commit.STORE with type Val.node = Node.key
  module Tag: Ir_tag.STORE with type value = Commit.key
end

module Make_ext (P: PRIVATE) = struct

  module C = P.Contents
  module N = P.Node
  module H = P.Commit
  module T = P.Tag

  module B = Ir_block.Make(C)(N)(H)
  module Block = B

  module Tag = T
  type tag = T.key

  module Key = Block.Path
  module KeySet = Ir_misc.Set(Key)
  type key = Key.t

  module Val = Block.Contents.Val
  type value = Val.t

  type step = B.step

  module Head = B.Commit.Key
  type head = Head.t

  type branch = [ `Tag of tag | `Key of B.Commit.key ]

  module Graph = Ir_graph.Make(B.Contents.Key)(B.Node.Key)(B.Commit.Key)(T.Key)

  type t = {
    block: B.Commit.t;
    tag: T.t;
    task: Ir_task.t;
    mutable branch: branch;
  }

  let tag_t t = t.tag
  let commit_t t = t.block
  let node_t t = B.Commit.node_t (commit_t t)
  let contents_t t = B.Node.contents_t (node_t t)

  let tag t = match t.branch with
    | `Tag t -> Some t
    | `Key _ -> None

  let tag_exn t = match t.branch with
    | `Tag t -> t
    | `Key _ -> raise Not_found

  let set_tag t tag =
    t.branch <- `Tag tag

  let head t = match t.branch with
    | `Key key -> return (Some key)
    | `Tag tag -> T.read (tag_t t) tag

  let heads t =
    T.dump (tag_t t) >>= fun tags ->
    let heads = List.map snd tags in
    head t >>= function
    | None   -> return heads
    | Some h -> return (h :: List.filter ((<>) h) heads)

  let head_exn t =
    head t >>= function
    | None   -> fail Not_found
    | Some k -> return k

  let detach t =
    match t.branch with
    | `Key _   -> return_unit
    | `Tag tag ->
      T.read_exn (tag_t t) tag >>= fun key ->
      t.branch <- `Key key;
      return_unit

  let of_tag config task t =
    B.Commit.create config task >>= fun block ->
    T.create config task >>= fun tag ->
    return { block; tag; task; branch = `Tag t }

  let task t = B.Commit.task t.block
  let config t = B.Commit.config t.block

  let create config task =
    of_tag config task T.Key.master

  let of_head config task key =
    B.Commit.create config task >>= fun block ->
    T.create config task >>= fun tag ->
    return { block; tag; task; branch = `Key key }

  let read_head_commit t =
    match t.branch with
    | `Key key ->
      Log.debugf "read detached head: %a" force (show (module Head) key);
      B.Commit.read (commit_t t) key
    | `Tag tag ->
      Log.debugf "read head: %a" force (show (module T.Key) tag);
      T.read (tag_t t) tag >>= function
      | None   -> return_none
      | Some k -> B.Commit.read (commit_t t) k

  let node_of_commit t c =
    match B.Commit.node (commit_t t) c with
    | None   -> return B.Node.empty
    | Some n -> n

  let node_of_opt_commit t = function
    | None   -> return B.Node.empty
    | Some c -> node_of_commit t c

  let read_head_node t =
    Log.debug (lazy "read_head_node");
    read_head_commit t >>=
    node_of_opt_commit t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let read_node t path =
    read_head_commit t >>= fun commit ->
    node_of_opt_commit t commit >>= fun node ->
    B.Node.sub (node_t t) node path

  let mem_node t path =
    read_node t path >>= function
    | None   -> return false
    | Some _ -> return true

  let apply t ~f =
    read_head_commit t >>= fun commit ->
    node_of_opt_commit t commit >>= fun old_node ->
    f old_node >>= fun node ->
    if B.Node.Val.equal node old_node then return_unit
    else (
      let parents = parents_of_commit commit in
      B.Commit.commit (commit_t t) ~node ~parents
      >>= fun (key, _) ->
      (* XXX: the head might have changed since we started the operation *)
      match t.branch with
      | `Key _   -> t.branch <- `Key key; return_unit
      | `Tag tag -> T.update (tag_t t) tag key
    )

 let update_node t path node =
    apply t ~f:(fun head ->
        B.Node.map (node_t t) head path (fun _ -> node)
      )

  let map t path ~f =
    read_head_node t >>= fun node ->
    f (node_t t) node path

  let read t path =
    map t path ~f:B.Node.find

  let update t path contents =
    Log.debugf "update %a" force (show (module Key) path);
    apply t ~f:(fun node ->
        B.Node.update (node_t t) node path contents
      )

  let remove t path =
    apply t ~f:(fun node ->
        B.Node.remove (node_t t) node path
      )

  let read_exn t path =
    Log.debugf "read_exn %a" force (show (module Key) path);
    map t path ~f:B.Node.find_exn

  let mem t path =
    map t path ~f:B.Node.valid

  (* Return the subpaths. *)
  let list t paths =
    Log.debugf "list";
    let one path =
      read_head_node t >>= fun n ->
      B.Node.sub (node_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        let steps = B.Node.Val.steps node in
        let paths = List.map (fun c -> path @ [c]) steps in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = KeySet.of_list paths in
        return (KeySet.union set paths)
      ) KeySet.empty paths
    >>= fun paths ->
    return (KeySet.to_list paths)

  let dump t =
    Log.debugf "dump";
    read_head_node t >>= fun node ->
    begin B.Node.find (node_t t) node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end >>= fun init ->
    let rec aux seen = function
      | []       -> return (List.sort Pervasives.compare seen)
      | path::tl ->
        list t [path] >>= fun childs ->
        let todo = childs @ tl in
        B.Node.find (node_t t) node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo
    in
    list t [[]] >>= aux init

  (* Merge two commits:
     - Search for a common ancestor
     - Perform a 3-way merge *)
  let three_way_merge t c1 c2 =
    Log.debugf "3-way merge between %a and %a"
      force (show (module Head) c1)
      force (show (module Head) c2);
    B.Commit.find_common_ancestor (commit_t t) c1 c2 >>= function
    | None     -> conflict "no common ancestor"
    | Some old -> B.Commit.merge (commit_t t) ~old c1 c2

  let update_head t c =
    match t.branch with
    | `Key _   -> t.branch <- `Key c; return_unit
    | `Tag tag -> Tag.update (tag_t t) tag c

  let update_tag_force t tag =
    begin head t >>= function
      | None   -> return_unit
      | Some k -> T.update (tag_t t) tag k
    end >>= fun () ->
    set_tag t tag;
    return_unit

  let update_tag t tag =
    T.mem (tag_t t) tag >>= function
    | true -> return `Duplicated_tag
    | false -> update_tag_force t tag >>= fun () -> return `Ok

  let switch t branch =
    Log.debugf "switch %a" force (show (module T.Key) branch);
    T.read (tag_t t) branch >>= function
    | Some c -> update_head t c
    | None   -> fail Not_found

  let merge_head t c1 =
    let aux c2 =
      three_way_merge t c1 c2 >>| fun c3 ->
      update_head t c3 >>= ok
    in
    match t.branch with
    | `Key c2  -> aux c2
    | `Tag tag ->
      T.read (tag_t t) tag >>= function
      | None    -> update_head t c1 >>= ok
      | Some c2 -> aux c2

  let clone_force t branch =
    Log.debugf "clone %a" force (show (module T.Key) branch);
    begin match t.branch with
      | `Key c -> T.update (tag_t t) branch c
      | `Tag tag ->
        T.read (tag_t t) tag >>= function
        | None   -> fail Not_found
        | Some c -> Tag.update (tag_t t) branch c
    end  >>= fun () ->
    return { t with branch = `Tag branch }

  let clone t branch =
    T.mem (tag_t t) branch >>= function
    | true  -> return `Duplicated_tag
    | false -> clone_force t branch >>= fun t -> return (`Ok t)

  let merge t branch =
    Log.debugf "merge %a" force (show (module T.Key) branch);
    T.read_exn (tag_t t) branch >>= fun c ->
    merge_head t c

  module ONode = Tc.Option(B.Node.Val)

  let watch_node t path =
    Log.infof "Adding a watch on %a" force (show (module Key) path);
    match t.branch with
    | `Key _   -> Lwt_stream.of_list []
    | `Tag tag ->
      let stream = Tag.watch (tag_t t) tag in
      Ir_watch.lwt_stream_lift (
        read_node t path >>= fun node ->
        let old_node = ref node in
        let stream = Lwt_stream.filter_map_s (function
            | None ->
              if ONode.equal !old_node None then return_none
              else (
                old_node := None;
                return (Some (path, None, None))
              )
            | Some head ->
              Log.debugf "watch: %a" force (show (module Head) head);
              B.Commit.read_exn (commit_t t) head >>= fun commit ->
              begin match B.Commit.node (commit_t t) commit with
                | None      -> return B.Node.empty
                | Some node -> node
              end >>= fun node ->
              B.Node.sub (node_t t) node path >>= fun node ->
              if ONode.equal !old_node node then return_none
              else (
                old_node := node;
                return (Some (path, Some head, node))
              )
          ) stream in
        return stream
      )

  module OContents = Tc.Option(B.Contents.Val)

  let watch_head t path =
    Lwt_stream.filter_map (fun (k, h, _) ->
        match h with
        | None -> None
        | Some h -> Some (k, h)
      ) (watch_node t path)

  (* watch contents changes. *)
  let watch t path =
    let path, file = Ir_misc.list_end path in
    let get_contents n =
      match B.Node.contents (node_t t) n file with
      | None   -> return_none
      | Some c -> c >>= fun c -> return (Some c)
    in
    Ir_watch.lwt_stream_lift (
      begin
        read_node t path >>= function
        | None   -> return_none
        | Some n -> get_contents n
      end >>= fun contents ->
      let old_contents = ref contents in
      let stream = watch_node t path in
      let stream = Lwt_stream.filter_map_s (fun (_, _, n) ->
          match n with
          | None ->
            if OContents.equal !old_contents None then return_none
            else (
              old_contents := None;
              return (Some None)
            )
          | Some n ->
            get_contents n >>= fun contents ->
            if OContents.equal !old_contents contents then return_none
            else (
              old_contents := contents;
              return (Some contents)
            )
        ) stream
      in
      return stream
    )

  module Slice = struct

    type t = {
      contents: (B.Contents.key * B.Contents.value) list;
      nodes   : (B.Node.key * B.Node.value) list;
      commits : (B.Commit.key * B.Commit.value) list;
      tags    : (T.key * T.value) list;
    }

    let create ?(contents=[]) ?(nodes=[]) ?(commits=[]) ?(tags=[]) () =
      { contents; nodes; commits; tags }

    module M (K: Tc.S0)(V: Tc.S0) = Tc.List( Tc.Pair(K)(V) )
    module Ct = M(B.Contents.Key)(B.Contents.Val)
    module No = M(B.Node.Key)(B.Node.Val)
    module Cm = M(B.Commit.Key)(B.Commit.Val)
    module Ta = M(T.Key)(T.Val)
    module T = Tc.Pair( Tc.Pair(Ct)(No) )( Tc.Pair(Cm)(Ta) )

    let explode t = (t.contents, t.nodes), (t.commits, t.tags)
    let implode ((contents, nodes), (commits, tags)) =
      { contents; nodes; commits; tags }

    let compare x y = T.compare (explode x) (explode y)
    let equal x y = T.equal (explode x) (explode y)
    let hash = Hashtbl.hash
    let write t buf = T.write (explode t) buf
    let read b = implode (T.read b)
    let size_of t = T.size_of (explode t)

    let to_sexp t =
      let open Sexplib.Type in
      List [
        List [ Atom "contents"; Ct.to_sexp t.contents ];
        List [ Atom "nodes"   ; No.to_sexp t.nodes ];
        List [ Atom "commmits"; Cm.to_sexp t.commits ];
        List [ Atom "tags"    ; Ta.to_sexp t.tags ];
      ]

    let to_json t =
      `O [
        ("contents", Ct.to_json t.contents);
        ("nodes"   , No.to_json t.nodes);
        ("commits" , Cm.to_json t.commits);
        ("tags"    , Ta.to_json t.tags);
      ]

    let of_json j =
      let contents = Ezjsonm.find j ["contents"] |> Ct.of_json in
      let nodes = Ezjsonm.find j ["nodes"] |> No.of_json in
      let commits = Ezjsonm.find j ["commits"] |> Cm.of_json in
      let tags = Ezjsonm.find j ["tags"] |> Ta.of_json in
      { contents; nodes; commits; tags }

  end

  type slice = Slice.t

  let slice_contents t = t.Slice.contents
  let slice_nodes t = t.Slice.nodes
  let slice_commits t = t.Slice.commits
  let slice_tags t = t.Slice.tags

  let export ?(full=true) ?depth ?(min=[]) ?max t =
    Log.debugf "export depth=%s full=%b min=%d max=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      full (List.length min)
      (match max with None -> "<none>" | Some l -> string_of_int (List.length l));
    begin match max with
      | Some m -> return m
      | None   -> heads t
    end >>= fun max ->
    T.dump (tag_t t) >>= fun tags ->
    let tags = List.filter (fun (_, k) -> List.mem k max) tags in
    let max = List.map (fun x -> `Commit x) max in
    let min = List.map (fun x -> `Commit x) min in
    let pred = function
        | `Commit k ->
          begin
            B.Commit.read (commit_t t) k >>= function
            | None   -> return_nil
            | Some c ->
              return (List.map (fun x -> `Commit x) (B.Commit.Val.parents c))
          end
        | _ -> return_nil in
      Graph.closure ?depth ~pred ~min max >>= fun g ->
      let keys =
        Ir_misc.list_filter_map
          (function `Commit c -> Some c | _ -> None)
          (Graph.vertex g)
      in
      Lwt_list.map_p (fun k ->
          B.Commit.read_exn (commit_t t) k >>= fun c ->
          return (k, c)
        ) keys
      >>= fun commits ->
      if not full then
        return (Slice.create ~commits ~tags ())
      else
        let root_nodes =
          Ir_misc.list_filter_map (fun (_,c) -> B.Commit.Val.node c) commits
        in
        B.Node.list (node_t t) root_nodes >>= fun nodes ->
        Lwt_list.map_p (fun k ->
            B.Node.read (node_t t) k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) nodes >>= fun nodes ->
        let nodes = Ir_misc.list_filter_map (fun x -> x) nodes in
        let root_contents =
          let module KSet = Ir_misc.Set(B.Contents.Key) in
          List.fold_left (fun acc (_, n) ->
              KSet.union
                (KSet.of_list (List.map snd (B.Node.Val.all_contents n)))
                acc
            ) KSet.empty nodes
          |> KSet.to_list
        in
        B.Contents.list (contents_t t) root_contents >>= fun contents ->
        Lwt_list.map_p (fun k ->
            B.Contents.read (contents_t t) k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) contents >>= fun contents ->
        let contents = Ir_misc.list_filter_map (fun x -> x) contents in
        return (Slice.create ~contents ~nodes ~commits ~tags ())

  let import_force t s =
    let aux (type k) (type v)
        name
        (type s)
        (module S: Ir_ao.STORE with type t = s and type key = k and type value = v)
        (module K: Tc.S0 with type t = k)
        (s:t -> s)
        elts
      =
      Lwt_list.iter_p (fun (k, v) ->
          S.add (s t) v >>= fun k' ->
          if not (K.equal k k') then
            Log.warnf "%s import error: expected %a, got %a"
              name force (show (module K) k) force (show (module K) k');
          return_unit
        ) elts
    in
    aux "Contents"
      (module B.Contents) (module B.Contents.Key) contents_t s.Slice.contents
    >>= fun () ->
    aux "Node" (module B.Node) (module B.Node.Key) node_t s.Slice.nodes
    >>= fun () ->
    aux "Commit"
      (module B.Commit) (module B.Commit.Key) commit_t s.Slice.commits

  let import t s =
    Lwt_list.partition_p
      (fun (tag, _) -> T.mem (tag_t t) tag)
      s.Slice.tags
    >>= fun (tags, duplicated_tags) ->
    import_force t { s with Slice.tags } >>= fun () ->
    match duplicated_tags with
    | [] -> return `Ok
    | l  -> return (`Duplicated_tags (List.map fst l))

end

module type MAKER =
  functor (K: Ir_hum.S) ->
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = K.t
           and type value = C.t
           and type head = H.t
           and type tag = T.t

module Make
    (AO: Ir_ao.MAKER)
    (RW: Ir_rw.MAKER)
    (P: Ir_path.S)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S) =
struct
  module P = struct
    module Contents = struct
      module Key = H
      module Val = C
      include AO (Key)(Val)
    end
    module Node = struct
      module Key = H
      module Val = Ir_node.Make (H)(H)(P)
      module Path = P
      include AO (Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Ir_commit.Make (H)(H)
      include AO (Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW (Key)(Val)
    end
  end
  include Make_ext(P)
end
