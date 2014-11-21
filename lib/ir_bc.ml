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
  val of_tag: tag -> t
  val tag: t -> tag option
  val tag_exn: t -> tag
  val update_tag: t -> origin -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  val update_tag_force: t -> origin -> tag -> unit Lwt.t
  val detach: t -> origin -> unit Lwt.t
  type head
  val of_head: head -> t
  val head: t -> origin -> head option Lwt.t
  val head_exn: t -> origin -> head Lwt.t
  val heads: t -> origin -> head list Lwt.t
  val update_head: t -> origin -> head -> unit Lwt.t
  val merge_head: t -> origin -> head -> unit Ir_merge.result Lwt.t
  val watch_head: t -> origin -> key -> (key * head) Lwt_stream.t
  val clone: t -> origin -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  val clone_force: t -> origin -> tag -> t Lwt.t
  val switch: t -> origin -> tag -> unit Lwt.t
  val merge: t -> origin -> tag -> unit Ir_merge.result Lwt.t
  module T: Tc.I0 with type t = t
  type slice
  module Slice: Tc.I0 with type t = slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> origin -> slice Lwt.t
  val import: t -> origin -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  val import_force: t -> origin -> slice -> unit Lwt.t
end

module type MAKER =
  functor (B: Ir_block.STORE) ->
  functor (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
    -> STORE with type key = B.step list
              and type value = B.contents
              and type origin = B.origin
              and type tag = T.key
              and type head = B.head


module type STORE_EXT = sig
  type step
  include STORE with type key = step list
  module Block: Ir_block.STORE
    with type step = step
     and type contents = value
     and type origin = origin
     and type head = head
  module Tag: Ir_tag.STORE
    with type key = tag and type value = head and type origin = origin
  module Key: Tc.I0 with type t = Block.step list
  module Val: Ir_contents.S with type t = value
  module Origin: Ir_origin.S with type t = origin
  val read_node: t -> origin -> key -> Block.Node.value option Lwt.t
  val mem_node: t -> origin -> key -> bool Lwt.t
  val update_node: t -> origin -> key -> Block.Node.value -> unit Lwt.t
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

module type MAKER_EXT =
  functor (B: Ir_block.STORE) ->
  functor (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
    -> STORE_EXT with type step = B.step
                  and type value = B.contents
                  and type origin = B.origin
                  and type tag = T.key
                  and type head = B.head
                  and module Block = B
                  and module Tag = T

module Make_ext
    (B: Ir_block.STORE)
    (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
= struct

  module Block = B

  module Origin = B.Origin
  type origin = Origin.t

  module Tag = T
  type tag = T.key

  module Key = Ir_step.Path(Block.Step)
  type key = Key.t

  module Val = Block.Contents.Val
  type value = Val.t

  module Path = Ir_step.Path(B.Step)
  module PathSet = Ir_misc.Set(Path)

  module StepMap = Ir_misc.Map(B.Node.Step)
  type step = B.step

  module Head = B.Commit.Key
  type head = Head.t

  module TK = struct

    type t = [ `Tag of tag | `Key of B.Commit.key ]

    module T = T.Key
    module K = Head

    let hash = Hashtbl.hash
    let compare = Pervasives.compare

    let equal x y = match x, y with
      | `Tag x, `Tag y -> T.equal x y
      | `Key x, `Key y -> K.equal x y
      | _ -> false

    let to_sexp t =
      let open Sexplib.Type in
      match t with
      | `Tag t -> List [ Atom "tag"; T.to_sexp t ]
      | `Key k -> List [ Atom "key"; K.to_sexp k ]

    let to_json = function
      | `Tag t -> `O [ "tag", T.to_json t ]
      | `Key k -> `O [ "key", K.to_json k ]

    let of_json = function
      | `O [ "tag", j ] -> `Tag (T.of_json j)
      | `O [ "key", j ] -> `Key (K.of_json j)
      | j -> Ezjsonm.parse_error j "BC.TagKey.of_json"

    let write t buf =
      match t with
      | `Tag t -> T.write t (Ir_misc.tag buf 0)
      | `Key k -> K.write k (Ir_misc.tag buf 1)

    let read buf =
      match Ir_misc.untag buf with
      | 0 -> `Tag (T.read buf)
      | 1 -> `Key (K.read buf)
      | n -> Tc.Reader.error "BC.TagKey.read (tag=%d)" n

    let size_of t = 1 + match t with
      | `Tag t -> T.size_of t
      | `Key k -> K.size_of k

  end

  module Graph = Ir_graph.Make(B.Contents.Key)(B.Node.Key)(B.Commit.Key)(T.Key)

  type t = { mutable branch: TK.t }

  let tag t = match t.branch with
    | `Tag t -> Some t
    | `Key _ -> None

  let tag_exn t = match t.branch with
    | `Tag t -> t
    | `Key _ -> raise Not_found

  let set_tag t tag =
    t.branch <- `Tag tag

  let head t origin = match t.branch with
    | `Key key -> return (Some key)
    | `Tag tag -> T.read (T.create ()) origin tag

  let heads t origin =
    T.dump (T.create ()) origin >>= fun tags ->
    let heads = List.map snd tags in
    head t origin >>= function
    | None   -> return heads
    | Some h -> return (h :: List.filter ((<>) h) heads)

  let head_exn t origin =
    head t origin >>= function
    | None   -> fail Not_found
    | Some k -> return k

  let detach t origin =
    match t.branch with
    | `Key _   -> return_unit
    | `Tag tag ->
      T.read_exn (T.create ()) origin tag >>= fun key ->
      t.branch <- `Key key;
      return_unit

  let of_tag tag =
    { branch = `Tag tag }

  let create () =
    of_tag T.Key.master

  let of_head key =
    { branch = `Key key }

  let read_head_commit t origin =
    match t.branch with
    | `Key key ->
      Log.debugf "read detached head: %a" force (show (module Head) key);
      B.Commit.read (B.Commit.create ()) origin key
    | `Tag tag ->
      Log.debugf "read head: %a" force (show (module T.Key) tag);
      T.read (T.create ()) origin tag >>= function
      | None   -> return_none
      | Some k -> B.Commit.read (B.Commit.create ()) origin k

  let node_of_commit _t origin c =
    match B.Commit.node (B.Commit.create ()) origin c with
    | None   -> return B.Node.empty
    | Some n -> n

  let node_of_opt_commit t origin = function
    | None   -> return B.Node.empty
    | Some c -> node_of_commit t origin c

  let read_head_node t origin =
    Log.debug (lazy "read_head_node");
    read_head_commit t origin >>=
    node_of_opt_commit t origin

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let read_node t origin path =
    read_head_commit t origin >>= fun commit ->
    node_of_opt_commit t origin commit >>= fun node ->
    B.Node.sub (B.Node.create ()) origin node path

  let mem_node t origin path =
    read_node t origin path >>= function
    | None   -> return false
    | Some _ -> return true

  let apply t origin ~f =
    read_head_commit t origin >>= fun commit ->
    node_of_opt_commit t origin commit >>= fun old_node ->
    f old_node >>= fun node ->
    if B.Node.Val.equal node old_node then return_unit
    else (
      let parents = parents_of_commit commit in
      B.Commit.commit (B.Commit.create ()) origin ~node ~parents
      >>= fun (key, _) ->
      (* XXX: the head might have changed since we started the operation *)
      match t.branch with
      | `Key _   -> t.branch <- `Key key; return_unit
      | `Tag tag -> T.update (T.create ()) origin tag key
    )

 let update_node t origin path node =
    apply t origin ~f:(fun head ->
        B.Node.map (B.Node.create ()) origin head path (fun _ -> node)
      )

  let map t origin path ~f =
    read_head_node t origin >>= fun node ->
    f (B.Node.create ()) origin node path

  let read t path =
    map t path ~f:B.Node.find

  let update t origin path contents =
    Log.debugf "update %a" force (show (module Path) path);
    apply t origin ~f:(fun node ->
        B.Node.update (B.Node.create ()) origin node path contents
      )

  let remove t origin path =
    apply t origin ~f:(fun node ->
        B.Node.remove (B.Node.create ()) origin node path
      )

  let read_exn t origin path =
    Log.debugf "read_exn %a" force (show (module Path) path);
    map t origin path ~f:B.Node.find_exn

  let mem t origin path =
    map t origin path ~f:B.Node.valid

  (* Return the subpaths. *)
  let list t origin paths =
    Log.debugf "list";
    let t_n = B.Node.create () in
    let one path =
      read_head_node t origin >>= fun n ->
      B.Node.sub t_n origin n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = B.Node.succ t_n origin node in
        let c = StepMap.keys c in
        let paths = List.map (fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = PathSet.of_list paths in
        return (PathSet.union set paths)
      ) PathSet.empty paths
    >>= fun paths ->
    return (PathSet.to_list paths)

  let dump t origin =
    Log.debugf "dump";
    read_head_node t origin >>= fun node ->
    let t_n = B.Node.create () in
    begin B.Node.find t_n origin node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end >>= fun init ->
    let rec aux seen = function
      | []       -> return (List.sort Pervasives.compare seen)
      | path::tl ->
        list t origin [path] >>= fun childs ->
        let todo = childs @ tl in
        B.Node.find t_n origin node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo
    in
    list t origin [[]] >>= aux init

  (* Merge two commits:
     - Search for a common ancestor
     - Perform a 3-way merge *)
  let three_way_merge _t origin c1 c2 =
    Log.debugf "3-way merge between %a and %a"
      force (show (module Head) c1)
      force (show (module Head) c2);
    let t_c = B.Commit.create () in
    B.Commit.find_common_ancestor t_c origin c1 c2 >>= function
    | None     -> conflict "no common ancestor"
    | Some old -> B.Commit.merge t_c origin ~old c1 c2

  let update_head t origin c =
    match t.branch with
    | `Key _   -> t.branch <- `Key c; return_unit
    | `Tag tag -> Tag.update (T.create ()) origin tag c

  let update_tag_force t origin tag =
    begin head t origin >>= function
      | None   -> return_unit
      | Some k -> T.update (T.create ()) origin tag k
    end >>= fun () ->
    set_tag t tag;
    return_unit

  let update_tag t origin tag =
    let t_t = T.create () in
    T.mem t_t origin tag >>= function
    | true -> return `Duplicated_tag
    | false -> update_tag_force t origin tag >>= fun () -> return `Ok

  let switch t origin branch =
    Log.debugf "switch %a" force (show (module T.Key) branch);
    T.read (T.create ()) origin branch >>= function
    | Some c -> update_head t origin c
    | None   -> fail Not_found

  let merge_head t origin c1 =
    let aux c2 =
      three_way_merge t origin c1 c2 >>| fun c3 ->
      update_head t origin c3 >>= ok
    in
    match t.branch with
    | `Key c2  -> aux c2
    | `Tag tag ->
      T.read (T.create ()) origin tag >>= function
      | None    -> update_head t origin c1 >>= ok
      | Some c2 -> aux c2

  let clone_force t origin branch =
    Log.debugf "clone %a" force (show (module T.Key) branch);
    let t_t = T.create () in
    begin match t.branch with
      | `Key c -> T.update t_t origin branch c
      | `Tag tag ->
        T.read t_t origin tag >>= function
        | None   -> fail Not_found
        | Some c -> Tag.update t_t origin branch c
    end  >>= fun () ->
    return { branch = `Tag branch }

  let clone t origin branch =
    T.mem (T.create ()) origin branch >>= function
    | true  -> return `Duplicated_tag
    | false -> clone_force t origin branch >>= fun t -> return (`Ok t)

  let merge t origin branch =
    Log.debugf "merge %a" force (show (module T.Key) branch);
    T.read_exn (T.create ()) origin branch >>= fun c ->
    merge_head t origin c

  let watch_head t origin path =
    Log.infof "Adding a watch on %a" force (show (module Path) path);
    match t.branch with
    | `Key _   -> Lwt_stream.of_list []
    | `Tag tag ->
      let t_t = T.create () in
      let stream = Tag.watch t_t origin tag in
      Ir_misc.Lwt_stream.lift (
        read_node t origin path >>= fun node ->
        let old_node = ref node in
        let stream = Lwt_stream.filter_map_s (fun key ->
            Log.debugf "watch: %a" force (show (module Head) key);
            B.Commit.read_exn (B.Commit.create ()) origin key >>= fun commit ->
            begin match B.Commit.node (B.Commit.create ()) origin commit with
              | None      -> return B.Node.empty
              | Some node -> node
            end >>= fun node ->
            B.Node.sub (B.Node.create ()) origin node path >>= fun node ->
            if node = !old_node then return_none
            else (
              old_node := node;
              return (Some (path, key))
            )
          ) stream in
        return stream
      )

  (* watch contents changes. *)
  let watch t origin path =
    let stream = watch_head t origin path in
    Lwt_stream.filter_map_s (fun (p, k) ->
        if Path.equal p path then
          B.Commit.read (B.Commit.create ()) origin k >>= function
          | None   -> return_none
          | Some c ->
            node_of_commit t origin c >>= fun n ->
            B.Node.find (B.Node.create ()) origin n p
        else
          return_none
      ) stream

  module Slice = struct

    type t = {
      contents: (B.Contents.key * B.Contents.value) list;
      nodes   : (B.Node.key * B.Node.value) list;
      commits : (B.Commit.key * B.Commit.value) list;
      tags    : (T.key * T.value) list;
    }

    let create ?(contents=[]) ?(nodes=[]) ?(commits=[]) ?(tags=[]) () =
      { contents; nodes; commits; tags }

    module M (K: Tc.I0)(V: Tc.I0) = Tc.App1 (Tc.L)( Tc.App2(Tc.P)(K)(V) )
    module Ct = M(B.Contents.Key)(B.Contents.Val)
    module No = M(B.Node.Key)(B.Node.Val)
    module Cm = M(B.Commit.Key)(B.Commit.Val)
    module Ta = M(T.Key)(T.Val)
    module T = Tc.App2 (Tc.P)( Tc.App2(Tc.P)(Ct)(No) )( Tc.App2(Tc.P)(Cm)(Ta) )

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

  let export ?(full=true) ?depth ?(min=[]) ?max t origin =
    Log.debugf "export depth=%s full=%b min=%d max=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      full (List.length min)
      (match max with None -> "<none>" | Some l -> string_of_int (List.length l));
    begin match max with
      | Some m -> return m
      | None   -> heads t origin
    end >>= fun max ->
    T.dump (T.create ()) origin >>= fun tags ->
    let tags = List.filter (fun (_, k) -> List.mem k max) tags in
    let max = List.map (fun x -> `Commit x) max in
    let min = List.map (fun x -> `Commit x) min in
    let t_c = B.Commit.create () in
    let pred = function
        | `Commit k ->
          begin
            B.Commit.read t_c origin k >>= function
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
          B.Commit.read_exn t_c origin k >>= fun c ->
          return (k, c)
        ) keys
      >>= fun commits ->
      if not full then
        return (Slice.create ~commits ~tags ())
      else
        let root_nodes =
          Ir_misc.list_filter_map (fun (_,c) -> B.Commit.Val.node c) commits
        in
        let t_n = B.Node.create () in
        B.Node.list t_n origin root_nodes >>= fun nodes ->
        Lwt_list.map_p (fun k ->
            B.Node.read t_n origin k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) nodes >>= fun nodes ->
        let nodes = Ir_misc.list_filter_map (fun x -> x) nodes in
        let root_contents =
          Ir_misc.list_filter_map (fun (_,n) -> B.Node.Val.contents n) nodes
        in
        let t_c = B.Contents.create () in
        B.Contents.list t_c origin root_contents >>= fun contents ->
        Lwt_list.map_p (fun k ->
            B.Contents.read t_c origin k >>= function
            | None   -> return_none
            | Some v -> return (Some (k, v))
          ) contents >>= fun contents ->
        let contents = Ir_misc.list_filter_map (fun x -> x) contents in
        return (Slice.create ~contents ~nodes ~commits ~tags ())

  let import_force _t origin s =
    let aux (type k) (type v)
        name
        (module S: Ir_ao.STORE with type key = k
                                and type value = v
                                and type origin = origin)
        (module K: Tc.I0 with type t = k)
        elts
      =
      let t = S.create () in
      Lwt_list.iter_p (fun (k, v) ->
          S.add t origin v >>= fun k' ->
          if not (K.equal k k') then
            Log.warnf "%s import error: expected %a, got %a"
              name force (show (module K) k) force (show (module K) k');
          return_unit
        ) elts
    in
    aux "Contents" (module B.Contents) (module B.Contents.Key) s.Slice.contents
    >>= fun () ->
    aux "Node" (module B.Node) (module B.Node.Key) s.Slice.nodes
    >>= fun () ->
    aux "Commit" (module B.Commit) (module B.Commit.Key) s.Slice.commits
    >>= fun () ->
    let t_t = T.create () in
    Lwt_list.iter_p (fun (k, v) -> T.update t_t origin k v) s.Slice.tags

  let import t origin s =
    Lwt_list.partition_p
      (fun (t, _) -> T.mem (T.create ()) origin t)
      s.Slice.tags
    >>= fun (tags, duplicated_tags) ->
    import_force t origin { s with Slice.tags } >>= fun () ->
    match duplicated_tags with
    | [] -> return `Ok
    | l  -> return (`Duplicated_tags (List.map fst l))

  module T = struct
    type r = t
    type t = r
    let hash t = TK.hash t.branch
    let compare x y = TK.compare x.branch y.branch
    let equal x y = TK.equal x.branch y.branch
    let to_sexp t = TK.to_sexp t.branch
    let to_json t = TK.to_json t.branch
    let of_json j = { branch = TK.of_json j }
    let write t = TK.write t.branch
    let read b = { branch = TK.read b }
    let size_of t = TK.size_of t.branch
  end

end

module Make
    (B: Ir_block.STORE)
    (T: Ir_tag.STORE with type value = B.head and type origin = B.origin)
= struct
  include Make_ext(B)(T)
end
