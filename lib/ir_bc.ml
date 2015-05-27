(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "BC" end)

module StringMap = Map.Make(String)

module type STORE = sig
  include Ir_rw.HIERARCHICAL
  type tag
  val of_tag: Ir_conf.t -> 'a Ir_task.f -> tag -> ('a -> t) Lwt.t
  val tag: t -> tag option Lwt.t
  val tag_exn: t -> tag Lwt.t
  val tags: t -> tag list Lwt.t
  val remove_tag: t -> tag -> unit Lwt.t
  val update_tag: t -> tag -> unit Lwt.t
  val merge_tag: t -> ?max_depth:int -> ?n:int -> tag -> unit Ir_merge.result Lwt.t
  val merge_tag_exn: t -> ?max_depth:int -> ?n:int -> tag -> unit Lwt.t
  type head
  val empty: Ir_conf.t -> 'a Ir_task.f -> ('a -> t) Lwt.t
  val of_head: Ir_conf.t -> ('a -> Ir_task.t) -> head -> ('a -> t) Lwt.t
  val head: t -> head option Lwt.t
  val head_exn: t -> head Lwt.t
  val branch: t -> [`Tag of tag | `Head of head | `Empty]
  val heads: t -> head list Lwt.t
  val update_head: t -> head -> unit Lwt.t
  val fast_forward_head: t -> ?max_depth:int -> ?n:int -> head -> bool Lwt.t
  val compare_and_set_head: t -> test:head option -> set:head option -> bool Lwt.t
  val merge_head: t -> ?max_depth:int -> ?n:int -> head -> unit Ir_merge.result Lwt.t
  val merge_head_exn: t -> ?max_depth:int -> ?n:int -> head -> unit Lwt.t
  val watch_head: t -> ?init:head -> (head Ir_watch.diff -> unit Lwt.t) ->
    (unit -> unit Lwt.t) Lwt.t
  val watch_tags: t -> ?init:(tag * head) list ->
    (tag -> head Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  val watch_key: t -> key -> ?init:(head * value) ->
    ((head * value) Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  val clone: 'a Ir_task.f -> t -> tag -> [`Ok of ('a -> t) | `Duplicated_tag] Lwt.t
  val clone_force: 'a Ir_task.f -> t ->  tag -> ('a -> t) Lwt.t
  val merge: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Ir_merge.result Lwt.t
  val merge_exn: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Lwt.t
  val lcas: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> ('a -> t) ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_tag: t -> ?max_depth:int -> ?n:int -> tag ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_head: t -> ?max_depth:int -> ?n:int -> head ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  module History: Graph.Sig.P with type V.t = head
  val history: ?depth:int -> ?min:head list -> ?max:head list -> t -> History.t Lwt.t
  val task_of_head: t -> head -> Ir_task.t Lwt.t
  type slice
  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  val import: t -> slice -> unit Lwt.t
end

module type PRIVATE = sig
  module Contents: Ir_contents.STORE
  module Node: Ir_node.STORE
    with type Val.contents = Contents.key and module Path = Contents.Path
  module Commit: Ir_commit.STORE with type Val.node = Node.key
  module Tag: Ir_tag.STORE with type value = Commit.key
  module Slice: Ir_slice.S
    with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Sync: Ir_sync.S with type head = Commit.key and type tag = Tag.key
end

module type STORE_EXT = sig
  include STORE
  module Key: Ir_path.S with type t = key
  module Val: Ir_contents.S with type t = value
  module Private: PRIVATE
    with type Contents.value = value
     and module Contents.Path = Key
     and type Commit.key = head
     and type Tag.key = tag
     and type Slice.t = slice
  val config: t -> Ir_conf.t
  val contents_t: t -> Private.Contents.t
  val node_t: t -> Private.Node.t
  val commit_t: t -> Private.Commit.t
  val tag_t: t -> Private.Tag.t
  val read_node: t -> key -> Private.Node.key option Lwt.t
  val mem_node: t -> key -> bool Lwt.t
  val update_node: t -> key -> Private.Node.key -> unit Lwt.t
end

module Make_ext (P: PRIVATE) = struct

  module Tag = P.Tag
  module Private = P

  type tag = Tag.key

  module Key = P.Node.Path
  module KeySet = Ir_misc.Set(Key)
  type key = Key.t

  module Val = P.Contents.Val
  type value = Val.t

  module Head = P.Commit.Key
  type head = Head.t

  type branch = [ `Tag of tag | `Head of head option ref ]

  module OCamlGraph = Graph
  module Graph = Ir_node.Graph(P.Contents)(P.Node)
  module H = Ir_commit.History(Graph.Store)(P.Commit)

  module KGraph =
    Ir_graph.Make(P.Contents.Key)(P.Node.Key)(P.Commit.Key)(Tag.Key)

  type t = {
    config: Ir_conf.t;
    task: Ir_task.t;
    contents: P.Contents.t;
    node: P.Node.t;
    commit: P.Commit.t;
    tag: Tag.t;
    branch: branch;
    lock: Lwt_mutex.t;
  }

  let config t = t.config
  let task t = t.task
  let tag_t t = t.tag
  let commit_t t = t.commit
  let node_t t = t.node
  let contents_t t = t.contents
  let graph_t t = t.contents, t.node
  let history_t t = graph_t t, t.commit
  let branch t = match t.branch with
    | `Tag t  -> `Tag t
    | `Head h -> match !h with None -> `Empty | Some h -> `Head h

  let tag t = match branch t with
    | `Tag t  -> Lwt.return (Some t)
    | `Empty
    | `Head _ -> Lwt.return_none


  let err_no_head = Ir_misc.invalid_arg "Irmin.%s: no head"
  let err_not_persistent =
    Ir_misc.invalid_arg "Irmin.%s: not a persistent branch"

  let tag_exn t = match branch t with
    | `Tag t  -> Lwt.return t
    | `Empty
    | `Head _ -> err_not_persistent "tag"

  let tags t =
    let tags = ref [] in
    Tag.iter (tag_t t) (fun t _ -> tags := t :: !tags; return_unit) >>= fun () ->
    return !tags

  let head t = match t.branch with
    | `Head key -> return !key
    | `Tag tag  -> Tag.read (tag_t t) tag

  let heads t =
    let heads = ref [] in
    Tag.iter (tag_t t) (fun _ h ->
        h >>= fun h -> heads := h :: !heads; return_unit
      ) >>= fun () ->
    head t >>= function
    | None   -> return !heads
    | Some h -> return (h :: List.filter ((<>) h) !heads)

  let head_exn t =
    head t >>= function
    | None   -> err_no_head "head"
    | Some k -> return k

  let of_branch config task branch =
    P.Contents.create config task >>= fun contents ->
    P.Node.create config task     >>= fun node ->
    P.Commit.create config task   >>= fun commit ->
    Tag.create config task        >>= fun tag ->
    let lock = Lwt_mutex.create () in
    (* [branch] is created outside of the closure as we want the
       branch to be shared by every invocation of the function return
       by [of_tag]. *)
    return (fun a ->
        { contents = contents a;
          node     = node a;
          commit   = commit a;
          tag      = tag a;
          task     = task a;
          config   = config;
          lock; branch }
      )

  let of_tag config task t =
    of_branch config task (`Tag t)

  let create config task =
    of_tag config task Tag.Key.master

  let empty config task =
    of_branch config task (`Head (ref None))

  let of_head config task key =
    of_branch config task (`Head (ref (Some key)))

  let read_head_commit t =
    Log.debug "read_head_commit";
    match branch t with
    | `Head key -> return (Some key)
    | `Empty    -> Lwt.return_none
    | `Tag tag  ->
      Tag.read (tag_t t) tag >>= function
      | None   -> return_none
      | Some k -> return (Some k)

  let read_head_node t =
    read_head_commit t >>= function
    | None   -> return_none
    | Some h -> H.node (history_t t) h

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let read_node t path =
    read_head_node t >>= function
    | None   -> return_none
    | Some n -> Graph.read_node (graph_t t) n path

  let mem_node t path =
    read_head_node t >>= function
    | None   -> return false
    | Some n -> Graph.mem_node (graph_t t) n path

  (* Retry an operation until the optimistic lock is happy. *)
  let retry name fn =
    let rec aux i =
      fn () >>= function
      | true  -> Lwt.return_unit
      | false ->
        Log.debug "Irmin.%s: conflict, retrying (%d)." name i;
        aux (i+1)
    in
    aux 1

  let with_commit t path ~f =
    let aux () =
      read_head_commit t >>= fun commit ->
      begin match commit with
        | None   -> Graph.empty (graph_t t)
        | Some h ->
          H.node (history_t t) h >>= function
          | None   -> Graph.empty (graph_t t)
          | Some n -> return n
      end >>= fun old_node ->
      f old_node >>= fun node ->
      let parents = parents_of_commit commit in
      H.create (history_t t) ~node ~parents >>= fun key ->
      match t.branch with
      | `Head head ->
        (* [head] is protected by [t.lock] *)
        head := Some key; Lwt.return true
      | `Tag tag   ->
        (* concurrent handle and/or process can modify the tag. Need to check
           that we are still working on the same head. *)
        Tag.compare_and_set (tag_t t) tag ~test:commit ~set:(Some key)
    in
    let msg = Printf.sprintf "with_commit(%s)" (Tc.show (module Key) path) in
    Lwt_mutex.with_lock t.lock (fun () -> retry msg aux)

  let update_node t path node =
    with_commit t path ~f:(fun head ->
        Graph.add_node (graph_t t) head path node
      )

  let map t path ~f =
    Log.debug "map %a" force (show (module Key) path);
    begin read_head_node t >>= function
      | None   -> Graph.empty (graph_t t)
      | Some n -> return n
    end >>= fun node ->
    f (graph_t t) node path

  let read t path =
    map t path ~f:Graph.read_contents >>= function
    | None   -> return_none
    | Some c -> P.Contents.read (contents_t t) c

  let update t path contents =
    Log.debug "update %a" force (show (module Key) path);
    P.Contents.add (contents_t t) contents >>= fun contents ->
    with_commit t path ~f:(fun node ->
        Graph.add_contents (graph_t t) node path contents
      )

  let remove t path =
    with_commit t path ~f:(fun node ->
        Graph.remove_contents (graph_t t) node path
      )

  let remove_rec t path =
    with_commit t path ~f:(fun node ->
        Graph.remove_node (graph_t t) node path
      )

  let read_exn t path =
    Log.debug "read_exn %a" force (show (module Key) path);
    map t path ~f:Graph.read_contents_exn >>= fun c ->
    P.Contents.read_exn (contents_t t) c

  let mem t path =
    map t path ~f:Graph.mem_contents

  let compare_and_set _ = failwith "Irmin.compare_and_set: TODO"

  (* Return the subpaths. *)
  let list t path =
    Log.debug "list";
    read_head_node t >>= function
    | None   -> return_nil
    | Some n ->
      Graph.read_node (graph_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        Graph.steps (graph_t t) node >>= fun steps ->
        let paths = List.map (fun c -> Key.rcons path c) steps in
        return paths

  let iter t fn =
    Log.debug "iter";
    head t >>= function
    | None   -> Lwt.return_unit
    | Some h ->
      (* we avoid races here by freezing the store head. *)
      of_head t.config (fun () -> t.task) h >>= fun t ->
      let t = t () in
      let rec aux acc = function
        | []       -> Lwt_list.iter_p (fun (path, v) -> fn path v) acc
        | path::tl ->
          list t path >>= fun childs ->
          let todo = childs @ tl in
          mem t path >>= fun exists ->
          if not exists then aux acc todo
          else aux ((path, read_exn t path) :: acc) todo
    in
    list t Key.empty >>= aux []

  let lcas a ?max_depth ?n t1 t2 =
    let t1 = t1 a and t2 = t2 a in
    head_exn t1 >>= fun h1 ->
    head_exn t2 >>= fun h2 ->
    H.lcas (history_t t1) ?max_depth ?n h1 h2

  let lcas_head t ?max_depth ?n head =
    head_exn t >>= fun h ->
    H.lcas (history_t t) ?max_depth ?n h head

  let lcas_tag t ?max_depth ?n tag =
    head_exn t >>= fun h ->
    head_exn { t with branch = `Tag tag } >>= fun head ->
    H.lcas (history_t t) ?max_depth ?n h head

  let task_of_head t head =
    P.Commit.read_exn (commit_t t) head >>= fun commit ->
    Lwt.return (P.Commit.Val.task commit)

  (* Merge two commits:
     - Search for common ancestors
     - Perform recursive 3-way merges *)
  let three_way_merge t ?max_depth ?n c1 c2 =
    H.three_way_merge (history_t t) ?max_depth ?n c1 c2

  let update_head t c =
    match t.branch with
    | `Head h  -> h := Some c; return_unit
    | `Tag tag -> Tag.update (tag_t t) tag c

  let remove_tag t tag = Tag.remove (tag_t t) tag

  let update_tag t tag =
    Tag.read_exn (tag_t t) tag >>= fun k ->
    update_head t k

  let compare_and_set_head_unsafe t ~test ~set =
    match t.branch with
    | `Head head ->
      (* [head] is protected by [t.lock]. *)
      if !head = test then (head := set; Lwt.return true)
      else Lwt.return false
    | `Tag tag -> Tag.compare_and_set (tag_t t) tag ~test ~set

  let compare_and_set_head t ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        compare_and_set_head_unsafe t ~test ~set
      )

  let fast_forward_head t ?max_depth ?n new_head =
    head t >>= function
    | None  -> compare_and_set_head t ~test:None ~set:(Some new_head)
    | Some old_head ->
      let pp = show (module Head) in
      Log.debug "fast-forward-head old=%a new=%a"
        force (pp old_head) force (pp new_head);
      if Head.equal new_head old_head then
        (* we only update if there is a change *)
        Lwt.return_false
      else
        H.lcas (history_t t) ?max_depth ?n new_head old_head >>= function
        | `Ok [x] when Head.equal x old_head ->
          (* we only update if new_head > old_head *)
          compare_and_set_head t ~test:(Some old_head) ~set:(Some new_head)
        | `Too_many_lcas -> Log.debug "ff: too many LCAs"; Lwt.return false
        | `Max_depth_reached -> Log.debug "ff: max depth reached"; Lwt.return false
        | `Ok _ -> Lwt.return false

  let retry_merge name fn =
    let rec aux i =
      fn () >>= function
      | `Conflict _ as c -> Lwt.return c
      | `Ok true  -> ok ()
      | `Ok false ->
        Log.debug "Irmin.%s: conflict, retrying (%d)." name i;
        aux (i+1)
    in
    aux 1

  (* FIXME: we might want to keep the new commit in case of conflict,
     and use it as a base for the next merge. *)
  let merge_head t ?max_depth ?n c1 =
    Log.debug "merge_head";
    let aux () =
      read_head_commit t >>= fun head ->
      match head with
      | None    ->
        compare_and_set_head_unsafe t ~test:head ~set:(Some c1) >>=
        ok
      | Some c2 ->
        three_way_merge t ?max_depth ?n c1 c2 >>| fun c3 ->
        compare_and_set_head_unsafe t ~test:head ~set:(Some c3) >>=
        ok
    in
    Lwt_mutex.with_lock t.lock (fun () -> retry_merge "merge_head" aux)

  let merge_head_exn t ?max_depth ?n c1 =
    merge_head t ?max_depth ?n c1 >>= Ir_merge.exn

  let clone_force task t tag =
    Log.debug "clone_force %a" force (show (module Tag.Key) tag);
    let return () = of_tag t.config task tag in
    head t >>= function
    | None   -> return ()
    | Some h -> Tag.update (tag_t t) tag h >>= return

  let clone task t tag =
    Tag.mem (tag_t t) tag >>= function
    | true  -> return `Duplicated_tag
    | false -> clone_force task t tag >>= fun t -> return (`Ok t)

  let merge_tag t ?max_depth ?n tag =
    Log.debug "merge_tag %a" force (show (module Tag.Key) tag);
    Tag.read (tag_t t) tag >>= function
    | None  ->
      let str =
        Printf.sprintf "merge_tag: %s is not a valid tag" (Tag.Key.to_hum tag)
      in
      Lwt.fail (Failure str)
    | Some c -> merge_head t ?max_depth ?n c

  let merge_tag_exn t ?max_depth ?n tag =
    merge_tag t ?max_depth ?n tag >>= Ir_merge.exn

  let merge a ?max_depth ?n t ~into =
    Log.debug "merge";
    let t = t a and into = into a in
    match branch t with
    | `Tag tag -> merge_tag  into ?max_depth ?n tag
    | `Head h  -> merge_head into ?max_depth ?n h
    | `Empty   -> ok ()

  let merge_exn a ?max_depth ?n t ~into =
    merge a ?max_depth ?n t ~into >>= Ir_merge.exn

  let watch_head t ?init fn =
    tag t >>= function
    | None       ->
      (* FIXME: start a local watcher on the detached branch *)
      Lwt.return (fun () -> Lwt.return_unit)
    | Some tag0 ->
      let init = match init with
        | None       -> None
        | Some head0 -> Some [tag0, head0]
      in
      Tag.watch (tag_t t) ?init (fun tag head ->
          if Tag.Key.equal tag0 tag then fn head else Lwt.return_unit
        ) >>= fun id ->
      Lwt.return (fun () -> Tag.unwatch (tag_t t) id)

  let watch_tags t ?init fn =
    Tag.watch (tag_t t) ?init fn >>= fun id ->
    Lwt.return (fun () -> Tag.unwatch (tag_t t) id)

  let lift value_of_head fn = function
    | `Removed x -> begin
        value_of_head x >>= function
        | None   -> Lwt.return_unit
        | Some v -> fn @@ `Removed (x, v)
      end
    | `Added x -> begin
        value_of_head x >>= function
        | None   -> Lwt.return_unit
        | Some v -> fn @@ `Added (x, v)
      end
    | `Updated (x, y) ->
      assert (not (Head.equal x y));
      value_of_head x >>= fun vx ->
      value_of_head y >>= fun vy ->
      match vx, vy with
      | None   ,  None   -> Lwt.return_unit
      | Some vx, None    -> fn @@ `Removed (x, vx)
      | None   , Some vy -> fn @@ `Added (y, vy)
      | Some vx, Some vy ->
        if Val.equal vx vy then Lwt.return_unit
        else fn @@ `Updated ( (x, vx), (y, vy) )

let watch_key t key ?init fn =
    Log.info "watch-key %a" force (show (module Key) key);
    let init_head = match init with
      | None        -> None
      | Some (h, _) -> Some h
    in
    let value_of_head h =
      of_head (config t) (fun () -> task t) h >>= fun t ->
      read (t ()) key
    in
    watch_head t ?init:init_head (lift value_of_head fn)

  type slice = P.Slice.t

  let export ?(full=true) ?depth ?(min=[]) ?max t =
    Log.debug "export depth=%s full=%b min=%d max=%s"
      (match depth with None -> "<none>" | Some d -> string_of_int d)
      full (List.length min)
      (match max with None -> "<none>" | Some l -> string_of_int (List.length l));
    begin match max with
      | Some m -> return m
      | None   -> heads t
    end >>= fun max ->
    P.Slice.create () >>= fun slice ->
    let max = List.map (fun x -> `Commit x) max in
    let min = List.map (fun x -> `Commit x) min in
    let pred = function
      | `Commit k ->
        H.parents (history_t t) k >>= fun parents ->
        return (List.map (fun x -> `Commit x) parents)
      | _ -> return_nil in
    KGraph.closure ?depth ~pred ~min ~max () >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Commit c -> Some c | _ -> None)
        (KGraph.vertex g)
    in
    let root_nodes = ref [] in
    Lwt_list.iter_p (fun k ->
        P.Commit.read_exn (commit_t t) k >>= fun c ->
        let () = match P.Commit.Val.node c with
          | None   -> ()
          | Some n -> root_nodes := n :: !root_nodes
        in
        P.Slice.add_commit slice (k, c)
      ) keys
    >>= fun () ->
    if not full then
      return slice
    else
      (* XXX: we can compute a [min] if needed *)
      Graph.closure (graph_t t) ~min:[] ~max:!root_nodes >>= fun nodes ->
      let module KSet = Ir_misc.Set(P.Contents.Key) in
      let contents = ref KSet.empty in
      Lwt_list.iter_p (fun k ->
          P.Node.read (node_t t) k >>= function
          | None   -> return_unit
          | Some v ->
            P.Node.Val.iter_contents v (fun _ k ->
                contents := KSet.add k !contents;
              );
            P.Slice.add_node slice (k, v)
        ) nodes >>= fun () ->
      Lwt_list.iter_p (fun k ->
          P.Contents.read (contents_t t) k >>= function
          | None   -> return_unit
          | Some v -> P.Slice.add_contents slice (k, v)
        ) (KSet.to_list !contents) >>= fun () ->
      return slice

  let import t s =
    let aux (type k) (type v)
        name
        (type s)
        (module S: Ir_ao.STORE with type t = s and type key = k and type value = v)
        (module K: Tc.S0 with type t = k)
        fn
        (s:t -> s)
      =
      fn (fun (k, v) ->
          S.add (s t) v >>= fun k' ->
          if not (K.equal k k') then
            Log.warn "%s import error: expected %a, got %a"
              name force (show (module K) k) force (show (module K) k');
          return_unit
        )
    in
    aux "Contents"
      (module P.Contents) (module P.Contents.Key)
      (P.Slice.iter_contents s) contents_t
    >>= fun () ->
    aux "Node"
      (module P.Node) (module P.Node.Key)
      (P.Slice.iter_nodes s) node_t
    >>= fun () ->
    aux "Commit"
      (module P.Commit) (module P.Commit.Key)
      (P.Slice.iter_commits s) commit_t

  module History =
    OCamlGraph.Persistent.Digraph.ConcreteBidirectional(P.Commit.Key)

  module Gmap = struct
    include OCamlGraph.Gmap.Vertex(KGraph)(struct
        include History
        let empty () = History.empty
      end)

    let filter_map f g =
      let t = filter_map f g in
      KGraph.fold_edges (fun x y t ->
          match f x, f y with
          | Some x, Some y -> History.add_edge t x y
          | _ -> t
        ) g t

    let _map f g =
      let t = map f g in
      KGraph.fold_edges (fun x y t ->
          History.add_edge t (f x) (f y)
        ) g t

  end

  let history ?depth ?(min=[]) ?max t =
    Log.debug "history";
    let pred = function
      | `Commit k ->
        H.parents (history_t t) k >>= fun parents ->
        return (List.map (fun x -> `Commit x) parents)
      | _ -> return_nil in
    begin head t >>= function
      | Some h -> Lwt.return [h]
      | None   -> match max with None -> Lwt.return_nil | Some m -> Lwt.return m
    end >>= fun max ->
    let max = List.map (fun k -> `Commit k) max in
    let min = List.map (fun k -> `Commit k) min in
    KGraph.closure ?depth ~min ~max ~pred () >>= fun g ->
    let h = Gmap.filter_map (function `Commit k -> Some k | _ -> None) g in
    Lwt.return h

end

module type MAKER =
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = C.Path.t
           and type value = C.t
           and type head = H.t
           and type tag = T.t

module Make
    (AO: Ir_ao.MAKER)
    (RW: Ir_rw.MAKER)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S) =
struct
  module X = struct
    module Contents = Ir_contents.Make(struct
        include AO(H)(C)
        module Key = H
        module Val = C
      end)
    module Node = struct
      module Key = H
      module Val = Ir_node.Make (H)(H)(C.Path)
      module Path = C.Path
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
    module Slice = Ir_slice.Make(Contents)(Node)(Commit)
    module Sync = Ir_sync.None(H)(T)
  end
  include Make_ext(X)
end
