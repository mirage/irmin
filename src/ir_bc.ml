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

let src = Logs.Src.create "irmin.bc" ~doc:"Irmin branch-consistent store"
module Log = (val Logs.src_log src : Logs.LOG)

module StringMap = Map.Make(String)

(* TODO: the BC interface should export file metadata (e.g. Git file type).
   Search for [_meta] and [Metadata.default] for places that may need to
   change. *)

module Make (P: Ir_s.PRIVATE) = struct

  module Ref_store = P.Ref
  module Metadata = P.Node.Val.Metadata

  type branch_id = Ref_store.key

  module Key = P.Node.Path
  module KeySet = Ir_misc.Set(Key)
  type key = Key.t

  module Val = P.Contents.Val
  type value = Val.t

  module Hash = P.Commit.Key
  type commit_id = Hash.t

  type head_ref = [ `Branch of branch_id | `Head of commit_id option ref ]

  module OCamlGraph = Graph
  module Graph = Ir_node.Graph(P.Node)
  module H = Ir_commit.History(P.Commit)

  module KGraph =
    Ir_graph.Make(P.Contents.Key)(P.Node.Key)(P.Commit.Key)(Ref_store.Key)

  type slice = P.Slice.t

  module Repo = struct
    type t = P.Repo.t

    let create = P.Repo.create

    let graph_t t = P.Repo.node_t t
    let history_t t = P.Repo.commit_t t

    let branches t =
      let branches = ref [] in
      Ref_store.iter (P.Repo.ref_t t) (fun t _ ->
          branches := t :: !branches; return_unit
        ) >>= fun () ->
      return !branches

    let remove_branch t name = Ref_store.remove (P.Repo.ref_t t) name

    let heads t =
      let heads = ref [] in
      Ref_store.iter (P.Repo.ref_t t) (fun _ h ->
          h () >>= fun h -> heads := h :: !heads; return_unit
        ) >|= fun () ->
      !heads

    let watch_branches t ?init fn =
      Ref_store.watch (P.Repo.ref_t t) ?init fn >>= fun id ->
      Lwt.return (fun () -> Ref_store.unwatch (P.Repo.ref_t t) id)

    let export ?(full=true) ?depth ?(min=[]) ?(max=[]) t =
      Log.debug (fun f -> f "export depth=%s full=%b min=%d max=%d"
        (match depth with None -> "<none>" | Some d -> string_of_int d)
        full (List.length min) (List.length max));
      begin match max with
        | [] -> heads t
        | m -> return m
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
          P.Commit.read_exn (P.Repo.commit_t t) k >>= fun c ->
          root_nodes := P.Commit.Val.node c :: !root_nodes;
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
            P.Node.read (P.Repo.node_t t) k >>= function
            | None   -> return_unit
            | Some v ->
              P.Node.Val.iter_contents v (fun _ (k, _meta) ->
                  contents := KSet.add k !contents;
                );
              P.Slice.add_node slice (k, v)
          ) nodes >>= fun () ->
        Lwt_list.iter_p (fun k ->
            P.Contents.read (P.Repo.contents_t t) k >>= function
            | None   -> return_unit
            | Some v -> P.Slice.add_contents slice (k, v)
          ) (KSet.to_list !contents) >>= fun () ->
        return slice

    exception Import_error

    let import t s =
      let aux (type k) (type v)
          name
          (type s)
          (module S: Ir_s.AO_STORE with type t = s and type key = k and type value = v)
          (module K: Tc.S0 with type t = k)
          fn
          (s:t -> s)
        =
        fn (fun (k, v) ->
            S.add (s t) v >>= fun k' ->
            if not (K.equal k k') then (
              Log.err (fun f -> f "%s import error: expected %a, got %a"
                name (show (module K)) k (show (module K)) k');
              Lwt.fail Import_error
            )
            else Lwt.return_unit
          )
      in
      Lwt.catch (fun () ->
          aux "Contents"
            (module P.Contents) (module P.Contents.Key)
            (P.Slice.iter_contents s) P.Repo.contents_t
          >>= fun () ->
          aux "Node"
            (module P.Node) (module P.Node.Key)
            (P.Slice.iter_nodes s) P.Repo.node_t
          >>= fun () ->
          aux "Commit"
            (module P.Commit) (module P.Commit.Key)
            (P.Slice.iter_commits s) P.Repo.commit_t
          >>= fun () ->
          Lwt.return `Ok)
        (function Import_error -> Lwt.return `Error | e -> Lwt.fail e)

    let task_of_commit_id t commit_id =
      P.Commit.read_exn (P.Repo.commit_t t) commit_id >>= fun commit ->
      Lwt.return (P.Commit.Val.task commit)

  end

  type t = {
    repo: Repo.t;
    task: Ir_task.t;
    head_ref: head_ref;
    lock: Lwt_mutex.t;
  }

  let repo t = t.repo
  let task t = t.task
  let ref_t t = P.Repo.ref_t t.repo
  let commit_t t = P.Repo.commit_t t.repo
  let node_t t = P.Repo.node_t t.repo
  let contents_t t = P.Repo.contents_t t.repo
  let graph_t t = node_t t
  let history_t t = commit_t t
  let head_ref t = match t.head_ref with
    | `Branch t -> `Branch t
    | `Head h -> match !h with None -> `Empty | Some h -> `Head h

  let name t = match head_ref t with
    | `Branch t  -> Lwt.return (Some t)
    | `Empty
    | `Head _ -> Lwt.return_none


  let err_no_head = Ir_misc.invalid_arg "Irmin.%s: no head"
  let err_not_persistent =
    Ir_misc.invalid_arg "Irmin.%s: not a persistent branch"

  let name_exn t = match head_ref t with
    | `Branch t  -> Lwt.return t
    | `Empty
    | `Head _ -> err_not_persistent "name_exn"

  let head t = match t.head_ref with
    | `Head key -> return !key
    | `Branch name  -> Ref_store.read (ref_t t) name

  let head_exn t =
    head t >>= function
    | None   -> err_no_head "head"
    | Some k -> return k

  let of_ref repo task head_ref =
    let lock = Lwt_mutex.create () in
    return (fun a ->
        { repo         = repo;
          task         = task a;
          lock; head_ref }
      )

  let err_invalid_branch_id t =
    let err = Printf.sprintf "%S is not a valid branch name." (Ref_store.Key.to_hum t) in
    Lwt.fail (Invalid_argument err)

  let of_branch_id task t repo =
    if Ref_store.Key.is_valid t then of_ref repo task (`Branch t)
    else err_invalid_branch_id t

  let master task repo =
    of_branch_id task Ref_store.Key.master repo

  let empty task repo =
    of_ref repo task (`Head (ref None))

  let of_commit_id task key repo =
    of_ref repo task (`Head (ref (Some key)))

  let read_head_commit t =
    Log.debug (fun f -> f "read_head_commit");
    match head_ref t with
    | `Head key -> return (Some key)
    | `Empty    -> Lwt.return_none
    | `Branch name  ->
      Ref_store.read (ref_t t) name >>= function
      | None   -> return_none
      | Some k -> return (Some k)

  let read_head_node t =
    read_head_commit t >>= function
    | None   -> return_none
    | Some h -> H.node (history_t t) h >|= fun n -> Some n

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  (* Retry an operation until the optimistic lock is happy. *)
  let retry name fn =
    let rec aux i =
      fn () >>= function
      | true  -> Lwt.return_unit
      | false ->
        Log.debug (fun f -> f "Irmin.%s: conflict, retrying (%d)." name i);
        aux (i+1)
    in
    aux 1

  let with_commit t path ~f =
    let aux () =
      read_head_commit t >>= fun commit ->
      begin match commit with
        | None   -> Graph.empty (graph_t t)
        | Some h -> H.node (history_t t) h
      end >>= fun old_node ->
      f old_node >>= fun node ->
      let parents = parents_of_commit commit in
      H.create (history_t t) ~node ~parents ~task:(task t) >>= fun key ->
      match t.head_ref with
      | `Head head ->
        (* [head] is protected by [t.lock] *)
        head := Some key; Lwt.return true
      | `Branch name   ->
        (* concurrent handle and/or process can modify the branch. Need to check
           that we are still working on the same head. *)
        Ref_store.compare_and_set (ref_t t) name ~test:commit ~set:(Some key)
    in
    let msg = Printf.sprintf "with_commit(%s)" (Tc.show (module Key) path) in
    Lwt_mutex.with_lock t.lock (fun () -> retry msg aux)

  let map t path ~f =
    Log.debug (fun f -> f "map %a" (show (module Key)) path);
    begin read_head_node t >>= function
      | None   -> Graph.empty (graph_t t)
      | Some n -> return n
    end >>= fun node ->
    f (graph_t t) node path

  let read t path =
    map t path ~f:Graph.read_contents >>= function
    | None   -> return_none
    | Some (c, _meta) -> P.Contents.read (contents_t t) c

  let update t path contents =
    Log.debug (fun f -> f "update %a" (show (module Key)) path);
    P.Contents.add (contents_t t) contents >>= fun contents ->
    with_commit t path ~f:(fun node ->
        Graph.add_contents (graph_t t) node path (contents, Metadata.default)
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
    Log.debug (fun f -> f "read_exn %a" (show (module Key)) path);
    map t path ~f:Graph.read_contents_exn >>= fun (c, _meta) ->
    P.Contents.read_exn (contents_t t) c

  let mem t path =
    map t path ~f:Graph.mem_contents

  let compare_and_set _ = failwith "Irmin.compare_and_set: TODO"

  (* Return the subpaths. *)
  let list_node t n path =
    Graph.read_node (graph_t t) n path >>= function
    | None      -> return_nil
    | Some node ->
      Graph.steps (graph_t t) node >>= fun steps ->
      let paths = List.map (fun c -> Key.rcons path c) steps in
      return paths

  let list t path =
    Log.debug (fun f -> f "list");
    read_head_node t >>= function
    | None   -> return_nil
    | Some n -> list_node t n path

  let iter t fn =
    Log.debug (fun f -> f "iter");
    head t >>= function
    | None   -> Lwt.return_unit
    | Some h ->
      (* we avoid races here by freezing the store head. *)
      of_commit_id (fun () -> t.task) h t.repo >>= fun t ->
      let t = t () in
      let rec aux acc = function
        | []       -> Lwt_list.iter_p (fun (path, v) -> fn path v) acc
        | path::tl ->
          list t path >>= fun childs ->
          let todo = childs @ tl in
          mem t path >>= fun exists ->
          if not exists then aux acc todo
          else aux ((path, fun () -> read_exn t path) :: acc) todo
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

  let lcas_branch t ?max_depth ?n other =
    head_exn t >>= fun h ->
    head_exn { t with head_ref = `Branch other } >>= fun head ->
    H.lcas (history_t t) ?max_depth ?n h head

  (* Merge two commits:
     - Search for common ancestors
     - Perform recursive 3-way merges *)
  let three_way_merge t ?max_depth ?n c1 c2 =
    H.three_way_merge (history_t t) ?max_depth ?n c1 c2

  let update_head t c =
    match t.head_ref with
    | `Head h  -> h := Some c; return_unit
    | `Branch name -> Ref_store.update (ref_t t) name c

  let update_branch t name =
    Ref_store.read_exn (ref_t t) name >>= fun k ->
    update_head t k

  let compare_and_set_head_unsafe t ~test ~set =
    match t.head_ref with
    | `Head head ->
      (* [head] is protected by [t.lock]. *)
      if !head = test then (head := set; Lwt.return true)
      else Lwt.return false
    | `Branch name -> Ref_store.compare_and_set (ref_t t) name ~test ~set

  let compare_and_set_head t ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        compare_and_set_head_unsafe t ~test ~set
      )

  let fast_forward_head t ?max_depth ?n new_head =
    head t >>= function
    | None  -> compare_and_set_head t ~test:None ~set:(Some new_head)
    | Some old_head ->
      let pp = show (module Hash) in
      Log.debug (fun f -> f "fast-forward-head old=%a new=%a"
        pp old_head pp new_head);
      if Hash.equal new_head old_head then
        (* we only update if there is a change *)
        Lwt.return_false
      else
        H.lcas (history_t t) ?max_depth ?n new_head old_head >>= function
        | `Ok [x] when Hash.equal x old_head ->
          (* we only update if new_head > old_head *)
          compare_and_set_head t ~test:(Some old_head) ~set:(Some new_head)
        | `Too_many_lcas -> Log.debug (fun f -> f "ff: too many LCAs"); Lwt.return false
        | `Max_depth_reached -> Log.debug (fun f -> f "ff: max depth reached"); Lwt.return false
        | `Ok _ -> Lwt.return false

  let retry_merge name fn =
    let rec aux i =
      fn () >>= function
      | `Conflict _ as c -> Lwt.return c
      | `Ok true  -> ok ()
      | `Ok false ->
        Log.debug (fun f -> f "Irmin.%s: conflict, retrying (%d)." name i);
        aux (i+1)
    in
    aux 1

  module Private = struct
    include P

    let read_node t path =
      read_head_node t >>= function
      | None   -> return_none
      | Some n -> Graph.read_node (graph_t t) n path

    let mem_node t path =
      read_head_node t >>= function
      | None   -> return false
      | Some n -> Graph.mem_node (graph_t t) n path

    let remove_node t path =
      with_commit t path ~f:(fun h -> Graph.remove_node (graph_t t) h path)

    let iter_node t node fn =
      Log.debug (fun f -> f "iter");
      let rec aux acc = function
        | []       -> Lwt_list.iter_p (fun (path, v) -> fn path v) acc
        | path::tl ->
          list_node t node path >>= fun childs ->
          let todo = childs @ tl in
          Graph.mem_contents (graph_t t) node path >>= fun exists ->
          if not exists then aux acc todo
          else
            let value () =
              Graph.read_contents (graph_t t) node path >>= function
              | None   -> Lwt.fail (Failure "iter_node")
              | Some (v, _meta) -> P.Contents.read_exn (contents_t t) v
            in
            aux ((path, value) :: acc) todo
      in
      list_node t node Key.empty >>= aux []

    let update_node t path node =
      with_commit t path ~f:(fun h -> Graph.add_node (graph_t t) h path node)

    let merge_node t path (parent, node) =
      Log.debug (fun f -> f "merge_node");
      let empty () = Graph.empty (graph_t t) in
      let node_of_commit_id head =
        begin match head with
          | None   -> Lwt.return_none
          | Some h -> H.node (history_t t) h >|= fun n -> Some n
        end
        >>= function
        | None   -> empty () >|= fun empty -> empty, None
        | Some h -> Graph.read_node (graph_t t) h path >|= fun n -> h, n
      in
      let parent_node () =
        node_of_commit_id (Some parent) >>= fun (_, x) -> ok (Some x)
      in
      let aux () =
        read_head_commit t >>= fun head ->
        node_of_commit_id head >>= fun (current_root, current_node) ->
        P.Node.merge path (graph_t t) ~old:parent_node current_node (Some node)
        >>| fun new_node ->
        begin match new_node with
          | None   -> Graph.remove_node (graph_t t) current_root path
          | Some n -> Graph.add_node (graph_t t) current_root path n
        end >>= fun new_root ->
        let parents =
          let aux = function None -> [] | Some x -> [x] in
          parent :: aux head
        in
        H.create (history_t t) ~node:new_root ~parents ~task:(task t) >>= fun h ->
        compare_and_set_head_unsafe t ~test:head ~set:(Some h) >>=
        ok
      in
      Lwt_mutex.with_lock t.lock (fun () -> retry_merge "merge_node" aux)

  end

  (* FIXME: we might want to keep the new commit in case of conflict,
     and use it as a base for the next merge. *)
  let merge_head t ?max_depth ?n c1 =
    Log.debug (fun f -> f "merge_head");
    let aux () =
      read_head_commit t >>= fun head ->
      match head with
      | None    ->
        compare_and_set_head_unsafe t ~test:head ~set:(Some c1) >>=
        ok
      | Some c2 ->
        three_way_merge t ~task:t.task ?max_depth ?n c1 c2 >>| fun c3 ->
        compare_and_set_head_unsafe t ~test:head ~set:(Some c3) >>=
        ok
    in
    Lwt_mutex.with_lock t.lock (fun () -> retry_merge "merge_head" aux)

  let merge_head_exn t ?max_depth ?n c1 =
    merge_head t ?max_depth ?n c1 >>= Ir_merge.exn

  let clone_force task t branch_id =
    Log.debug (fun f -> f "clone_force %a" (show (module Ref_store.Key)) branch_id);
    let return () = of_branch_id task branch_id t.repo in
    head t >>= function
    | None   -> return ()
    | Some h -> Ref_store.update (ref_t t) branch_id h >>= return

  let clone task t branch_id =
    Log.debug (fun f -> f "clone %a" (show (module Ref_store.Key)) branch_id);
    Ref_store.mem (ref_t t) branch_id >>= function
    | true  -> Lwt.return `Duplicated_branch
    | false ->
      let return () = of_branch_id task branch_id t.repo >|= fun t -> `Ok t in
      head t >>= function
      | None   -> Lwt.return `Empty_head
      | Some h ->
        Ref_store.compare_and_set (ref_t t) branch_id ~test:None ~set:(Some h) >>= function
        | true  -> return ()
        | false -> Lwt.return `Duplicated_branch

  let merge_branch t ?max_depth ?n other =
    Log.debug (fun f -> f "merge_branch %a" (show (module Ref_store.Key)) other);
    Ref_store.read (ref_t t) other >>= function
    | None  ->
      let str =
        Printf.sprintf "merge_branch: %s is not a valid branch ID" (Ref_store.Key.to_hum other)
      in
      Lwt.fail (Failure str)
    | Some c -> merge_head t ?max_depth ?n c

  let merge_branch_exn t ?max_depth ?n other =
    merge_branch t ?max_depth ?n other >>= Ir_merge.exn

  let merge a ?max_depth ?n t ~into =
    Log.debug (fun f -> f "merge");
    let t = t a and into = into a in
    match head_ref t with
    | `Branch name -> merge_branch into ?max_depth ?n name
    | `Head h  -> merge_head into ?max_depth ?n h
    | `Empty   -> ok ()

  let merge_exn a ?max_depth ?n t ~into =
    merge a ?max_depth ?n t ~into >>= Ir_merge.exn

  let watch_head t ?init fn =
    name t >>= function
    | None       ->
      (* FIXME: start a local watcher on the detached branch *)
      Lwt.return (fun () -> Lwt.return_unit)
    | Some name0 ->
      let init = match init with
        | None       -> None
        | Some head0 -> Some [name0, head0]
      in
      Ref_store.watch (ref_t t) ?init (fun name head ->
          if Ref_store.Key.equal name0 name then fn head else Lwt.return_unit
        ) >>= fun id ->
      Lwt.return (fun () -> Ref_store.unwatch (ref_t t) id)

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
      assert (not (Hash.equal x y));
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
    Log.info (fun f -> f "watch-key %a" (show (module Key)) key);
    let init_head = match init with
      | None        -> None
      | Some (h, _) -> Some h
    in
    let value_of_head h =
      of_commit_id (fun () -> task t) h (repo t) >>= fun t ->
      read (t ()) key
    in
    watch_head t ?init:init_head (lift value_of_head fn)

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

  let history ?depth ?(min=[]) ?(max=[]) t =
    Log.debug (fun f -> f "history");
    let pred = function
      | `Commit k ->
        H.parents (history_t t) k >>= fun parents ->
        return (List.map (fun x -> `Commit x) parents)
      | _ -> return_nil in
    begin head t >>= function
      | Some h -> Lwt.return [h]
      | None   -> Lwt.return max
    end >>= fun max ->
    let max = List.map (fun k -> `Commit k) max in
    let min = List.map (fun k -> `Commit k) min in
    KGraph.closure ?depth ~min ~max ~pred () >>= fun g ->
    let h = Gmap.filter_map (function `Commit k -> Some k | _ -> None) g in
    Lwt.return h

  module Ref = P.Ref.Key
end
