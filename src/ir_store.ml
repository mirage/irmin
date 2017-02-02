(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Ir_merge.Infix

let src = Logs.Src.create "irmin" ~doc:"Irmin branch-consistent store"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (P: Ir_s.PRIVATE) = struct

  module Branch_store = P.Branch

  type branch = Branch_store.key

  module Key = P.Node.Path
  type key = Key.t

  module Metadata = P.Node.Metadata
  module Contents = P.Contents.Val
  module Tree = Ir_tree.Make(P)

  type node = Tree.node
  type contents = Contents.t
  type metadata = Metadata.t
  type tree = Tree.tree

  module Hash = P.Commit.Key
  type commit = Hash.t

  type head_ref = [ `Branch of branch | `Head of commit option ref ]

  module OCamlGraph = Graph
  module Graph = Ir_node.Graph(P.Node)
  module H = Ir_commit.History(P.Commit)

  module KGraph = Ir_graph.Make
      (P.Contents.Key)(P.Node.Metadata)(P.Node.Key)(P.Commit.Key)
      (Branch_store.Key)

  type slice = P.Slice.t
  type watch = unit -> unit Lwt.t

  let unwatch w = w ()

  module Repo = struct

    type t = P.Repo.t
    let v = P.Repo.v
    let graph_t t = P.Repo.node_t t
    let history_t t = P.Repo.commit_t t
    let branches t = P.Branch.list (P.Repo.branch_t t)

    let heads t =
      let t = P.Repo.branch_t t in
      Branch_store.list t >>= fun bs ->
      Lwt_list.fold_left_s (fun acc r ->
          Branch_store.find t r >|= function
          | None   -> acc
          | Some h -> h::acc
        ) [] bs

    let export ?(full=true) ?depth ?(min=[]) ?(max=[]) t =
      Log.debug (fun f -> f "export depth=%s full=%b min=%d max=%d"
        (match depth with None -> "<none>" | Some d -> string_of_int d)
        full (List.length min) (List.length max));
      begin match max with
        | [] -> heads t
        | m -> Lwt.return m
      end >>= fun max ->
      P.Slice.empty () >>= fun slice ->
      let max = List.map (fun x -> `Commit x) max in
      let min = List.map (fun x -> `Commit x) min in
      let pred = function
        | `Commit k ->
          H.parents (history_t t) k >|= fun parents ->
          List.map (fun x -> `Commit x) parents
        | _ -> Lwt.return_nil in
      KGraph.closure ?depth ~pred ~min ~max () >>= fun g ->
      let keys =
        List.fold_left (fun acc -> function
            | `Commit c -> c :: acc
            | _ -> acc
          ) [] (KGraph.vertex g)
      in
      let root_nodes = ref [] in
      Lwt_list.iter_p (fun k ->
          P.Commit.find (P.Repo.commit_t t) k >>= function
          | None   -> Lwt.return_unit
          | Some c ->
            root_nodes := P.Commit.Val.node c :: !root_nodes;
            P.Slice.add slice (`Commit (k, c))
        ) keys
      >>= fun () ->
      if not full then
        Lwt.return slice
      else
        (* XXX: we can compute a [min] if needed *)
        Graph.closure (graph_t t) ~min:[] ~max:!root_nodes >>= fun nodes ->
        let module KSet = Set.Make(struct
            type t = P.Contents.key
            let compare = Ir_type.compare P.Contents.Key.t
          end) in
        let contents = ref KSet.empty in
        Lwt_list.iter_p (fun k ->
            P.Node.find (P.Repo.node_t t) k >>= function
            | None   -> Lwt.return_unit
            | Some v ->
              List.iter (function
                  | _, `Contents (c, _) -> contents := KSet.add c !contents
                  | _ -> ()
                ) (P.Node.Val.list v);
              P.Slice.add slice (`Node (k, v))
          ) nodes >>= fun () ->
        Lwt_list.iter_p (fun k ->
            P.Contents.find (P.Repo.contents_t t) k >>= function
            | None   -> Lwt.return_unit
            | Some m -> P.Slice.add slice (`Contents (k, m))
          ) (KSet.elements !contents) >|= fun () ->
        slice

    exception Import_error

    let import t s =
      let aux (type k) (type v)
          name
          (type s)
          (module S: Ir_s.AO with type t = s and type key = k and type value = v)
          (dk: k Ir_type.t)
          fn
          (s:t -> s)
        =
        fn (fun (k, v) ->
            S.add (s t) v >>= fun k' ->
            if not (Ir_type.equal dk k k') then (
              Log.err (fun f -> f "%s import error: expected %a, got %a"
                name Ir_type.(dump dk) k Ir_type.(dump dk) k');
              Lwt.fail Import_error
            )
            else Lwt.return_unit
          )
      in
      let contents = ref [] in
      let nodes = ref [] in
      let commits = ref [] in
      P.Slice.iter s (function
          | `Contents c -> contents := c :: !contents; Lwt.return_unit
          | `Node n     -> nodes := n :: !nodes; Lwt.return_unit
          | `Commit c   -> commits := c :: !commits; Lwt.return_unit
        ) >>= fun () ->
      Lwt.catch (fun () ->
          aux "Contents"
            (module P.Contents) P.Contents.Key.t
            (fun f -> Lwt_list.iter_s f !contents) P.Repo.contents_t
          >>= fun () ->
          aux "Node"
            (module P.Node) P.Node.Key.t
            (fun f -> Lwt_list.iter_s f !nodes) P.Repo.node_t
          >>= fun () ->
          aux "Commit"
            (module P.Commit) P.Commit.Key.t
            (fun f -> Lwt_list.iter_s f !commits) P.Repo.commit_t
          >>= fun () ->
          Lwt.return `Ok)
        (function Import_error -> Lwt.return `Error | e -> Lwt.fail e)

    let task_of_commit t commit =
      P.Commit.find (P.Repo.commit_t t) commit >|= function
      | None   -> None
      | Some c -> Some (P.Commit.Val.task c)

  end

  type tree_root = [ `Empty | `Node of node ]

  type t = {
    repo: Repo.t;
    head_ref: head_ref;
    mutable tree: (commit * tree_root) option; (* cache for the store tree *)
    lock: Lwt_mutex.t;
  }

  type step = Key.step
  let repo t = t.repo
  let branch_t t = P.Repo.branch_t t.repo
  let commit_t t = P.Repo.commit_t t.repo
  let node_t t = P.Repo.node_t t.repo
  let graph_t t = node_t t
  let history_t t = commit_t t

  let status t = match t.head_ref with
    | `Branch b -> `Branch b
    | `Head h   -> match ! h with
      | None   -> `Empty
      | Some c -> `Commit c

  let head_ref t = match t.head_ref with
    | `Branch t -> `Branch t
    | `Head h -> match !h with None -> `Empty | Some h -> `Head h

  let branch t = match head_ref t with
    | `Branch t  -> Lwt.return (Some t)
    | `Empty
    | `Head _ -> Lwt.return_none

  let err_no_head s = Fmt.kstrf Lwt.fail_invalid_arg "Irmin.%s: no head" s

  let retry_merge name fn =
    let rec aux i =
      fn () >>= function
      | Error _ as c -> Lwt.return c
      | Ok true      -> Ir_merge.ok ()
      | Ok false     ->
        Log.debug (fun f -> f "Irmin.%s: conflict, retrying (%d)." name i);
        aux (i+1)
    in
    aux 1

  let of_ref repo head_ref =
    let lock = Lwt_mutex.create () in
    Lwt.return { repo = repo; lock; head_ref; tree = None }

  let err_invalid_branch t =
    let err = Fmt.strf "%a is not a valid branch name." Branch_store.Key.pp t in
    Lwt.fail (Invalid_argument err)

  let of_branch repo id =
    if Branch_store.Key.is_valid id then of_ref repo (`Branch id)
    else err_invalid_branch id

  let master repo = of_branch repo Branch_store.Key.master
  let empty repo = of_ref repo (`Head (ref None))
  let of_commit repo id = of_ref repo (`Head (ref (Some id)))

  let lift value_of_head fn = function
    | `Removed x ->
      value_of_head x >>= fun v ->
      fn @@ `Removed (x, v)
    | `Added x ->
      value_of_head x >>= fun v ->
      fn @@ `Added (x, v)
    | `Updated (x, y) ->
      assert (not (Ir_type.equal Hash.t x y));
      value_of_head x >>= fun vx ->
      value_of_head y >>= fun vy ->
      match vx, vy with
      | `Empty, `Empty -> Lwt.return_unit
      | `Empty, _      -> fn @@ `Added (y, vy)
      | _     , `Empty -> fn @@ `Removed (x, vx)
      | _ ->
        Tree.equal vx vy >>= function
        | true  -> Lwt.return_unit
        | false -> fn @@ `Updated ( (x, vx), (y, vy) )

  let head t =
    let h = match head_ref t with
      | `Head key    -> Lwt.return (Some key)
      | `Empty       -> Lwt.return_none
      | `Branch name -> Branch_store.find (branch_t t) name
    in
    h >|= fun h ->
    Log.debug (fun f -> f "Head.find -> %a" Fmt.(option Hash.pp) h);
    h

  let node t h = H.node (history_t t) h

  let tree_and_head t =
    head t >>= function
    | None   -> Lwt.return None
    | Some h ->
      match t.tree with
      | Some (o, t) when Ir_type.equal Hash.t o h -> Lwt.return @@ Some (o, t)
      | _   ->
        t.tree <- None;
        (* the tree cache needs to be invalidated *)
        (node t h >>= function
          | None   -> Lwt.return `Empty
          | Some n -> Tree.import (repo t) n >|= fun n -> `Node n
        ) >|= fun tree ->
        t.tree <- Some (h, tree);
        Some (h, tree)

  let tree t =
    tree_and_head t >|= function
    | None           -> `Empty
    | Some (_, tree) -> (tree :> tree)

  let watch t ?init fn =
    branch t >>= function
    | None       -> failwith "watch a detached head: TODO"
    | Some name0 ->
      let init = match init with
        | None       -> None
        | Some head0 -> Some [name0, head0]
      in
      Branch_store.watch (branch_t t) ?init (fun name head ->
          if Ir_type.equal Branch_store.Key.t name0 name
          then fn head else Lwt.return_unit
        ) >|= fun id ->
      fun () -> Branch_store.unwatch (branch_t t) id

  let watch_key t key ?init fn =
    Log.info (fun f -> f "watch-key %a" Key.pp key);
    let value_of_head h =
      of_commit (repo t) h >>= fun t ->
      tree t >>= fun tree ->
      Tree.getv (tree :> tree) key
    in
    watch t ?init (lift value_of_head fn)

  module Head = struct

    let list = Repo.heads
    let find = head

    let v repo task ~parents tree =
      (match tree with
      | `Empty      -> P.Node.add (P.Repo.node_t repo) P.Node.Val.empty
      | `Contents _ -> failwith "cannot commit a top-level tree"
      | `Node t     -> Tree.export repo t)
      >>= fun node ->
      let c = P.Commit.Val.v task ~node ~parents in
      P.Commit.add (P.Repo.commit_t repo) c

    let get t = find t >>= function
      | None   -> err_no_head "head"
      | Some k -> Lwt.return k

    let set t c =
      match t.head_ref with
      | `Head h      -> h := Some c; Lwt.return_unit
      | `Branch name -> Branch_store.set (branch_t t) name c

    let test_and_set_unsafe t ~test ~set =
      match t.head_ref with
      | `Head head ->
        (* [head] is protected by [t.lock]. *)
        if !head = test then (head := set; Lwt.return true)
        else Lwt.return false
      | `Branch name -> Branch_store.test_and_set (branch_t t) name ~test ~set

    let test_and_set t ~test ~set =
      Lwt_mutex.with_lock t.lock (fun () ->
          test_and_set_unsafe t ~test ~set
        )

    let fast_forward t ?max_depth ?n new_head =
      find t >>= function
      | None  -> test_and_set t ~test:None ~set:(Some new_head)
      | Some old_head ->
        Log.debug (fun f -> f "fast-forward-head old=%a new=%a"
                      Hash.pp old_head Hash.pp new_head);
        if Ir_type.equal Hash.t new_head old_head then
          (* we only update if there is a change *)
          Lwt.return_false
        else
          H.lcas (history_t t) ?max_depth ?n new_head old_head >>= function
          | `Ok [x] when Ir_type.equal Hash.t x old_head ->
            (* we only update if new_head > old_head *)
            test_and_set t ~test:(Some old_head) ~set:(Some new_head)
          | `Too_many_lcas ->
            Log.debug (fun f -> f "ff: too many LCAs");
            Lwt.return false
          | `Max_depth_reached ->
            Log.debug (fun f -> f "ff: max depth reached");
            Lwt.return false
          | `Ok _ -> Lwt.return false

    (* Merge two commits:
       - Search for common ancestors
       - Perform recursive 3-way merges *)
    let three_way_merge t ?max_depth ?n c1 c2 =
      H.three_way_merge (history_t t) ?max_depth ?n c1 c2

    (* FIXME: we might want to keep the new commit in case of conflict,
         and use it as a base for the next merge. *)
    let merge ~into:t task ?max_depth ?n c1 =
      Log.debug (fun f -> f "merge_head");
      let aux () =
        head t >>= fun head ->
        match head with
        | None    ->
          test_and_set_unsafe t ~test:head ~set:(Some c1) >>=
          Ir_merge.ok
        | Some c2 ->
          three_way_merge t ~task ?max_depth ?n c1 c2 >>| fun c3 ->
          test_and_set_unsafe t ~test:head ~set:(Some c3) >>=
          Ir_merge.ok
      in
      Lwt_mutex.with_lock t.lock (fun () -> retry_merge "merge_head" aux)

    let parents t =
      find t >>= function
      | None   -> Lwt.return []
      | Some h -> H.parents (history_t t) h

  end

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

  let tree_root = function
    | `Empty | `Node _ as n -> n
    | `Contents _ -> assert false

  let with_commit t task ?parents ~msg (f: tree -> tree Lwt.t) =
    let aux () =
      (tree_and_head t >|= function
        | None           -> None, `Empty, []
        | Some (c, tree) ->
          let parents = match parents with None -> [c] | Some p -> p in
          Some c, tree, parents
      ) >>= fun (head, tree, parents) ->
      f (tree :> tree) >>= fun tree ->
      (match tree with
        | `Empty      -> Graph.empty (graph_t t)
        | `Node n     -> Tree.export (repo t) n
        | `Contents _ -> Lwt.fail_invalid_arg "The tree root cannot be contents"
      ) >>= fun node ->
      H.v (history_t t) ~node ~parents ~task >>= fun key ->
      match t.head_ref with
      | `Head head ->
        (* [head] is protected by [t.lock] *)
        head := Some key;
        t.tree <- Some (key, tree_root tree);
        Lwt.return true
      | `Branch name   ->
        (* concurrent handle and/or process can modify the branch. Need to check
           that we are still working on the same head. *)
        Branch_store.test_and_set (branch_t t) name ~test:head ~set:(Some key)
        >>= fun r ->
        if r then t.tree <- Some (key, tree_root tree);
        Lwt.return r
    in
    Lwt_mutex.with_lock t.lock (fun () -> retry msg aux)

  let mem t k =
    tree t >>= fun tree ->
    Tree.mem tree k

  let memv t k =
    tree t >>= fun tree ->
    Tree.memv tree k

  let findm t k =
    tree t >>= fun tree ->
    Tree.findm tree k

  let find t k =
    tree t >>= fun tree ->
    Tree.find tree k

  let get t k =
    tree t >>= fun tree ->
    Tree.get tree k

  let getv t k =
    tree t >>= fun tree ->
    Tree.getv tree k

  let getm t k =
    tree t >>= fun tree ->
    Tree.getm tree k

  let list t k =
    tree t >>= fun tree ->
    Tree.list tree k

  let kind t k =
    tree t >>= fun tree ->
    Tree.kind tree k

  let setv t task ?parents path (v:tree) =
    Log.debug (fun f -> f "setv %a" Key.pp path);
    with_commit t task ?parents ~msg:"setv" (fun tree ->
        Tree.addv tree path v
      )

  let set t task ?parents path ?metadata (c:contents) =
    Log.debug (fun f -> f "set %a" Key.pp path);
    with_commit t task ?parents ~msg:"set" (fun tree ->
        Tree.add tree path ?metadata c
      )

  let remove t task path =
    Log.debug (fun f -> f "remove %a" Key.pp path);
    with_commit t task ~msg:"remove" (fun tree ->
        Tree.remove tree path
      )

  let clone ~src ~dst =
    (Head.find src >>= function
      | None   -> Branch_store.remove (branch_t src) dst
      | Some h -> Branch_store.set (branch_t src) dst h
    ) >>= fun () ->
    of_branch (repo src) dst

  let lcas ?max_depth ?n t1 t2 =
    Head.get t1 >>= fun h1 ->
    Head.get t2 >>= fun h2 ->
    H.lcas (history_t t1) ?max_depth ?n h1 h2

  let lcas_with_commit t ?max_depth ?n c =
    Head.get t >>= fun h ->
    H.lcas (history_t t) ?max_depth ?n h c

  let lcas_with_branch t ?max_depth ?n b =
    Head.get t >>= fun h ->
    Head.get { t with head_ref = `Branch b } >>= fun head ->
    H.lcas (history_t t) ?max_depth ?n h head

  module Private = struct
    include P
    let import_node = Tree.import
    let export_node = Tree.export
  end

  let merge_with_branch t task ?max_depth ?n other =
    Log.debug (fun f -> f "merge_with_branch %a" Branch_store.Key.pp other);
    Branch_store.find (branch_t t) other >>= function
    | None  ->
      let str =
        Fmt.strf "merge_with_branch: %a is not a valid branch ID"
          Branch_store.Key.pp other
      in
      Lwt.fail (Failure str)
    | Some c -> Head.merge ~into:t task ?max_depth ?n c

  let merge_with_commit t task ?max_depth ?n other =
    Head.merge ~into:t task ?max_depth ?n other

  let merge ~into task ?max_depth ?n t =
    Log.debug (fun l -> l "merge");
    match head_ref t with
    | `Branch name -> merge_with_branch into task ?max_depth ?n name
    | `Head h      -> merge_with_commit into task ?max_depth ?n h
    | `Empty       -> Ir_merge.ok ()

  exception Conflict of Ir_merge.conflict

  let mergev t task ~parents ?max_depth ?n path tree1 =
    Log.debug (fun l -> l "mergev");
    let old () =
      H.lca (history_t t) ~task ?max_depth ?n parents >>= function
      | Error _ as e -> Lwt.return e
      | Ok None     -> Lwt.return (Ok None)
      | Ok (Some h) ->
        of_commit (repo t) h >>= fun t ->
        getv t path >|= fun tree ->
        Ok (Some tree)
    in
    Lwt.catch
      (fun () ->
         with_commit t task ~parents ~msg:"mergev" (fun root ->
             Tree.getv root path >>= fun tree2 ->
             (Ir_merge.(f Tree.merge) ~old tree1 tree2 >>= function
               | Ok tree -> Lwt.return tree
               | Error e -> Lwt.fail (Conflict e))
             >>= fun tree_m ->
             Tree.addv root path tree_m
           ) >>= Ir_merge.ok
      ) (function
          | Conflict e -> Lwt.return (Error e)
          | e -> Lwt.fail e)

  module History =
    OCamlGraph.Persistent.Digraph.ConcreteBidirectional(struct
      type t = P.Commit.key
      let hash h = Hashtbl.hash (P.Commit.Key.to_raw h)
      let compare = Ir_type.compare P.Commit.Key.t
      let equal = Ir_type.equal P.Commit.Key.t
    end)

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
        H.parents (history_t t) k >|= fun parents ->
        List.map (fun x -> `Commit x) parents
      | _ -> Lwt.return_nil in
    begin Head.find t >>= function
      | Some h -> Lwt.return [h]
      | None   -> Lwt.return max
    end >>= fun max ->
    let max = List.map (fun k -> `Commit k) max in
    let min = List.map (fun k -> `Commit k) min in
    KGraph.closure ?depth ~min ~max ~pred () >>= fun g ->
    let h = Gmap.filter_map (function `Commit k -> Some k | _ -> None) g in
    Lwt.return h

  module Branch = struct

    include P.Branch.Key

    let mem t = P.Branch.mem (P.Repo.branch_t t)
    let find t = P.Branch.find (P.Repo.branch_t t)
    let set t = P.Branch.set (P.Repo.branch_t t)
    let remove t = P.Branch.remove (P.Repo.branch_t t)
    let list = Repo.branches

    let watch t k ?init f =
      P.Branch.watch_key (P.Repo.branch_t t) k ?init f >|= fun w ->
      (fun () -> Branch_store.unwatch (P.Repo.branch_t t) w)

    let watch_all t ?init f =
      P.Branch.watch (P.Repo.branch_t t) ?init f >|= fun w ->
      (fun () -> Branch_store.unwatch (P.Repo.branch_t t) w)

    let err_not_found k =
      Fmt.kstrf invalid_arg "Branch.get: %a not found" P.Branch.Key.pp k

    let get t k =
      find t k >>= function
      | None   -> err_not_found k
      | Some v -> Lwt.return v

  end

  module Commit = Hash
  module Status = struct
    type t = [ `Empty | `Branch of branch | `Commit of commit ]
    let t =
      let open Ir_type in
      variant "status" (fun empty branch commit -> function
          | `Empty    -> empty
          | `Branch b -> branch b
          | `Commit c -> commit c)
      |~ case0 "`Empty" `Empty
      |~ case1 "`Branch" Branch.t (fun b -> `Branch b)
      |~ case1 "`Commit" Commit.t (fun c -> `Commit c)
      |> sealv

    let pp ppf x = Ir_type.pp_json t ppf x

    let of_string str =
      match Ir_type.decode_json t (Jsonm.decoder (`String str)) with
      | Ok t    -> `Ok t
      | Error e -> `Error e

  end

  let slice_t = P.Slice.t
  let tree_t = Tree.tree_t
  let contents_t = Contents.t
  let metadata_t = Metadata.t
  let key_t = Key.t
  let step_t = Key.step_t
  let node_t = Tree.node_t
  let commit_t = Commit.t
  let branch_t = Branch.t
  let kind_t = Ir_type.enum "kind" [ "contents", `Contents; "node", `Node ]
  let kinde_t =
    Ir_type.enum "kinde" [ "empty", `Empty; "contents", `Contents; "node", `Node ]
  let lca_t =
    let open Ir_type in
    variant "lca" (fun ok mdr tml -> function
        | `Ok l              -> ok l
        | `Max_depth_reached -> mdr
        | `Too_many_lcas     -> tml)
    |~ case1 "ok" (list commit_t) (fun l -> `Ok l)
    |~ case0 "max-depth-reached" `Max_depth_reached
    |~ case0 "too-many-lcas" `Too_many_lcas
    |> sealv
end
