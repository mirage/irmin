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
open Merge.Infix

let src = Logs.Src.create "irmin" ~doc:"Irmin branch-consistent store"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (P: S.PRIVATE) = struct

  module Branch_store = P.Branch
  type branch = Branch_store.key

  module Hash = P.Hash
  type hash = Hash.t

  type lca_error = [`Max_depth_reached | `Too_many_lcas]
  type ff_error = [`No_change | `Rejected | lca_error]

  module Key = P.Node.Path
  type key = Key.t

  module Metadata = P.Node.Metadata
  module H = Commit.History(P.Commit)

  type S.remote += E of P.Sync.endpoint

  module Contents = struct
    include P.Contents.Val
    let of_hash r h = P.Contents.find (P.Repo.contents_t r) h
    let hash c =
      let s = Type.encode_bin P.Contents.Val.t c in
      P.Hash.digest s
  end

  module Tree = struct
    include Tree.Make(P)

    let of_hash r h = Lwt.return (Some (`Node (import r h)))

    let hash: tree -> hash = fun tr -> match hash tr with
      | `Node h -> h
      | `Contents (h, _) -> h
  end

  type node = Tree.node
  type contents = Contents.t
  type metadata = Metadata.t
  type tree = Tree.tree
  type repo = P.Repo.t

  module Commit = struct

    type t = { r: repo; h: Hash.t; v: P.Commit.value }

    let t r =
      let open Type in
      record "commit" (fun h v -> { r; h; v })
      |+ field "hash"  Hash.t         (fun t -> t.h)
      |+ field "value" P.Commit.Val.t (fun t -> t.v)
      |> sealr

    let compare_hash = Type.compare Hash.t

    let v r ~info ~parents tree =
      P.Repo.batch r @@ fun contents_t node_t commit_t ->
      let parents = List.rev_map (fun c -> c.h) parents in
      let parents = List.sort compare_hash parents in
      (match tree with
       | `Node n     -> Tree.export r contents_t node_t n
       | `Contents _ -> Lwt.fail_invalid_arg "cannot add contents at the root")
      >>= fun node ->
      let v = P.Commit.Val.v ~info ~node ~parents in
      P.Commit.add commit_t v >|= fun h ->
      { r; h; v }

    let node t = P.Commit.Val.node t.v
    let tree t = Tree.import t.r (node t) |> fun n -> `Node n |> Lwt.return
    let equal x y = Type.equal Hash.t x.h y.h
    let hash t = t.h
    let info t = P.Commit.Val.info t.v

    let of_hash r h =
      P.Commit.find (P.Repo.commit_t r) h >|= function
      | None   -> None
      | Some v -> Some { r; h; v }

    let parents t =
      Lwt_list.filter_map_p (of_hash t.r) (P.Commit.Val.parents t.v)

    let pp_hash ppf t = Type.pp Hash.t ppf t.h

    let equal_opt x y =
      match x, y with
      | None  , None   -> true
      | Some x, Some y -> equal x y
      | _ -> false

  end

  type commit = Commit.t

  type head_ref = [ `Branch of branch | `Head of commit option ref ]

  module OCamlGraph = Graph
  module Graph = Node.Graph(P.Node)

  module KGraph = Object_graph.Make
      (P.Contents.Key)(P.Node.Metadata)(P.Node.Key)(P.Commit.Key)
      (Branch_store.Key)

  type slice = P.Slice.t
  type watch = unit -> unit Lwt.t

  let unwatch w = w ()

  module Repo = struct

    type t = repo
    let v = P.Repo.v
    let graph_t t = P.Repo.node_t t
    let history_t t = P.Repo.commit_t t
    let branch_t t = P.Repo.branch_t t
    let commit_t t = P.Repo.commit_t t
    let node_t t = P.Repo.node_t t
    let contents_t t = P.Repo.contents_t t
    let branches t = P.Branch.list (branch_t t)

    let heads repo =
      let t = branch_t repo in
      Branch_store.list t >>= fun bs ->
      Lwt_list.fold_left_s (fun acc r ->
          Branch_store.find t r >>= function
          | None   -> Lwt.return acc
          | Some h ->
            Commit.of_hash repo h >|= function
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
      let max = List.map (fun x -> `Commit x.Commit.h) max in
      let min = List.map (fun x -> `Commit x.Commit.h) min in
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
          P.Commit.find (commit_t t) k >>= function
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
            let compare = Type.compare P.Contents.Key.t
          end) in
        let contents = ref KSet.empty in
        Lwt_list.iter_p (fun k ->
            P.Node.find (node_t t) k >>= function
            | None   -> Lwt.return_unit
            | Some v ->
              List.iter (function
                  | _, `Contents (c, _) -> contents := KSet.add c !contents
                  | _ -> ()
                ) (P.Node.Val.list v);
              P.Slice.add slice (`Node (k, v))
          ) nodes >>= fun () ->
        Lwt_list.iter_p (fun k ->
            P.Contents.find (contents_t t) k >>= function
            | None   -> Lwt.return_unit
            | Some m -> P.Slice.add slice (`Contents (k, m))
          ) (KSet.elements !contents) >|= fun () ->
        slice

    exception Import_error of string
    let import_error fmt = Fmt.kstrf (fun x -> Lwt.fail (Import_error x)) fmt

    let import t s =
      let aux name add dk (k, v) =
        add v >>= fun k' ->
        if not (Type.equal dk k k') then (
          import_error "%s import error: expected %a, got %a"
            name Type.(pp dk) k Type.(pp dk) k'
        )
        else Lwt.return_unit
      in
      let contents = ref [] in
      let nodes = ref [] in
      let commits = ref [] in
      P.Slice.iter s (function
          | `Contents c -> contents := c :: !contents; Lwt.return_unit
          | `Node n     -> nodes := n :: !nodes; Lwt.return_unit
          | `Commit c   -> commits := c :: !commits; Lwt.return_unit
        ) >>= fun () ->
      P.Repo.batch t @@ fun contents_t node_t commit_t ->
      Lwt.catch (fun () ->
          Lwt_list.iter_p
            (aux "Contents" (P.Contents.add contents_t) P.Contents.Key.t)
            !contents
          >>= fun () ->
          Lwt_list.iter_p
            (aux "Node" (P.Node.add node_t) P.Node.Key.t)
            !nodes
          >>= fun () ->
          Lwt_list.iter_p
            (aux "Commit" (P.Commit.add commit_t) P.Commit.Key.t)
            !commits
          >|= fun () ->
          Ok ())
        (function
          | Import_error e -> Lwt.return (Error (`Msg e))
          | e -> Fmt.kstrf Lwt.fail_invalid_arg "impot error: %a" Fmt.exn e)

  end

  type root_tree = [ `Node of node ]

  type t = {
    repo: Repo.t;
    head_ref: head_ref;
    mutable tree: (commit * root_tree) option;    (* cache for the store tree *)
    lock: Lwt_mutex.t;
  }

  type step = Key.step
  let repo t = t.repo
  let branch_t t = Repo.branch_t t.repo
  let commit_t t = Repo.commit_t t.repo
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
      | Ok true      -> Merge.ok ()
      | Ok false     ->
        Log.debug (fun f -> f "Irmin.%s: conflict, retrying (%d)." name i);
        aux (i+1)
    in
    aux 1

  let of_ref repo head_ref =
    let lock = Lwt_mutex.create () in
    Lwt.return {
      lock; head_ref;
      repo  = repo;
      tree  = None;
    }

  let pp_branch = Type.pp Branch_store.Key.t

  let err_invalid_branch t =
    let err = Fmt.strf "%a is not a valid branch name." pp_branch t in
    Lwt.fail (Invalid_argument err)

  let of_branch repo id =
    if Branch_store.Key.is_valid id then of_ref repo (`Branch id)
    else err_invalid_branch id

  let master repo = of_branch repo Branch_store.Key.master
  let empty repo = of_ref repo (`Head (ref None))
  let of_commit c = of_ref c.Commit.r (`Head (ref (Some c)))
  let (>>?) x f = x >>= function None -> Lwt.return_unit | Some x -> f x

  let lift_tree_diff tree fn = function
    | `Removed x -> tree x >>? fun v -> fn @@ `Removed (x, v)
    | `Added x   -> tree x >>? fun v -> fn @@ `Added (x, v)
    | `Updated (x, y) ->
      assert (not (Commit.equal x y));
      tree x >>= fun vx ->
      tree y >>= fun vy ->
      match vx, vy with
      | None    , None    -> Lwt.return_unit
      | None    , Some vy -> fn @@ `Added (y, vy)
      | Some vx, None     -> fn @@ `Removed (x, vx)
      | Some vx, Some vy  ->
        if Tree.equal vx vy then Lwt.return_unit
        else fn @@ `Updated ( (x, vx), (y, vy) )

  let head t =
    let h = match head_ref t with
      | `Head key    -> Lwt.return (Some key)
      | `Empty       -> Lwt.return_none
      | `Branch name ->
        Branch_store.find (branch_t t) name >>= function
        | None   -> Lwt.return_none
        | Some h -> Commit.of_hash t.repo h
    in
    h >|= fun h ->
    Log.debug (fun f -> f "Head.find -> %a" Fmt.(option Commit.pp_hash) h);
    h

  let tree_and_head t =
    head t >|= function
    | None   -> None
    | Some h ->
      match t.tree with
      | Some (o, t) when Commit.equal o h -> Some (o, t)
      | _   ->
        t.tree <- None;
        (* the tree cache needs to be invalidated *)
        let n = Tree.import (repo t) (Commit.node h) in
        let tree = `Node n in
        t.tree <- Some (h, tree);
        Some (h, tree)

  let tree t =
    tree_and_head t >|= function
    | None           -> Tree.empty
    | Some (_, tree) -> (tree :> tree)

  let lift_head_diff repo fn = function
    | `Removed x ->
      (Commit.of_hash repo x >>= function
        | None   -> Lwt.return_unit
        | Some x -> fn (`Removed x))
    |`Updated (x, y) ->
      (Commit.of_hash repo x >>= fun x ->
       Commit.of_hash repo y >>= fun y ->
       match x, y with
       | None  , None   -> Lwt.return_unit
       | Some x, None   -> fn (`Removed x)
       | None  , Some y -> fn (`Added y)
       | Some x, Some y -> fn (`Updated (x, y)))
    |`Added x ->
      (Commit.of_hash repo x >>= function
        | None   -> Lwt.return_unit
        | Some x -> fn (`Added x))

  let watch t ?init fn =
    branch t >>= function
    | None       -> failwith "watch a detached head: TODO"
    | Some name0 ->
      let init = match init with
        | None       -> None
        | Some head0 -> Some [name0, head0.Commit.h]
      in
      Branch_store.watch (branch_t t) ?init (fun name head ->
          if Type.equal Branch_store.Key.t name0 name
          then lift_head_diff t.repo fn head
          else Lwt.return_unit
        ) >|= fun id ->
      fun () -> Branch_store.unwatch (branch_t t) id

  let pp_key = Type.pp Key.t

  let watch_key t key ?init fn =
    Log.info (fun f -> f "watch-key %a" pp_key key);
    let tree c = Commit.tree c >>= fun tree -> Tree.find_tree tree key in
    watch t ?init (lift_tree_diff tree fn)

  module Head = struct

    let list = Repo.heads
    let find = head

    let get t = find t >>= function
      | None   -> err_no_head "head"
      | Some k -> Lwt.return k

    let set t c =
      match t.head_ref with
      | `Head h      -> h := Some c; Lwt.return_unit
      | `Branch name -> Branch_store.set (branch_t t) name c.Commit.h

    let test_and_set_unsafe t ~test ~set =
      match t.head_ref with
      | `Head head ->
        (* [head] is protected by [t.lock]. *)
        if Commit.equal_opt !head test then (head := set; Lwt.return true)
        else Lwt.return false
      | `Branch name ->
        let h = function None -> None | Some c -> Some c.Commit.h in
        Branch_store.test_and_set (branch_t t) name ~test:(h test) ~set:(h set)

    let test_and_set t ~test ~set =
      Lwt_mutex.with_lock t.lock (fun () ->
          test_and_set_unsafe t ~test ~set
        )

    type ff_error = [`Rejected | `No_change | lca_error]

    let fast_forward t ?max_depth ?n new_head =
      let return x = if x then Ok () else Error (`Rejected :> ff_error) in
      find t >>= function
      | None  -> test_and_set t ~test:None ~set:(Some new_head) >|= return
      | Some old_head ->
        Log.debug (fun f -> f "fast-forward-head old=%a new=%a"
                      Commit.pp_hash old_head Commit.pp_hash new_head);
        if Commit.equal new_head old_head then
          (* we only update if there is a change *)
          Lwt.return (Error `No_change)
        else
          H.lcas (history_t t) ?max_depth ?n new_head.Commit.h old_head.Commit.h
          >>= function
          | Ok [x] when Type.equal Hash.t x old_head.Commit.h ->
            (* we only update if new_head > old_head *)
            test_and_set t ~test:(Some old_head) ~set:(Some new_head) >|= return
          | Ok _    -> Lwt.return (Error `Rejected)
          | Error e -> Lwt.return (Error (e :> ff_error))

    (* Merge two commits:
       - Search for common ancestors
       - Perform recursive 3-way merges *)
    let three_way_merge t ?max_depth ?n ~info c1 c2 =
      P.Repo.batch (repo t) @@ fun _ _ commit_t ->
      H.three_way_merge commit_t ?max_depth ?n ~info c1.Commit.h c2.Commit.h

    (* FIXME: we might want to keep the new commit in case of conflict,
         and use it as a base for the next merge. *)
    let merge ~into:t ~info ?max_depth ?n c1 =
      Log.debug (fun f -> f "merge_head");
      let aux () =
        head t >>= fun head ->
        match head with
        | None    ->
          test_and_set_unsafe t ~test:head ~set:(Some c1) >>=
          Merge.ok
        | Some c2 ->
          three_way_merge t ~info ?max_depth ?n c1 c2 >>=* fun c3 ->
          Commit.of_hash t.repo c3 >>= fun c3 ->
          test_and_set_unsafe t ~test:head ~set:c3 >>=
          Merge.ok
      in
      Lwt_mutex.with_lock t.lock (fun () -> retry_merge "merge_head" aux)

  end

  (* Retry an operation until the optimistic lock is happy. Ensure
     that the operation is done at least once. *)
  let retry ~retries fn =
    let done_once = ref false in
    let rec aux i =
      if !done_once && i > retries then
        Lwt.return (Error (`Too_many_retries retries))
      else
        fn () >>= function
        | Ok true  -> Lwt.return (Ok ())
        | Error e  -> Lwt.return (Error e)
        | Ok false -> done_once := true; aux (i+1)
    in
    aux 0

  let root_tree = function
    | `Node _ as n -> n
    | `Contents _  -> assert false

  let add_commit t old_head (c, _ as tree) =
    match t.head_ref with
    | `Head head ->
      Lwt_mutex.with_lock t.lock (fun () ->
          if not (Commit.equal_opt old_head !head) then
            Lwt.return false
          else (
            (* [head] is protected by [t.lock] *)
            head := Some c;
            t.tree <- Some tree;
            Lwt.return true
          ))
    | `Branch name   ->
      (* concurrent handlers and/or process can modify the
         branch. Need to check that we are still working on the same
         head. *)
      let test = match old_head with
        | None   -> None
        | Some c -> Some (Commit.hash c)
      in
      let set = Some (Commit.hash c) in
      Branch_store.test_and_set (branch_t t) name ~test ~set >|= fun r ->
      if r then t.tree <- Some tree;
      r

  type write_error = [
    | Merge.conflict
    | `Too_many_retries of int
    | `Test_was of tree option
  ]

  let pp_write_error ppf = function
    | `Conflict e  -> Fmt.pf ppf "Got a conflict: %s" e
    | `Too_many_retries i ->
      Fmt.pf ppf "Failure after %d attempts to retry the operation: Too many \
                  attempts." i
    | `Test_was t ->
      Fmt.pf ppf "Test-and-set failed: got %a when reading the store"
        Type.(pp (option Tree.tree_t)) t

  let write_error e : ('a, write_error) result Lwt.t = Lwt.return (Error e)
  let err_test v = write_error (`Test_was v)

  type snapshot = {
    head   : commit option;
    root   : tree;
    tree   : tree option; (* the subtree used by the transaction *)
    parents: commit list;
  }

  let snapshot t key =
    tree_and_head t >>= function
    | None           ->
      Lwt.return {
        head    = None;
        root    = Tree.empty;
        tree    = None;
        parents = [];
      }
    | Some (c, root) ->
      let root = (root :> tree) in
      Tree.find_tree root key >|= fun tree ->
      { head = Some c; root; tree; parents = [c] }

  let same_tree x y = match x, y with
    | None  , None   -> true
    | None  , _
    | _     , None   -> false
    | Some x, Some y -> Tree.equal x y

  (* Update the store with a new commit. Ensure the no commit becomes orphan
     in the process.  *)
  let update ?(allow_empty=false) ~info ?parents t key merge_tree f =
    snapshot t key >>= fun s ->
    (* this might take a very long time *)
    f s.tree >>= fun new_tree ->
    (* if no change and [allow_empty = true] then, do nothing *)
    if same_tree s.tree new_tree && not allow_empty && s.head <> None then
      Lwt.return (Ok true)
    else
      merge_tree s.root key ~current_tree:s.tree ~new_tree >>= function
      | Error _ as e -> Lwt.return e
      | Ok root      ->
        let info = info () in
        let parents = match parents with None -> s.parents | Some p -> p in
        Commit.v (repo t) ~info ~parents root >>= fun c ->
        add_commit t s.head (c, root_tree root) >|= fun r ->
        Ok r

  let ok x = Ok x

  let fail name = function
    | Ok x    -> Lwt.return x
    | Error e -> Fmt.kstrf Lwt.fail_with "%s: %a" name pp_write_error e

  let set_tree_once root key ~current_tree:_ ~new_tree = match new_tree with
    | None      -> Tree.remove root key >|= ok
    | Some tree -> Tree.add_tree root key tree >|= ok

  let set_tree ?(retries=13) ?allow_empty ?parents ~info t k v =
    Log.debug (fun l -> l "set %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info set_tree_once @@ fun _tree ->
    Lwt.return (Some v)

  let set_tree_exn ?retries ?allow_empty ?parents ~info t k v =
    set_tree ?retries ?allow_empty ?parents ~info t k v >>= fail "set_exn"

  let remove ?(retries=13) ?allow_empty ?parents ~info t k =
    Log.debug (fun l -> l "debug %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info set_tree_once @@ fun _tree ->
    Lwt.return None

  let remove_exn ?retries ?allow_empty ?parents ~info t k =
    remove ?retries ?allow_empty ?parents ~info t k >>= fail "remove_exn"

  let set ?retries ?allow_empty ?parents ~info t k v =
    let v = `Contents (v, Metadata.default) in
    set_tree t k ?retries ?allow_empty ?parents ~info v

  let set_exn ?retries ?allow_empty ?parents ~info t k v =
    set t k ?retries ?allow_empty ?parents ~info v >>= fail "set_exn"

  let test_and_set_tree_once ~test root key ~current_tree ~new_tree =
    match test, current_tree with
    | None      , None  -> set_tree_once root key ~new_tree ~current_tree
    | None, _| _, None  -> err_test current_tree
    | Some test, Some v ->
      if Tree.equal test v
      then set_tree_once root key ~new_tree ~current_tree
      else err_test current_tree

  let test_and_set_tree
      ?(retries=13) ?allow_empty ?parents ~info t k ~test ~set
    =
    Log.debug (fun l -> l "test-and-set %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info (test_and_set_tree_once ~test)
    @@ fun _tree -> Lwt.return set

  let test_and_set_tree_exn ?retries ?allow_empty ?parents ~info t k ~test ~set =
    test_and_set_tree ?retries ?allow_empty ?parents ~info t k ~test ~set
    >>= fail "test_and_set_tree_exn"

  let test_and_set ?retries ?allow_empty ?parents ~info t k ~test ~set =
    let test = match test with
      | None -> None
      | Some t -> Some (`Contents (t, Metadata.default))
    in
    let set = match set with
      | None -> None
      | Some s -> Some (`Contents (s, Metadata.default))
    in
    test_and_set_tree ?retries ?allow_empty ?parents ~info t k ~test ~set

  let test_and_set_exn ?retries ?allow_empty ?parents ~info t k ~test ~set =
    test_and_set ?retries ?allow_empty ?parents ~info t k ~test ~set
    >>= fail "test_and_set_exn"

  let merge_once ~old root key ~current_tree ~new_tree =
    let old = Merge.promise old in
    Merge.f (Merge.option Tree.merge) ~old current_tree new_tree >>= function
    | Ok tr   -> set_tree_once root key ~new_tree:tr ~current_tree
    | Error e -> write_error (e :> write_error)

  let merge_tree ?(retries=13) ?allow_empty ?parents ~info ~old t k tree =
    Log.debug (fun l -> l "merge %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info (merge_once ~old) @@ fun _tree ->
    Lwt.return tree

  let merge_tree_exn ?retries ?allow_empty ?parents ~info ~old t k tree =
    merge_tree ?retries ?allow_empty ?parents ~info ~old t k tree
    >>= fail "merge_tree_exn"

  let merge ?retries ?allow_empty ?parents ~info ~old t k v =
    let old = match old with
      | None   -> None
      | Some v -> Some (`Contents (v, Metadata.default))
    in
    let v = match v with
      | None   -> None
      | Some v -> Some (`Contents (v, Metadata.default))
    in
    merge_tree ?retries ?allow_empty ?parents ~info ~old t k v

  let merge_exn ?retries ?allow_empty ?parents ~info ~old t k v =
    merge ?retries ?allow_empty ?parents ~info ~old t k v >>= fail "merge_exn"

  let mem t k =
    tree t >>= fun tree ->
    Tree.mem tree k

  let mem_tree t k =
    tree t >>= fun tree ->
    Tree.mem_tree tree k

  let find_all t k =
    tree t >>= fun tree ->
    Tree.find_all tree k

  let find t k =
    tree t >>= fun tree ->
    Tree.find tree k

  let get t k =
    tree t >>= fun tree ->
    Tree.get tree k

  let find_tree t k =
    tree t >>= fun tree ->
    Tree.find_tree tree k

  let get_tree t k =
    tree t >>= fun tree ->
    Tree.get_tree tree k

  let hash t k =
    find_tree t k >|= function
    | None      -> None
    | Some tree -> Some (Tree.hash tree)

  let get_all t k =
    tree t >>= fun tree ->
    Tree.get_all tree k

  let list t k =
    tree t >>= fun tree ->
    Tree.list tree k

  let kind t k =
    tree t >>= fun tree ->
    Tree.kind tree k

  let with_tree
      ?(retries=13) ?allow_empty ?parents ?(strategy=`Test_and_set) ~info
      t key f =
    let done_once = ref false in
    let rec aux n old_tree =
      Log.debug (fun l -> l "with_tree %a (%d/%d)" pp_key key n retries);
      if !done_once && n > retries then write_error (`Too_many_retries retries)
      else
        f old_tree >>= fun new_tree ->
        match strategy, new_tree with
        | `Set, Some tree  ->
          set_tree t key ~retries ?allow_empty ?parents tree ~info
        | `Set, None       ->
          remove t key ~retries ?allow_empty ~info ?parents
        | `Test_and_set, _ ->
          (test_and_set_tree t key ~retries ?allow_empty ?parents ~info
             ~test:old_tree ~set:new_tree
           >>= function
           | Error (`Test_was tr) when retries > 0 && n <= retries ->
             done_once := true;
             aux (n+1) tr
           | e -> Lwt.return e)
        | `Merge, _ ->
          merge_tree ~old:old_tree ~retries ?allow_empty ?parents ~info
            t key new_tree
          >>= function
          | Ok _ as x -> Lwt.return x
          | Error (`Conflict _) when retries > 0 && n <= retries ->
            done_once := true;
            (* use the store's current tree as the new 'old store' *)
            (tree_and_head t >>= function
              | None         -> Lwt.return None
              | Some (_, tr) -> Tree.find_tree (tr :> tree) key)
            >>= fun old_tree ->
            aux (n+1) old_tree
          | Error e -> write_error e
    in
    find_tree t key >>= fun old_tree ->
    aux 0 old_tree

  let with_tree_exn ?retries ?allow_empty ?parents ?strategy ~info f t key =
    with_tree ?retries ?allow_empty ?strategy ?parents ~info f t key
    >>= fail "with_tree_exn"

  let clone ~src ~dst =
    (Head.find src >>= function
      | None   -> Branch_store.remove (branch_t src) dst
      | Some h -> Branch_store.set (branch_t src) dst h.Commit.h
    ) >>= fun () ->
    of_branch (repo src) dst

  let return_lcas r = function
    | Error _ as e -> Lwt.return e
    | Ok commits   ->
      Lwt_list.filter_map_p  (Commit.of_hash r) commits >|= fun x ->
      Ok x

  let lcas ?max_depth ?n t1 t2 =
    Head.get t1 >>= fun h1 ->
    Head.get t2 >>= fun h2 ->
    H.lcas (history_t t1) ?max_depth ?n h1.Commit.h h2.Commit.h >>=
    return_lcas t1.repo

  let lcas_with_commit t ?max_depth ?n c =
    Head.get t >>= fun h ->
    H.lcas (history_t t) ?max_depth ?n h.Commit.h c.Commit.h >>=
    return_lcas t.repo

  let lcas_with_branch t ?max_depth ?n b =
    Head.get t >>= fun h ->
    Head.get { t with head_ref = `Branch b } >>= fun head ->
    H.lcas (history_t t) ?max_depth ?n h.Commit.h head.Commit.h >>=
    return_lcas t.repo

  module Private = P

  type 'a merge = info:Info.f -> ?max_depth:int -> ?n:int -> 'a ->
    (unit, Merge.conflict) result Lwt.t

  let merge_with_branch t ~info ?max_depth ?n other =
    Log.debug (fun f -> f "merge_with_branch %a" pp_branch other);
    Branch_store.find (branch_t t) other >>= function
    | None  ->
      Fmt.kstrf Lwt.fail_invalid_arg
        "merge_with_branch: %a is not a valid branch ID"
        pp_branch other
    | Some c ->
      Commit.of_hash t.repo c >>= function
      | None   -> Lwt.fail_invalid_arg "invalid commit"
      | Some c -> Head.merge ~into:t ~info ?max_depth ?n c

  let merge_with_commit t ~info ?max_depth ?n other =
    Head.merge ~into:t ~info ?max_depth ?n other

  let merge_into ~into ~info ?max_depth ?n t =
    Log.debug (fun l -> l "merge");
    match head_ref t with
    | `Branch name -> merge_with_branch into ~info ?max_depth ?n name
    | `Head h      -> merge_with_commit into ~info ?max_depth ?n h
    | `Empty       -> Merge.ok ()

  module History =
    OCamlGraph.Persistent.Digraph.ConcreteBidirectional(struct
      type t = commit
      let hash h = P.Commit.Key.hash h.Commit.h
      let compare x y = Type.compare P.Commit.Key.t x.Commit.h y.Commit.h
      let equal x y = Type.equal P.Commit.Key.t x.Commit.h y.Commit.h
    end)

  module Gmap  = struct

    module Src = Object_graph.Make
        (P.Contents.Key)(P.Node.Metadata)(P.Node.Key)(P.Commit.Key)
        (Branch_store.Key)

    module Dst = struct
      include History
      let empty () = empty
    end

    let filter_map f g =
      let t = Dst.empty () in
      Src.fold_edges (fun x y t ->
          t >>= fun t ->
          f x >>= fun x ->
          f y >|= fun y ->
          match x,  y with
          | Some x, Some y ->
            let t = Dst.add_vertex t x in
            let t = Dst.add_vertex t y in
            Dst.add_edge t x y
          | _ -> t
        ) g (Lwt.return t)

  end

  let history ?depth ?(min=[]) ?(max=[]) t =
    Log.debug (fun f -> f "history");
    let pred = function
      | `Commit k ->
        H.parents (history_t t) k >>=
        Lwt_list.filter_map_p (Commit.of_hash t.repo) >|= fun parents ->
        List.map (fun x -> `Commit x.Commit.h) parents
      | _ -> Lwt.return_nil in
    begin Head.find t >>= function
      | Some h -> Lwt.return [h]
      | None   -> Lwt.return max
    end >>= fun max ->
    let max = List.map (fun k -> `Commit k.Commit.h) max in
    let min = List.map (fun k -> `Commit k.Commit.h) min in
    Gmap.Src.closure ?depth ~min ~max ~pred () >>= fun g ->
    Gmap.filter_map (function
        | `Commit k -> Commit.of_hash t.repo k
        | _ -> Lwt.return_none
      ) g

  module Branch = struct

    include P.Branch.Key

    let mem t = P.Branch.mem (P.Repo.branch_t t)

    let find t br =
      P.Branch.find (Repo.branch_t t) br >>= function
      | None   -> Lwt.return_none
      | Some h -> Commit.of_hash t h

    let set t br h = P.Branch.set (P.Repo.branch_t t) br (Commit.hash h)
    let remove t = P.Branch.remove (P.Repo.branch_t t)
    let list = Repo.branches

    let watch t k ?init f =
      let init = match init with None -> None | Some h -> Some h.Commit.h in
      P.Branch.watch_key (Repo.branch_t t) k ?init (lift_head_diff t f)
      >|= fun w ->
      (fun () -> Branch_store.unwatch (Repo.branch_t t) w)

    let watch_all t ?init f =
      let init = match init with
        | None   -> None
        | Some i -> Some (List.map (fun (k, v) -> k, v.Commit.h) i)
      in
      let f k v = lift_head_diff t (f k) v in
      P.Branch.watch (Repo.branch_t t) ?init f >|= fun w ->
      (fun () -> Branch_store.unwatch (Repo.branch_t t) w)

    let err_not_found k =
      Fmt.kstrf invalid_arg "Branch.get: %a not found" pp_branch k

    let get t k =
      find t k >>= function
      | None   -> err_not_found k
      | Some v -> Lwt.return v

  end

  module Status = struct
    type t = [ `Empty | `Branch of branch | `Commit of commit ]
    let t r =
      let open Type in
      variant "status" (fun empty branch commit -> function
          | `Empty    -> empty
          | `Branch b -> branch b
          | `Commit c -> commit c)
      |~ case0 "empty" `Empty
      |~ case1 "branch" Branch.t     (fun b -> `Branch b)
      |~ case1 "commit" (Commit.t r) (fun c -> `Commit c)
      |> sealv

    let pp ppf = function
      | `Empty    -> Fmt.string ppf "empty"
      | `Branch b -> Type.pp Branch.t ppf b
      | `Commit c -> Type.pp Hash.t ppf (Commit.hash c)

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
  let kind_t = Type.enum "kind" [ "contents", `Contents; "node", `Node ]

  let lca_error_t =
    Type.enum "lca-error" [
      "max-depth-reached", `Max_depth_reached;
      "too-many-lcas"    , `Too_many_lcas;
    ]

  let ff_error_t =
    Type.enum "ff-error" [
      "max-depth-reached", `Max_depth_reached;
      "too-many-lcas"    , `Too_many_lcas;
      "no-change"        , `No_change;
      "rejected"         , `Rejected;
    ]

  let write_error_t =
    let open Type in
    variant "write-error" (fun c m e -> function
        | `Conflict x -> c x
        | `Too_many_retries x -> m x
        | `Test_was x -> e x)
    |~ case1 "conflict" string (fun x -> `Conflict x)
    |~ case1 "too-many-retries" int (fun x -> `Too_many_retries x)
    |~ case1 "test-got" (option tree_t) (fun x -> `Test_was x)
    |> sealv

  let write_error_t =
    let of_string _ = assert false in
    Type.like' ~cli:(pp_write_error, of_string) write_error_t

end
