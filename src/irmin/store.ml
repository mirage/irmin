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

open! Import
include Store_intf

let src = Logs.Src.create "irmin" ~doc:"Irmin branch-consistent store"

module Log = (val Logs.src_log src : Logs.LOG)

module Content_addressable
    (IO : IO.S)
    (AO : S.APPEND_ONLY_STORE_MAKER with type 'a io := 'a IO.t) =
struct
  type 'a io = 'a IO.t

  open Syntax (IO)

  module Make (K : Hash.S) (V : Type.S) = struct
    include AO.Make (K) (V)
    module H = Hash.Typed (K) (V)

    let hash = H.hash
    let pp_key = Type.pp K.t
    let equal_hash = Type.(unstage (equal K.t))
    let invalid_arg msg = IO.fail (Invalid_argument msg)

    let find t k =
      let* v = find t k in
      match v with
      | None -> IO.return None
      | Some v as r ->
          let k' = hash v in
          if equal_hash k k' then IO.return r
          else
            Fmt.kstrf invalid_arg "corrupted value: got %a, expecting %a" pp_key
              k' pp_key k

    let unsafe_add t k v = add t k v

    let add t v =
      let k = hash v in
      let+ () = add t k v in
      k
  end
end

module Of_direct = struct
  module Append_only
      (DST : IO.S)
      (AO : S.APPEND_ONLY_STORE_MAKER with type 'a io := 'a) =
  struct
    type 'a io = 'a DST.t

    module Make (K : Hash.S) (V : Type.S) = struct
      module AO = AO.Make (K) (V)

      type 'a t = 'a AO.t
      type key = AO.key
      type value = AO.value

      let mem t k = DST.return (AO.mem t k)
      let find t k = DST.return (AO.find t k)
      let add t k v = DST.return (AO.add t k v)
      let clear t = DST.return (AO.clear t)
      let batch = AO.batch
      let close t = DST.return (AO.close t)
      let v c = DST.return (AO.v c)
    end
  end

  module Content_addressable
      (DST : IO.S)
      (CA : S.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io := 'a) =
  struct
    type 'a io = 'a DST.t

    module Make (K : Hash.S) (V : Type.S) = struct
      module CA = CA.Make (K) (V)

      type 'a t = 'a CA.t
      type key = CA.key
      type value = CA.value

      let mem t k = DST.return (CA.mem t k)
      let find t k = DST.return (CA.find t k)
      let add t k = DST.return (CA.add t k)
      let unsafe_add t k v = DST.return (CA.unsafe_add t k v)
      let clear t = DST.return (CA.clear t)
      let batch = CA.batch
      let close t = DST.return (CA.close t)
      let v c = DST.return (CA.v c)
    end
  end
end

module Make (P : Private.S) = struct
  module IO_list = IO.List (P.IO)
  module IO = P.IO
  open IO.Syntax
  module Merge = P.Merge
  module Mutex = IO.Mutex
  module Branch_store = P.Branch

  let invalid_arg msg = IO.fail (Invalid_argument msg)
  let failwith msg = IO.fail (Failure msg)
  let some x = IO.return (Some x)
  let none = IO.return None
  let ok x = IO.return (Ok x)
  let error e = IO.return (Error e)

  type branch = Branch_store.key

  module Hash = P.Hash

  type hash = Hash.t
  type lca_error = [ `Max_depth_reached | `Too_many_lcas ] [@@deriving irmin]
  type ff_error = [ `No_change | `Rejected | lca_error ]

  module Key = P.Node.Path

  type key = Key.t [@@deriving irmin]

  module Metadata = P.Node.Metadata
  module H = Commit.History (P.Merge) (P.Commit)

  type S.remote += E of P.Sync.endpoint

  module Contents = struct
    include P.Contents.Val

    let of_hash r h = P.Contents.find (P.Repo.contents_t r) h
    let hash c = P.Contents.Key.hash c
  end

  module Tree = struct
    include Tree.Make (P)

    let of_hash r h = import r h
    let shallow r h = import_no_check r h

    let hash : t -> hash =
     fun tr -> match hash tr with `Node h -> h | `Contents (h, _) -> h
  end

  let save_contents b c = P.Contents.add b c

  let save_tree ?(clear = true) r x y (tr : Tree.t) =
    match Tree.destruct tr with
    | `Contents (c, _) ->
        let* c = Tree.Contents.force_exn c in
        save_contents x c
    | `Node n -> Tree.export ~clear r x y n

  type node = Tree.node [@@deriving irmin]
  type contents = Contents.t [@@deriving irmin]
  type metadata = Metadata.t [@@deriving irmin]
  type tree = Tree.t
  type repo = P.Repo.t

  let equal_hash = Type.(unstage (equal Hash.t))
  let equal_contents = Type.(unstage (equal Contents.t))
  let equal_branch = Type.(unstage (equal Branch_store.Key.t))
  let pp_key = Type.pp Key.t
  let pp_hash = Type.pp Hash.t
  let pp_branch = Type.pp Branch_store.Key.t
  let pp_option = Type.pp (Type.option Type.int)

  module Commit = struct
    type t = { r : repo; h : Hash.t; v : P.Commit.value }

    let t r =
      let open Type in
      record "commit" (fun h v -> { r; h; v })
      |+ field "hash" Hash.t (fun t -> t.h)
      |+ field "value" P.Commit.Val.t (fun t -> t.v)
      |> sealr

    let v r ~info ~parents tree =
      P.Repo.batch r @@ fun contents_t node_t commit_t ->
      let* node =
        match Tree.destruct tree with
        | `Node t -> Tree.export r contents_t node_t t
        | `Contents _ -> invalid_arg "cannot add contents at the root"
      in
      let v = P.Commit.Val.v ~info ~node ~parents in
      let+ h = P.Commit.add commit_t v in
      { r; h; v }

    let node t = P.Commit.Val.node t.v
    let tree t = Tree.import_no_check t.r (`Node (node t))
    let equal x y = equal_hash x.h y.h
    let hash t = t.h
    let info t = P.Commit.Val.info t.v
    let parents t = P.Commit.Val.parents t.v
    let pp_hash ppf t = Type.pp Hash.t ppf t.h

    let of_hash r h =
      let+ v = P.Commit.find (P.Repo.commit_t r) h in
      match v with None -> None | Some v -> Some { r; h; v }

    let to_private_commit t = t.v

    let of_private_commit r v =
      let h = P.Commit.Key.hash v in
      { r; h; v }

    let equal_opt x y =
      match (x, y) with
      | None, None -> true
      | Some x, Some y -> equal x y
      | _ -> false
  end

  type commit = Commit.t

  let to_private_node = Tree.to_private_node
  let of_private_node = Tree.of_private_node
  let to_private_commit = Commit.to_private_commit
  let of_private_commit = Commit.of_private_commit

  type head_ref = [ `Branch of branch | `Head of commit option ref ]

  module OCamlGraph = Graph
  module Graph = Node.Graph (Merge) (P.Node)
  module KGraph = Object_graph.Make (IO) (Hash) (Branch_store.Key)

  type slice = P.Slice.t [@@deriving irmin]
  type watch = unit -> unit IO.t

  let unwatch w = w ()

  module Repo = struct
    type t = repo

    let v = P.Repo.v
    let close = P.Repo.close
    let graph_t t = P.Repo.node_t t
    let history_t t = P.Repo.commit_t t
    let branch_t t = P.Repo.branch_t t
    let commit_t t = P.Repo.commit_t t
    let node_t t = P.Repo.node_t t
    let contents_t t = P.Repo.contents_t t
    let branches t = P.Branch.list (branch_t t)

    let heads repo =
      let t = branch_t repo in
      let* bs = Branch_store.list t in
      IO_list.fold_left_s
        (fun acc r ->
          let* h = Branch_store.find t r in
          match h with
          | None -> IO.return acc
          | Some h -> (
              let+ h = Commit.of_hash repo h in
              match h with None -> acc | Some h -> h :: acc))
        [] bs

    let export ?(full = true) ?depth ?(min = []) ?(max = `Head) t =
      Log.debug (fun f ->
          f "export depth=%s full=%b min=%d max=%s"
            (match depth with None -> "<none>" | Some d -> string_of_int d)
            full (List.length min)
            (match max with
            | `Head -> "heads"
            | `Max m -> string_of_int (List.length m)));
      let* max = match max with `Head -> heads t | `Max m -> IO.return m in
      let* slice = P.Slice.empty () in
      let max = List.map (fun x -> `Commit x.Commit.h) max in
      let min = List.map (fun x -> `Commit x.Commit.h) min in
      let pred = function
        | `Commit k ->
            let+ parents = H.parents (history_t t) k in
            List.map (fun x -> `Commit x) parents
        | _ -> IO.return []
      in
      let* g = KGraph.closure ?depth ~pred ~min ~max () in
      let keys =
        List.fold_left
          (fun acc -> function `Commit c -> c :: acc | _ -> acc)
          [] (KGraph.vertex g)
      in
      let root_nodes = ref [] in
      let* () =
        IO_list.iter_p
          (fun k ->
            let* c = P.Commit.find (commit_t t) k in
            match c with
            | None -> IO.return ()
            | Some c ->
                root_nodes := P.Commit.Val.node c :: !root_nodes;
                P.Slice.add slice (`Commit (k, c)))
          keys
      in
      if not full then IO.return slice
      else
        (* XXX: we can compute a [min] if needed *)
        let* nodes = Graph.closure (graph_t t) ~min:[] ~max:!root_nodes in
        let module KSet = Set.Make (struct
          type t = P.Contents.key

          let compare = Type.(unstage (compare P.Contents.Key.t))
        end) in
        let contents = ref KSet.empty in
        let* () =
          IO_list.iter_p
            (fun k ->
              let* v = P.Node.find (node_t t) k in
              match v with
              | None -> IO.return ()
              | Some v ->
                  List.iter
                    (function
                      | _, `Contents (c, _) -> contents := KSet.add c !contents
                      | _ -> ())
                    (P.Node.Val.list v);
                  P.Slice.add slice (`Node (k, v)))
            nodes
        in
        let+ () =
          IO_list.iter_p
            (fun k ->
              let* m = P.Contents.find (contents_t t) k in
              match m with
              | None -> IO.return ()
              | Some m -> P.Slice.add slice (`Contents (k, m)))
            (KSet.elements !contents)
        in
        slice

    exception Import_error of string

    let import_error fmt = Fmt.kstrf (fun x -> IO.fail (Import_error x)) fmt

    let import t s =
      let aux name add (k, v) =
        let* k' = add v in
        if not (equal_hash k k') then
          import_error "%s import error: expected %a, got %a" name pp_hash k
            pp_hash k'
        else IO.return ()
      in
      let contents = ref [] in
      let nodes = ref [] in
      let commits = ref [] in
      let* () =
        P.Slice.iter s (function
          | `Contents c ->
              contents := c :: !contents;
              IO.return ()
          | `Node n ->
              nodes := n :: !nodes;
              IO.return ()
          | `Commit c ->
              commits := c :: !commits;
              IO.return ())
      in
      P.Repo.batch t @@ fun contents_t node_t commit_t ->
      IO.catch
        (fun () ->
          let* () =
            IO_list.iter_p
              (aux "Contents" (P.Contents.add contents_t))
              !contents
          in
          let* () = IO_list.iter_p (aux "Node" (P.Node.add node_t)) !nodes in
          let+ () =
            IO_list.iter_p (aux "Commit" (P.Commit.add commit_t)) !commits
          in
          Ok ())
        (function
          | Import_error e -> error (`Msg e)
          | e -> Fmt.kstrf invalid_arg "impot error: %a" Fmt.exn e)

    type elt =
      [ `Commit of Hash.t
      | `Node of Hash.t
      | `Contents of Hash.t
      | `Branch of P.Branch.Key.t ]
    [@@deriving irmin]

    let always x _ = IO.return x
    let default_pred_contents _ _ = IO.return []

    let default_pred_node t k =
      let+ v = P.Node.find (node_t t) k in
      match v with
      | None -> []
      | Some v ->
          List.rev_map
            (function
              | _, `Node n -> `Node n | _, `Contents (c, _) -> `Contents c)
            (P.Node.Val.list v)

    let default_pred_commit t c =
      let+ co = P.Commit.find (commit_t t) c in
      match co with
      | None ->
          Log.debug (fun l -> l "%a: not found" (Type.pp Hash.t) c);
          []
      | Some c ->
          let node = P.Commit.Val.node c in
          let parents = P.Commit.Val.parents c in
          [ `Node node ] @ List.map (fun k -> `Commit k) parents

    let default_pred_branch t b =
      let+ br = P.Branch.find (branch_t t) b in
      match br with
      | None ->
          Log.debug (fun l -> l "%a: not found" (Type.pp P.Branch.Key.t) b);
          []
      | Some b -> [ `Commit b ]

    let iter ?cache_size ~min ~max ?edge ?(branch = always ())
        ?(commit = always ()) ?(node = always ()) ?(contents = always ())
        ?(skip_branch = always false) ?(skip_commit = always false)
        ?(skip_node = always false) ?(skip_contents = always false)
        ?(pred_branch = default_pred_branch)
        ?(pred_commit = default_pred_commit) ?(pred_node = default_pred_node)
        ?(pred_contents = default_pred_contents) ?(rev = true) t =
      let node = function
        | `Commit x -> commit x
        | `Node x -> node x
        | `Contents x -> contents x
        | `Branch x -> branch x
      in
      let skip = function
        | `Commit x -> skip_commit x
        | `Node x -> skip_node x
        | `Contents x -> skip_contents x
        | `Branch x -> skip_branch x
      in
      let pred = function
        | `Commit x -> pred_commit t x
        | `Node x -> pred_node t x
        | `Contents x -> pred_contents t x
        | `Branch x -> pred_branch t x
      in
      KGraph.iter ?cache_size ~pred ~min ~max ~node ?edge ~skip ~rev ()
  end

  type t = {
    repo : Repo.t;
    head_ref : head_ref;
    mutable tree : (commit * tree) option;
    (* cache for the store tree *)
    lock : Mutex.t;
  }

  type step = Key.step [@@deriving irmin]

  let repo t = t.repo
  let branch_t t = Repo.branch_t t.repo
  let commit_t t = Repo.commit_t t.repo
  let history_t t = commit_t t

  let status t =
    match t.head_ref with
    | `Branch b -> `Branch b
    | `Head h -> ( match !h with None -> `Empty | Some c -> `Commit c)

  let head_ref t =
    match t.head_ref with
    | `Branch t -> `Branch t
    | `Head h -> ( match !h with None -> `Empty | Some h -> `Head h)

  let branch t =
    match head_ref t with `Branch t -> some t | `Empty | `Head _ -> none

  let err_no_head s = Fmt.kstrf invalid_arg "Irmin.%s: no head" s

  open Merge.Infix

  let retry_merge name fn =
    let rec aux i =
      let* r = fn () in
      match r with
      | Error _ as c -> IO.return c
      | Ok true -> Merge.ok ()
      | Ok false ->
          Log.debug (fun f -> f "Irmin.%s: conflict, retrying (%d)." name i);
          aux (i + 1)
    in
    aux 1

  let of_ref repo head_ref =
    let lock = Mutex.create () in
    IO.return { lock; head_ref; repo; tree = None }

  let err_invalid_branch t =
    Fmt.kstrf invalid_arg "%a is not a valid branch name." pp_branch t

  let of_branch repo id =
    if Branch_store.Key.is_valid id then of_ref repo (`Branch id)
    else err_invalid_branch id

  let master repo = of_branch repo Branch_store.Key.master
  let empty repo = of_ref repo (`Head (ref None))
  let of_commit c = of_ref c.Commit.r (`Head (ref (Some c)))

  let skip_key key =
    Log.debug (fun l -> l "[watch-key] key %a has not changed" pp_key key);
    IO.return ()

  let changed_key key old_t new_t =
    Log.debug (fun l ->
        let pp = Fmt.option ~none:(Fmt.any "<none>") pp_hash in
        let old_h = Option.map Tree.hash old_t in
        let new_h = Option.map Tree.hash new_t in
        l "[watch-key] key %a has changed: %a -> %a" pp_key key pp old_h pp
          new_h)

  let with_tree ~key x f =
    let* x = x in
    match x with
    | None -> skip_key key
    | Some x ->
        changed_key key None None;
        f x

  let lift_tree_diff ~key tree fn = function
    | `Removed x ->
        with_tree ~key (tree x) @@ fun v ->
        changed_key key (Some v) None;
        fn @@ `Removed (x, v)
    | `Added x ->
        with_tree ~key (tree x) @@ fun v ->
        changed_key key None (Some v);
        fn @@ `Added (x, v)
    | `Updated (x, y) -> (
        assert (not (Commit.equal x y));
        let* vx = tree x in
        let* vy = tree y in
        match (vx, vy) with
        | None, None -> skip_key key
        | None, Some vy ->
            changed_key key None (Some vy);
            fn @@ `Added (y, vy)
        | Some vx, None ->
            changed_key key (Some vx) None;
            fn @@ `Removed (x, vx)
        | Some vx, Some vy ->
            if Tree.equal vx vy then skip_key key
            else (
              changed_key key (Some vx) (Some vy);
              fn @@ `Updated ((x, vx), (y, vy))))

  let head t =
    let h =
      match head_ref t with
      | `Head key -> some key
      | `Empty -> none
      | `Branch name -> (
          let* h = Branch_store.find (branch_t t) name in
          match h with None -> none | Some h -> Commit.of_hash t.repo h)
    in
    let+ h = h in
    Log.debug (fun f -> f "Head.find -> %a" Fmt.(option Commit.pp_hash) h);
    h

  let tree_and_head t =
    let+ h = head t in
    match h with
    | None -> None
    | Some h -> (
        match t.tree with
        | Some (o, t) when Commit.equal o h -> Some (o, t)
        | _ ->
            t.tree <- None;

            (* the tree cache needs to be invalidated *)
            let tree = Tree.import_no_check (repo t) (`Node (Commit.node h)) in
            t.tree <- Some (h, tree);
            Some (h, tree))

  let tree t =
    let+ r = tree_and_head t in
    match r with None -> Tree.empty | Some (_, tree) -> (tree :> tree)

  let lift_head_diff repo fn = function
    | `Removed x -> (
        let* x = Commit.of_hash repo x in
        match x with None -> IO.return () | Some x -> fn (`Removed x))
    | `Updated (x, y) -> (
        let* x = Commit.of_hash repo x in
        let* y = Commit.of_hash repo y in
        match (x, y) with
        | None, None -> IO.return ()
        | Some x, None -> fn (`Removed x)
        | None, Some y -> fn (`Added y)
        | Some x, Some y -> fn (`Updated (x, y)))
    | `Added x -> (
        let* x = Commit.of_hash repo x in
        match x with None -> IO.return () | Some x -> fn (`Added x))

  let watch t ?init fn =
    let* n = branch t in
    match n with
    | None -> failwith "watch a detached head: TODO"
    | Some name0 ->
        let init =
          match init with
          | None -> None
          | Some head0 -> Some [ (name0, head0.Commit.h) ]
        in
        let+ id =
          Branch_store.watch (branch_t t) ?init (fun name head ->
              if equal_branch name0 name then lift_head_diff t.repo fn head
              else IO.return ())
        in
        fun () -> Branch_store.unwatch (branch_t t) id

  let watch_key t key ?init fn =
    Log.debug (fun f -> f "watch-key %a" pp_key key);
    let tree c = Tree.find_tree (Commit.tree c) key in
    watch t ?init (lift_tree_diff ~key tree fn)

  module Head = struct
    let list = Repo.heads
    let find = head

    let get t =
      let* k = find t in
      match k with None -> err_no_head "head" | Some k -> IO.return k

    let set t c =
      match t.head_ref with
      | `Head h ->
          h := Some c;
          IO.return ()
      | `Branch name -> Branch_store.set (branch_t t) name c.Commit.h

    let test_and_set_unsafe t ~test ~set =
      match t.head_ref with
      | `Head head ->
          (* [head] is protected by [t.lock]. *)
          if Commit.equal_opt !head test then (
            head := set;
            IO.return true)
          else IO.return false
      | `Branch name ->
          let h = function None -> None | Some c -> Some c.Commit.h in
          Branch_store.test_and_set (branch_t t) name ~test:(h test)
            ~set:(h set)

    let test_and_set t ~test ~set =
      Mutex.with_lock t.lock (fun () -> test_and_set_unsafe t ~test ~set)

    type ff_error = [ `Rejected | `No_change | lca_error ]

    let fast_forward t ?max_depth ?n new_head =
      let return x = if x then Ok () else Error (`Rejected :> ff_error) in
      let* h = find t in
      match h with
      | None ->
          let+ h = test_and_set t ~test:None ~set:(Some new_head) in
          return h
      | Some old_head -> (
          Log.debug (fun f ->
              f "fast-forward-head old=%a new=%a" Commit.pp_hash old_head
                Commit.pp_hash new_head);
          if Commit.equal new_head old_head then
            (* we only update if there is a change *)
            error `No_change
          else
            let* lcas =
              H.lcas (history_t t) ?max_depth ?n new_head.Commit.h
                old_head.Commit.h
            in
            match lcas with
            | Ok [ x ] when equal_hash x old_head.Commit.h ->
                (* we only update if new_head > old_head *)
                let+ h =
                  test_and_set t ~test:(Some old_head) ~set:(Some new_head)
                in
                return h
            | Ok _ -> error `Rejected
            | Error e -> error (e :> ff_error))

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
        let* head = head t in
        match head with
        | None ->
            let* h = test_and_set_unsafe t ~test:head ~set:(Some c1) in
            Merge.ok h
        | Some c2 ->
            three_way_merge t ~info ?max_depth ?n c1 c2 >>=* fun c3 ->
            let* c3 = Commit.of_hash t.repo c3 in
            let* h = test_and_set_unsafe t ~test:head ~set:c3 in
            Merge.ok h
      in
      Mutex.with_lock t.lock (fun () -> retry_merge "merge_head" aux)
  end

  (* Retry an operation until the optimistic lock is happy. Ensure
     that the operation is done at least once. *)
  let retry ~retries fn =
    let done_once = ref false in
    let rec aux i =
      if !done_once && i > retries then error (`Too_many_retries retries)
      else
        let* r = fn () in
        match r with
        | Ok true -> ok ()
        | Error e -> error e
        | Ok false ->
            done_once := true;
            aux (i + 1)
    in
    aux 0

  let root_tree = function
    | `Node _ as n -> Tree.v n
    | `Contents _ -> assert false

  let add_commit t old_head ((c, _) as tree) =
    match t.head_ref with
    | `Head head ->
        IO.Mutex.with_lock t.lock (fun () ->
            if not (Commit.equal_opt old_head !head) then IO.return false
            else (
              (* [head] is protected by [t.lock] *)
              head := Some c;
              t.tree <- Some tree;
              IO.return true))
    | `Branch name ->
        (* concurrent handlers and/or process can modify the
           branch. Need to check that we are still working on the same
           head. *)
        let test =
          match old_head with None -> None | Some c -> Some (Commit.hash c)
        in
        let set = Some (Commit.hash c) in
        let+ r = Branch_store.test_and_set (branch_t t) name ~test ~set in
        if r then t.tree <- Some tree;
        r

  type write_error =
    [ Merge.conflict | `Too_many_retries of int | `Test_was of tree option ]

  let pp_write_error ppf = function
    | `Conflict e -> Fmt.pf ppf "Got a conflict: %s" e
    | `Too_many_retries i ->
        Fmt.pf ppf
          "Failure after %d attempts to retry the operation: Too many attempts."
          i
    | `Test_was t ->
        Fmt.pf ppf "Test-and-set failed: got %a when reading the store"
          Type.(pp (option Tree.tree_t))
          t

  let write_error e : ('a, write_error) result IO.t = error e
  let err_test v = write_error (`Test_was v)

  type snapshot = {
    head : commit option;
    root : tree;
    tree : tree option;
    (* the subtree used by the transaction *)
    parents : commit list;
  }

  let snapshot t key =
    let* r = tree_and_head t in
    match r with
    | None ->
        IO.return { head = None; root = Tree.empty; tree = None; parents = [] }
    | Some (c, root) ->
        let root = (root :> tree) in
        let+ tree = Tree.find_tree root key in
        { head = Some c; root; tree; parents = [ c ] }

  let same_tree x y =
    match (x, y) with
    | None, None -> true
    | None, _ | _, None -> false
    | Some x, Some y -> Tree.equal x y

  (* Update the store with a new commit. Ensure the no commit becomes orphan
     in the process. *)
  let update ?(allow_empty = false) ~info ?parents t key merge_tree f =
    let* s = snapshot t key in
    (* this might take a very long time *)
    let* new_tree = f s.tree in
    (* if no change and [allow_empty = true] then, do nothing *)
    if same_tree s.tree new_tree && (not allow_empty) && s.head <> None then
      ok true
    else
      let* m = merge_tree s.root key ~current_tree:s.tree ~new_tree in
      match m with
      | Error e -> error e
      | Ok root ->
          let info = info () in
          let parents = match parents with None -> s.parents | Some p -> p in
          let parents = List.map Commit.hash parents in
          let* c = Commit.v (repo t) ~info ~parents root in
          let+ r = add_commit t s.head (c, root_tree (Tree.destruct root)) in
          Ok r

  let get_ok name = function
    | Ok x -> IO.return x
    | Error e -> Fmt.kstrf failwith "%s: %a" name pp_write_error e

  let set_tree_once root key ~current_tree:_ ~new_tree =
    match new_tree with
    | None ->
        let+ t = Tree.remove root key in
        Ok t
    | Some tree ->
        let+ t = Tree.add_tree root key tree in
        Ok t

  let set_tree ?(retries = 13) ?allow_empty ?parents ~info t k v =
    Log.debug (fun l -> l "set %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info set_tree_once @@ fun _tree -> some v

  let set_tree_exn ?retries ?allow_empty ?parents ~info t k v =
    let* t = set_tree ?retries ?allow_empty ?parents ~info t k v in
    get_ok "set_exn" t

  let remove ?(retries = 13) ?allow_empty ?parents ~info t k =
    Log.debug (fun l -> l "debug %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info set_tree_once @@ fun _tree -> none

  let remove_exn ?retries ?allow_empty ?parents ~info t k =
    let* t = remove ?retries ?allow_empty ?parents ~info t k in
    get_ok "remove_exn" t

  let set ?retries ?allow_empty ?parents ~info t k v =
    let v = Tree.of_contents v in
    set_tree t k ?retries ?allow_empty ?parents ~info v

  let set_exn ?retries ?allow_empty ?parents ~info t k v =
    let* t = set t k ?retries ?allow_empty ?parents ~info v in
    get_ok "set_exn" t

  let test_and_set_tree_once ~test root key ~current_tree ~new_tree =
    match (test, current_tree) with
    | None, None -> set_tree_once root key ~new_tree ~current_tree
    | None, _ | _, None -> err_test current_tree
    | Some test, Some v ->
        if Tree.equal test v then set_tree_once root key ~new_tree ~current_tree
        else err_test current_tree

  let test_and_set_tree ?(retries = 13) ?allow_empty ?parents ~info t k ~test
      ~set =
    Log.debug (fun l -> l "test-and-set %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info (test_and_set_tree_once ~test)
    @@ fun _tree -> IO.return set

  let test_and_set_tree_exn ?retries ?allow_empty ?parents ~info t k ~test ~set
      =
    let* t =
      test_and_set_tree ?retries ?allow_empty ?parents ~info t k ~test ~set
    in
    get_ok "test_and_set_tree_exn" t

  let test_and_set ?retries ?allow_empty ?parents ~info t k ~test ~set =
    let test = Option.map Tree.of_contents test in
    let set = Option.map Tree.of_contents set in
    test_and_set_tree ?retries ?allow_empty ?parents ~info t k ~test ~set

  let test_and_set_exn ?retries ?allow_empty ?parents ~info t k ~test ~set =
    let* t = test_and_set ?retries ?allow_empty ?parents ~info t k ~test ~set in
    get_ok "test_and_set_exn" t

  let merge_tree = Tree.merge ()

  let merge_once ~old root key ~current_tree ~new_tree =
    let old = Merge.promise old in
    let* tr = Merge.f (Merge.option merge_tree) ~old current_tree new_tree in
    match tr with
    | Ok tr -> set_tree_once root key ~new_tree:tr ~current_tree
    | Error e -> write_error (e :> write_error)

  let merge_tree ?(retries = 13) ?allow_empty ?parents ~info ~old t k tree =
    Log.debug (fun l -> l "merge %a" pp_key k);
    retry ~retries @@ fun () ->
    update t k ?allow_empty ?parents ~info (merge_once ~old) @@ fun _tree ->
    IO.return tree

  let merge_tree_exn ?retries ?allow_empty ?parents ~info ~old t k tree =
    let* t = merge_tree ?retries ?allow_empty ?parents ~info ~old t k tree in
    get_ok "merge_tree_exn" t

  let merge ?retries ?allow_empty ?parents ~info ~old t k v =
    let old = Option.map Tree.of_contents old in
    let v = Option.map Tree.of_contents v in
    merge_tree ?retries ?allow_empty ?parents ~info ~old t k v

  let merge_exn ?retries ?allow_empty ?parents ~info ~old t k v =
    let* t = merge ?retries ?allow_empty ?parents ~info ~old t k v in
    get_ok "merge_exn" t

  let mem t k =
    let* tree = tree t in
    Tree.mem tree k

  let mem_tree t k =
    let* tree = tree t in
    Tree.mem_tree tree k

  let find_all t k =
    let* tree = tree t in
    Tree.find_all tree k

  let find t k =
    let* tree = tree t in
    Tree.find tree k

  let get t k =
    let* tree = tree t in
    Tree.get tree k

  let find_tree t k =
    let* tree = tree t in
    Tree.find_tree tree k

  let get_tree t k =
    let* tree = tree t in
    Tree.get_tree tree k

  let hash t k =
    let+ tree = find_tree t k in
    match tree with None -> None | Some tree -> Some (Tree.hash tree)

  let get_all t k =
    let* tree = tree t in
    Tree.get_all tree k

  let list t k =
    let* tree = tree t in
    Tree.list tree k

  let kind t k =
    let* tree = tree t in
    Tree.kind tree k

  let with_tree ?(retries = 13) ?allow_empty ?parents
      ?(strategy = `Test_and_set) ~info t key f =
    let done_once = ref false in
    let rec aux n old_tree =
      Log.debug (fun l -> l "with_tree %a (%d/%d)" pp_key key n retries);
      if !done_once && n > retries then write_error (`Too_many_retries retries)
      else
        let* new_tree = f old_tree in
        match (strategy, new_tree) with
        | `Set, Some tree ->
            set_tree t key ~retries ?allow_empty ?parents tree ~info
        | `Set, None -> remove t key ~retries ?allow_empty ~info ?parents
        | `Test_and_set, _ -> (
            let* t =
              test_and_set_tree t key ~retries ?allow_empty ?parents ~info
                ~test:old_tree ~set:new_tree
            in
            match t with
            | Error (`Test_was tr) when retries > 0 && n <= retries ->
                done_once := true;
                aux (n + 1) tr
            | e -> IO.return e)
        | `Merge, _ -> (
            let* tr =
              merge_tree ~old:old_tree ~retries ?allow_empty ?parents ~info t
                key new_tree
            in
            match tr with
            | Ok _ as x -> IO.return x
            | Error (`Conflict _) when retries > 0 && n <= retries ->
                done_once := true;

                (* use the store's current tree as the new 'old store' *)
                let* old_tree =
                  let* tr = tree_and_head t in
                  match tr with
                  | None -> none
                  | Some (_, tr) -> Tree.find_tree (tr :> tree) key
                in
                aux (n + 1) old_tree
            | Error e -> write_error e)
    in
    let* old_tree = find_tree t key in
    aux 0 old_tree

  let with_tree_exn ?retries ?allow_empty ?parents ?strategy ~info f t key =
    let* t = with_tree ?retries ?allow_empty ?strategy ?parents ~info f t key in
    get_ok "with_tree_exn" t

  let clone ~src ~dst =
    let* () =
      let* h = Head.find src in
      match h with
      | None -> Branch_store.remove (branch_t src) dst
      | Some h -> Branch_store.set (branch_t src) dst h.Commit.h
    in
    of_branch (repo src) dst

  let return_lcas r = function
    | Error _ as e -> IO.return e
    | Ok commits ->
        let+ ls = IO_list.filter_map_p (Commit.of_hash r) commits in
        Ok ls

  let lcas ?max_depth ?n t1 t2 =
    let* h1 = Head.get t1 in
    let* h2 = Head.get t2 in
    let* ls = H.lcas (history_t t1) ?max_depth ?n h1.Commit.h h2.Commit.h in
    return_lcas t1.repo ls

  let lcas_with_commit t ?max_depth ?n c =
    let* h = Head.get t in
    let* ls = H.lcas (history_t t) ?max_depth ?n h.Commit.h c.Commit.h in
    return_lcas t.repo ls

  let lcas_with_branch t ?max_depth ?n b =
    let* h = Head.get t in
    let* head = Head.get { t with head_ref = `Branch b } in
    let* ls = H.lcas (history_t t) ?max_depth ?n h.Commit.h head.Commit.h in
    return_lcas t.repo ls

  module Private = P

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result IO.t

  let merge_with_branch t ~info ?max_depth ?n other =
    Log.debug (fun f -> f "merge_with_branch %a" pp_branch other);
    let* c = Branch_store.find (branch_t t) other in
    match c with
    | None ->
        Fmt.kstrf invalid_arg "merge_with_branch: %a is not a valid branch ID"
          pp_branch other
    | Some c -> (
        let* c = Commit.of_hash t.repo c in
        match c with
        | None -> invalid_arg "invalid commit"
        | Some c -> Head.merge ~into:t ~info ?max_depth ?n c)

  let merge_with_commit t ~info ?max_depth ?n other =
    Head.merge ~into:t ~info ?max_depth ?n other

  let merge_into ~into ~info ?max_depth ?n t =
    Log.debug (fun l -> l "merge");
    match head_ref t with
    | `Branch name -> merge_with_branch into ~info ?max_depth ?n name
    | `Head h -> merge_with_commit into ~info ?max_depth ?n h
    | `Empty -> Merge.ok ()

  module History = OCamlGraph.Persistent.Digraph.ConcreteBidirectional (struct
    type t = commit

    let hash h = P.Commit.Key.short_hash h.Commit.h
    let compare_key = Type.(unstage (compare P.Commit.Key.t))
    let compare x y = compare_key x.Commit.h y.Commit.h
    let equal x y = equal_hash x.Commit.h y.Commit.h
  end)

  module Gmap = struct
    module Src = Object_graph.Make (IO) (Hash) (Branch_store.Key)

    module Dst = struct
      include History

      let empty () = empty
    end

    let filter_map f g =
      let t = Dst.empty () in
      if Src.nb_vertex g = 1 then
        match Src.vertex g with
        | [ v ] -> (
            let+ x = f v in
            match x with Some v -> Dst.add_vertex t v | None -> t)
        | _ -> assert false
      else
        Src.fold_edges
          (fun x y t ->
            let* t = t in
            let* x = f x in
            let+ y = f y in
            match (x, y) with
            | Some x, Some y ->
                let t = Dst.add_vertex t x in
                let t = Dst.add_vertex t y in
                Dst.add_edge t x y
            | _ -> t)
          g (IO.return t)
  end

  let history ?depth ?(min = []) ?(max = []) t =
    Log.debug (fun f -> f "history");
    let pred = function
      | `Commit k ->
          let* ps = H.parents (history_t t) k in
          let+ ps = IO_list.filter_map_p (Commit.of_hash t.repo) ps in
          List.map (fun x -> `Commit x.Commit.h) ps
      | _ -> IO.return []
    in
    let* max =
      let+ h = Head.find t in
      match h with Some h -> [ h ] | None -> max
    in
    let max = List.map (fun k -> `Commit k.Commit.h) max in
    let min = List.map (fun k -> `Commit k.Commit.h) min in
    let* g = Gmap.Src.closure ?depth ~min ~max ~pred () in
    Gmap.filter_map
      (function `Commit k -> Commit.of_hash t.repo k | _ -> none)
      g

  module Heap = Binary_heap.Make (struct
    type t = commit * int

    let compare c1 c2 =
      (* [bheap] operates on miminums, we need to invert the comparison. *)
      -Int64.compare
         (Info.date (Commit.info (fst c1)))
         (Info.date (Commit.info (fst c2)))
  end)

  let last_modified ?depth ?(n = 1) t key =
    Log.debug (fun l ->
        l "last_modified depth=%a n=%d key=%a" pp_option depth n pp_key key);
    let repo = repo t in
    let* commit = Head.get t in
    let heap = Heap.create ~dummy:(commit, 0) 0 in
    let () = Heap.add heap (commit, 0) in
    let rec search acc =
      if Heap.is_empty heap || List.length acc = n then IO.return acc
      else
        let current, current_depth = Heap.pop_minimum heap in
        let parents = Commit.parents current in
        let tree = Commit.tree current in
        let* current_value = Tree.find tree key in
        if List.length parents = 0 then
          if current_value <> None then IO.return (current :: acc)
          else IO.return acc
        else
          let max_depth =
            match depth with
            | Some depth -> current_depth >= depth
            | None -> false
          in
          let* found =
            IO_list.for_all_p
              (fun hash ->
                let* c = Commit.of_hash repo hash in
                match c with
                | Some commit -> (
                    let () =
                      if not max_depth then
                        Heap.add heap (commit, current_depth + 1)
                    in
                    let tree = Commit.tree commit in
                    let+ e = Tree.find tree key in
                    match (e, current_value) with
                    | Some x, Some y -> not (equal_contents x y)
                    | Some _, None -> true
                    | None, Some _ -> true
                    | _, _ -> false)
                | None -> IO.return false)
              parents
          in
          if found then search (current :: acc) else search acc
    in
    search []

  module Branch = struct
    include P.Branch.Key

    let mem t = P.Branch.mem (P.Repo.branch_t t)

    let find t br =
      let* h = P.Branch.find (Repo.branch_t t) br in
      match h with None -> none | Some h -> Commit.of_hash t h

    let set t br h = P.Branch.set (P.Repo.branch_t t) br (Commit.hash h)
    let remove t = P.Branch.remove (P.Repo.branch_t t)
    let list = Repo.branches

    let watch t k ?init f =
      let init = match init with None -> None | Some h -> Some h.Commit.h in
      let+ w =
        P.Branch.watch_key (Repo.branch_t t) k ?init (lift_head_diff t f)
      in
      fun () -> Branch_store.unwatch (Repo.branch_t t) w

    let watch_all t ?init f =
      let init =
        match init with
        | None -> None
        | Some i -> Some (List.map (fun (k, v) -> (k, v.Commit.h)) i)
      in
      let f k v = lift_head_diff t (f k) v in
      let+ w = P.Branch.watch (Repo.branch_t t) ?init f in
      fun () -> Branch_store.unwatch (Repo.branch_t t) w

    let err_not_found k =
      Fmt.kstrf invalid_arg "Branch.get: %a not found" pp_branch k

    let get t k =
      let* v = find t k in
      match v with None -> err_not_found k | Some v -> IO.return v
  end

  module Status = struct
    type t = [ `Empty | `Branch of branch | `Commit of commit ]

    let t r =
      let open Type in
      variant "status" (fun empty branch commit -> function
        | `Empty -> empty | `Branch b -> branch b | `Commit c -> commit c)
      |~ case0 "empty" `Empty
      |~ case1 "branch" Branch.t (fun b -> `Branch b)
      |~ case1 "commit" (Commit.t r) (fun c -> `Commit c)
      |> sealv

    let pp ppf = function
      | `Empty -> Fmt.string ppf "empty"
      | `Branch b -> Type.pp Branch.t ppf b
      | `Commit c -> Type.pp Hash.t ppf (Commit.hash c)
  end

  let tree_t = Tree.tree_t
  let commit_t = Commit.t
  let branch_t = Branch.t

  type kind = [ `Contents | `Node ] [@@deriving irmin]

  let ff_error_t =
    Type.enum "ff-error"
      [
        ("max-depth-reached", `Max_depth_reached);
        ("too-many-lcas", `Too_many_lcas);
        ("no-change", `No_change);
        ("rejected", `Rejected);
      ]

  let write_error_t =
    let open Type in
    variant "write-error" (fun c m e -> function
      | `Conflict x -> c x | `Too_many_retries x -> m x | `Test_was x -> e x)
    |~ case1 "conflict" string (fun x -> `Conflict x)
    |~ case1 "too-many-retries" int (fun x -> `Too_many_retries x)
    |~ case1 "test-got" (option tree_t) (fun x -> `Test_was x)
    |> sealv

  let write_error_t =
    let of_string _ = assert false in
    Type.like ~pp:pp_write_error ~of_string write_error_t
end

module Json_tree (Store : S with type contents = Contents.json) = struct
  module IO = Store.IO
  include Contents.Json_value
  open IO.Syntax

  type json = Contents.json

  let to_concrete_tree j : Store.Tree.concrete =
    let rec obj j acc =
      match j with
      | [] -> `Tree acc
      | (k, v) :: l -> (
          match Type.of_string Store.Key.step_t k with
          | Ok key -> obj l ((key, node v []) :: acc)
          | _ -> obj l acc)
    and node j acc =
      match j with
      | `O j -> obj j acc
      | _ -> `Contents (j, Store.Metadata.default)
    in
    node j []

  let of_concrete_tree c : json =
    let step = Type.to_string Store.Key.step_t in
    let rec tree t acc =
      match t with
      | [] -> `O acc
      | (k, v) :: l -> tree l ((step k, contents v []) :: acc)
    and contents t acc =
      match t with `Contents (c, _) -> c | `Tree c -> tree c acc
    in
    contents c []

  let set_tree (tree : Store.tree) key j =
    let c = to_concrete_tree j in
    let c = Store.Tree.of_concrete c in
    Store.Tree.add_tree tree key c

  let get_tree (tree : Store.tree) key =
    let* t = Store.Tree.get_tree tree key in
    let+ c = Store.Tree.to_concrete t in
    of_concrete_tree c

  let set t key j ~info =
    let* tree = set_tree Store.Tree.empty Store.Key.empty j in
    Store.set_tree_exn ~info t key tree

  let get t key =
    let* tree = Store.get_tree t key in
    get_tree tree Store.Key.empty

  let merge () = Store.Merge.run (merge ())
end
