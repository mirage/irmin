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

let src = Logs.Src.create "irmin.commit" ~doc:"Irmin commits"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (K: Type.S) = struct

  type hash = K.t

  type t = {
    node   : hash;
    parents: hash list;
    info   : Info.t;
  }

  let parents t = t.parents
  let node t = t.node
  let info t = t.info
  let v ~info ~node ~parents = { node; parents; info }

  let t =
    let open Type in
    record "commit" (fun node parents info -> { node; parents; info })
    |+ field "node"    K.t        (fun t -> t.node)
    |+ field "parents" (list K.t) (fun t -> t.parents)
    |+ field "info"    Info.t  (fun t -> t.info)
    |> sealr

  let hash_t = K.t
end

module Store
    (N: S.NODE_STORE)
    (S: sig
       include S.CONTENT_ADDRESSABLE_STORE with type key = N.key
       module Key: S.HASH with type t = key
       module Val: S.COMMIT with type t = value
                            and type hash = key
     end)
= struct

  module Node = N

  type 'a t = 'a N.t * 'a S.t
  type key = S.key
  type value = S.value

  let add (_, t) = S.add t
  let mem (_, t) = S.mem t
  let find (_, t) = S.find t
  let merge_node (t, _) = Merge.f (N.merge t)

  let pp_key = Type.pp S.Key.t

  let err_not_found k =
    Fmt.kstrf invalid_arg "Commit.get: %a not found" pp_key k

  let get (_, t) k =
    S.find t k >>= function
    | None   -> err_not_found k
    | Some v -> Lwt.return v

  let empty_if_none (n, _) = function
    | None -> N.add n N.Val.empty
    | Some node -> Lwt.return node

  let equal_opt_keys = Type.(equal (option S.Key.t))

  let merge_commit info t ~old k1 k2 =
    get t k1 >>= fun v1   ->
    get t k2 >>= fun v2   ->
    if List.mem k1 (S.Val.parents v2) then Merge.ok k2
    else if List.mem k2 (S.Val.parents v1) then Merge.ok k1
    else
      (* If we get an error while looking the the lca, then we
         assume that there is no common ancestor. Maybe we want to
         expose this to the user in a more structured way. But maybe
         that's too much low-level details. *)
      begin old () >>= function
        | Error (`Conflict msg) ->
          Log.debug (fun f -> f "old: conflict %s" msg);
          Lwt.return None
        | Ok o -> Lwt.return o
      end >>= fun old ->
      if equal_opt_keys old (Some k1) then Merge.ok k2
      else if equal_opt_keys old (Some k2) then Merge.ok k1
      else
        let old () = match old with
          | None     -> Merge.ok None
          | Some old ->
            get t old >>= fun vold ->
            Merge.ok (Some (Some (S.Val.node vold)))
        in
        merge_node t ~old (Some (S.Val.node v1)) (Some (S.Val.node v2))
        >>=* fun node ->
        empty_if_none t node >>= fun node ->
        let parents = [k1; k2] in
        let commit = S.Val.v ~node ~parents ~info:(info ()) in
        add t commit >>= fun key ->
        Merge.ok key

  let merge t ~info = Merge.(option (v S.Key.t (merge_commit info t)))

  module Key = S.Key
  module Val = S.Val
end

module History (S: S.COMMIT_STORE) = struct

  type commit = S.key
  type node = S.Node.key
  type 'a t = 'a S.t
  type v = S.Val.t

  let commit_t = S.Key.t

  let merge t ~info =
    let f ~old c1 c2 =
      let somify = Merge.map_promise (fun x -> Some x) in
      let merge = S.merge t ~info in
      Merge.f merge ~old:(somify old) (Some c1) (Some c2) >>=* function
      | None   -> Merge.conflict "History.merge"
      | Some x -> Merge.ok x
    in
    Merge.v S.Key.t f

  let v t ~node ~parents ~info =
    let commit = S.Val.v ~node ~parents ~info in
    S.add t commit >|= fun hash ->
    (hash, commit)

  let pp_key = Type.pp S.Key.t

  let parents t c =
    Log.debug (fun f -> f "parents %a" pp_key c);
    S.find t c >|= function
    | None   -> []
    | Some c -> S.Val.parents c

  module Graph = Object_graph.Make
      (S.Node.Contents.Key)(S.Node.Metadata)(S.Node.Key)(S.Key)
      (struct type t = unit let t = Type.unit end)

  let edges t =
    Log.debug (fun f -> f "edges");
    [`Node (S.Val.node t)]
    @ List.map (fun k -> `Commit k) (S.Val.parents t)

  let closure t ~min ~max =
    Log.debug (fun f -> f "closure");
    let pred = function
      | `Commit k -> (S.find t k >|= function Some r -> edges r | None -> [])
      | _         -> Lwt.return_nil in
    let min = List.map (fun k -> `Commit k) min in
    let max = List.map (fun k -> `Commit k) max in
    Graph.closure ~pred ~min ~max () >|= fun g ->
    List.fold_left (fun acc -> function
        | `Commit k -> k :: acc
        | _ -> acc
      ) [] (Graph.vertex g)

  module K = struct
    type t = S.Key.t
    let compare = Type.compare S.Key.t
    let hash = S.Key.hash
    let equal = Type.equal S.Key.t
  end
  module KSet = Set.Make(K)
  module KHashtbl = Hashtbl.Make(K)

  let read_parents t commit =
    S.find t commit >|= function
    | None   -> KSet.empty
    | Some c -> KSet.of_list (S.Val.parents c)

  let equal_keys = Type.equal S.Key.t
  let str_key k = String.sub (Type.to_string S.Key.t k) 0 4
  let pp_key = Fmt.of_to_string str_key

  let pp_keys ppf keys =
    let keys = KSet.elements keys in
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(unit " ") pp_key) keys

  let str_keys = Fmt.to_to_string pp_keys

  let lca_calls = ref 0

  let rec unqueue todo seen =
    if Queue.is_empty todo then None
    else
      let (_ , commit as pop) = Queue.pop todo in
      if KSet.mem commit seen then unqueue todo seen
      else Some pop

  (* Traverse the graph of commits using a breadth first search
     strategy. Start by visiting the commits in [init] and stops
     either when [check] returns [`Stop] or when all the ancestors of
     [init] have been visited. *)
  let traverse_bfs t ~f ~pp:_ ~check ~init ~return =
    let todo = Queue.create () in
    let add_todo d x = Queue.add (d, x) todo in
    KSet.iter (add_todo 0) init;
    let rec aux seen = match check () with
      | `Too_many_lcas | `Max_depth_reached as x -> Lwt.return (Error x)
      | `Stop -> return ()
      | `Continue ->
        match unqueue todo seen with
        | None  -> return ()
        | Some (depth, commit) ->
          (* Log.debug "lca %d: %s.%d %a"
             !lca_calls (pp_key commit) depth force (pp ()); *)
          let seen = KSet.add commit seen in
          read_parents t commit >>= fun parents ->
          let () = f depth commit parents in
          let parents = KSet.diff parents seen in
          KSet.iter (add_todo (depth+1)) parents;
          aux seen
    in
    aux KSet.empty

  (* Initially the first node is marked as [Seen1] and the second as [Seen2].
     Marks are updated as the search progresses, and may change. *)
  type mark =
    | Seen1     (* reachable from the first commit *)
    | Seen2     (* reachable from the second commit *)
    | SeenBoth  (* reachable from both, but below an LCA *)
    | LCA       (* reachable from both; candidate for the answer set *)

  let _pp_mark = function
    | Seen1 -> "seen1" | Seen2 -> "seen2"
    | SeenBoth -> "seenBoth" | LCA -> "LCA"

  (* Exploration state *)
  type state = {
    marks  : mark KHashtbl.t;            (* marks of commits already explored *)
    parents: KSet.t KHashtbl.t;        (* parents of commits already explored *)
    layers : (int, KSet.t) Hashtbl.t;    (* layers of commit, sorted by depth *)
    c1: S.key;                                             (* initial state 1 *)
    c2: S.key;                                             (* initial state 2 *)
    mutable depth: int;                      (* the current exploration depth *)
    mutable lcas : int;                   (* number of commit marked with LCA *)
    mutable complete : bool;                  (* is the exploration complete? *)
  }

  let pp_state t = lazy (
    let pp m =
      KHashtbl.fold (fun k v acc -> if v = m then str_key k :: acc else acc)
        t.marks []
      |> String.concat " "
    in
    Fmt.strf "d: %d, seen1: %s, seen2: %s, seenboth: %s, lcas: %s (%d) %s"
      t.depth (pp Seen1) (pp Seen2) (pp SeenBoth) (pp LCA) t.lcas
      (String.concat " | " (
          (Hashtbl.fold (fun d ks acc ->
               Fmt.strf "(%d: %s)" d (str_keys ks) :: acc
             ) t.layers [])))
  )

  let get_mark_exn t elt = KHashtbl.find t.marks elt
  let get_mark t elt = try Some (get_mark_exn t elt) with Not_found -> None
  let set_mark t elt mark = KHashtbl.replace t.marks elt mark
  let get_layer t d = try Hashtbl.find t.layers d with Not_found -> KSet.empty
  let add_to_layer t d k = Hashtbl.replace t.layers d (KSet.add k (get_layer t d))
  let add_parent t c p = KHashtbl.add t.parents c p
  let get_parent t c = try KHashtbl.find t.parents c with Not_found -> KSet.empty
  let incr_lcas t = t.lcas <- t.lcas + 1
  let decr_lcas t = t.lcas <- t.lcas - 1

  let both_seen t k = match get_mark t k with
    | None | Some Seen1 | Some Seen2 -> false
    | _ -> true

  let empty_state c1 c2 =
    let t =  {
      marks   = KHashtbl.create 10;
      parents = KHashtbl.create 10;
      layers  = Hashtbl.create 10;
      c1; c2; depth = 0; lcas  = 0; complete = false;
    } in
    set_mark t c1 Seen1;
    set_mark t c2 Seen2;
    t

  (* update the parent mark and keep the number of lcas up-to-date. *)
  let update_mark t mark commit =
    let new_mark = match mark, get_mark t commit with
      | Seen1, Some Seen1 | Seen1, None -> Seen1
      | Seen2, Some Seen2 | Seen2, None -> Seen2
      | SeenBoth, Some LCA -> decr_lcas t; SeenBoth
      | SeenBoth, _ -> SeenBoth
      | Seen1, Some Seen2 | Seen2, Some Seen1 -> incr_lcas t; LCA
      | _ ,Some LCA -> LCA
      | _ -> SeenBoth
    in
    (* check for fast-forwards *)
    let is_init () = equal_keys commit t.c1 || equal_keys commit t.c2 in
    let is_shared () = new_mark = SeenBoth || new_mark = LCA in
    if is_shared () && is_init () then (
      Log.debug (fun f -> f "fast-forward");
      t.complete <- true;
    );
    set_mark t commit new_mark;
    new_mark

  (* update the ancestors which have already been visisted. *)
  let update_ancestors_marks t mark commit =
    let todo = Queue.create () in
    Queue.add commit todo;
    let rec loop mark =
      if Queue.is_empty todo then ()
      else
        let a = Queue.pop todo in
        let old_mark = get_mark t a  in
        let mark = update_mark t mark a in
        let () = match old_mark with
          | Some (SeenBoth | LCA) -> ()     (* Can't be an LCA lower down *)
          | Some old when old = mark -> ()  (* No change *)
          | _ -> KSet.iter (fun x -> Queue.push x todo) (get_parent t a)
        in
        loop (if mark = LCA then SeenBoth else mark)
    in
    loop mark

  (* We are looking for LCAs, doing a breadth-first-search from the two starting commits.
     This is called each time we visit a new commit.  *)
  let update_parents t depth commit parents =
    add_parent t commit parents;
    add_to_layer t depth commit;
    if depth <> t.depth then (
      assert (depth = t.depth + 1);
      (* before starting to explore a new layer, check if we really
         have some work to do, ie. do we still have a commit seen only
         by one node? *)
      let layer = get_layer t t.depth in
      let complete = KSet.for_all (both_seen t) layer in
      if complete then t.complete <- true else t.depth <- depth
    );
    let mark = get_mark_exn t commit in
    KSet.iter (update_ancestors_marks t mark) parents

  let lcas t =
    KHashtbl.fold
      (fun k v acc -> if v = LCA then k :: acc else acc)
      t.marks []

  let check ~max_depth ~n t =
    if t.depth > max_depth then `Max_depth_reached
    else if t.lcas > n then `Too_many_lcas
    else if t.lcas = n || t.complete then `Stop
    else `Continue


  let lcas t ?(max_depth=max_int) ?(n=max_int) c1 c2 =
    incr lca_calls;
    if max_depth < 0 then Lwt.return (Error `Max_depth_reached)
    else if n <= 0 then Lwt.return (Error `Too_many_lcas)
    else if equal_keys c1 c2 then Lwt.return (Ok [c1])
    else (
      let init = KSet.of_list [c1; c2] in
      let s = empty_state c1 c2 in
      let check () = check ~max_depth ~n s in
      let pp () = pp_state s in
      let return () = Lwt.return (Ok (lcas s)) in
      let t0 = Sys.time () in
      Lwt.finalize
        (fun () -> traverse_bfs t ~f:(update_parents s) ~pp ~check ~init ~return)
        (fun () ->
           let t1 = Sys.time () -. t0 in
           Log.debug (fun f -> f "lcas %d: depth=%d time=%.4fs" !lca_calls s.depth t1);
           Lwt.return_unit)
    )

  let rec three_way_merge t ~info ?max_depth ?n c1 c2 =
    Log.debug (fun f -> f "3-way merge between %a and %a" pp_key c1 pp_key c2);
    if equal_keys c1 c2 then Merge.ok c1
    else (
      lcas t ?max_depth ?n c1 c2 >>= fun lcas ->
      let old () = match lcas with
        | Error `Too_many_lcas     -> Merge.conflict "Too many lcas"
        | Error `Max_depth_reached -> Merge.conflict "Max depth reached"
        | Ok []                    -> Merge.ok None (* no common ancestor *)
        | Ok (old :: olds)         ->
        let rec aux acc = function
          | []        -> Merge.ok (Some acc)
          | old::olds ->
            three_way_merge t ~info acc old >>=* fun acc ->
            aux acc olds
        in
        aux old olds
      in
      let merge =
        merge t ~info
        |> Merge.with_conflict
          (fun msg -> Fmt.strf "Recursive merging of common ancestors: %s" msg)
        |> Merge.f
      in
      merge ~old:old c1 c2
    )

  let lca_aux t ~info ?max_depth ?n c1 c2 =
    if equal_keys c1 c2 then Merge.ok (Some c1)
    else (
      lcas t ?max_depth ?n c1 c2 >>= function
      | Error `Too_many_lcas     -> Merge.conflict "Too many lcas"
      | Error `Max_depth_reached -> Merge.conflict "Max depth reached"
      | Ok []                    -> Merge.ok None (* no common ancestor *)
      | Ok [x]                   -> Merge.ok (Some x)
      | Ok (c :: cs)             ->
        let rec aux acc = function
          | []    -> Merge.ok (Some acc)
          | c::cs ->
            three_way_merge t ~info ?max_depth ?n acc c >>= function
            | Error (`Conflict _) -> Merge.ok None
            | Ok acc              -> aux acc cs
        in
        aux c cs
    )

  let rec lca t ~info ?max_depth ?n = function
    | []  -> Merge.conflict "History.lca: empty"
    | [c] -> Merge.ok (Some c)
    | c1::c2::cs ->
      lca_aux t ~info ?max_depth ?n c1 c2 >>=* function
      | None   -> Merge.ok None
      | Some c -> lca t ~info ?max_depth ?n (c::cs)

end
