(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

let src = Logs.Src.create "irmin.tree" ~doc:"Persistent lazy trees for Irmin"
module Log = (val Logs.src_log src : Logs.LOG)

(* assume l1 and l2 are key-sorted *)
let alist_iter2 compare_k f l1 l2 =
  let rec aux l1 l2 = match l1, l2 with
    | [], t -> List.iter (fun (key, v) -> f key (`Right v)) t
    | t, [] -> List.iter (fun (key, v) -> f key (`Left v)) t
    | (k1,v1)::t1, (k2,v2)::t2 ->
      match compare_k k1 k2 with
      | 0 ->
        f k1 (`Both (v1, v2));
        aux t1 t2
      | x -> if x < 0 then (
          f k1 (`Left v1);
          aux t1 l2
        ) else (
          f k2 (`Right v2);
          aux l1 t2
        )
  in
  aux l1 l2

(* assume l1 and l2 are key-sorted *)
let alist_iter2_lwt compare_k f l1 l2 =
  let l3 = ref [] in
  alist_iter2 compare_k (fun left right ->
      l3 := f left right :: !l3
    ) l1 l2;
  Lwt_list.iter_p
    (fun b -> b >>= fun () -> Lwt.return_unit) (List.rev !l3)

module Make (P: S.PRIVATE) = struct

  module Path = P.Node.Path
  module StepMap = struct
    module X = struct
      type t = Path.step
      let t = Path.step_t
      let compare = Type.compare Path.step_t
    end
    include Map.Make(X)
    include Merge.Map(X)
  end

  module Metadata = P.Node.Metadata

  type key = Path.t
  type step = Path.step
  type contents = P.Contents.value
  type repo = P.Repo.t

  module Contents = struct

    type key = P.Contents.key

    type value =
      | Key     : repo * key -> value
      | Contents: contents -> value
      | Both    : repo * key * contents -> value

    type t = { mutable v: value }
    (* Same as [Contents.t] but can either be a raw contents or a key
       that will be fetched lazily. *)

    let value =
      let open Type in
      variant "Node.Contents" (fun key contents both -> function
          | Key (_, x)     -> key x
          | Contents x     -> contents x
          | Both (_, x, y) -> both (x, y))
      |~ case1 "Key" P.Contents.Key.t (fun _ -> assert false)
      |~ case1 "Contents" P.Contents.Val.t (fun x -> Contents x)
      |~ case1 "Both" (pair P.Contents.Key.t P.Contents.Val.t)
        (fun _ -> assert false)
      |> sealv

    let t = Type.like value (fun v -> { v }) (fun t -> t.v)

    let of_contents c = { v = Contents c }
    let of_key db k = { v = Key (db, k) }

    let key c = match c.v with
      | Both (_, k, _) | Key (_, k) -> k
      | Contents v -> P.Hash.digest (Type.encode_bin P.Contents.Val.t v)

    let v t = match t.v with
      | Both (_, _, c)
      | Contents c -> Lwt.return (Some c)
      | Key (db, k) ->
        P.Contents.find (P.Repo.contents_t db) k >|= function
        | None   -> None
        | Some c -> t.v <- Both (db, k, c); Some c

    let equal (x:t) (y:t) =
      x == y ||
      Type.equal P.Contents.Key.t (key x) (key y)

    let merge: t Merge.t =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              v old >|= fun c ->
              Ok (Some c)
            )
        in
        v x >>= fun x ->
        v y >>= fun y ->
        Merge.(f P.Contents.Val.merge) ~old x y >|= function
        | Ok (Some c)  -> Ok (of_contents c)
        | Ok None      -> Error (`Conflict "empty contents")
        | Error _ as e -> e
      in
      Merge.v t f

    let fold ~force ~path f t acc =
      let aux = function
        | None   -> Lwt.return acc
        | Some c -> f path c acc
      in
      match force with
      | `True -> v t >>= aux
      | `False skip ->
        match t.v with
        | Key _ -> skip path acc
        | Both (_, _, c) | Contents c -> aux (Some c)

    let clear_caches t = match t.v with
      | Key _ | Contents _ -> ()
      | Both (r, k, _) -> t.v <- Key (r, k)

  end

  module Node = struct

    type key = P.Node.key

    type value = [ `Node of t | `Contents of Contents.t * Metadata.t ]

    and map = value StepMap.t

    and node =
      | Map : map -> node
      | Key : repo * key -> node
      | Both: repo * key * map -> node

    and t = { mutable v: node }

    let value t =
      let open Type in
      variant "Node.value" (fun node contents -> function
          | `Node x     -> node x
          | `Contents x -> contents x)
      |~ case1 "Node" t (fun x -> `Node x)
      |~ case1 "Contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
      |> sealv

    let map value =
      let open Type in
      let to_map x =
        List.fold_left (fun acc (k, v) -> StepMap.add k v acc) StepMap.empty x
      in
      let of_map m = StepMap.fold (fun k v acc -> (k, v) :: acc) m [] in
      like (list (pair Path.step_t value)) to_map of_map

    let node map =
      let open Type in
      variant "Node.node" (fun map key both -> function
          | Map x        -> map x
          | Key (_,y)    -> key y
          | Both (_,y,z) -> both (y, z))
      |~ case1 "Map" map (fun x -> Map x)
      |~ case1 "Key" P.Node.Key.t (fun _ -> assert false)
      |~ case1 "Both" (pair P.Node.Key.t map) (fun _ -> assert false)
      |> sealv

    let t node = Type.like node (fun v -> { v }) (fun t -> t.v)

    let _, t = Type.mu2 (fun _ y ->
        let value = value y in
        let node = node (map value) in
        let t = t node in
        node, t
      )

    let value_t = value t
    let dump = Type.pp_json ~minify:false t

    let of_map map = { v = Map map }
    let of_key repo k = { v = Key (repo, k) }
    let empty = of_map StepMap.empty

    let import t n =
      let alist = P.Node.Val.list n in
      let alist = List.map (fun (l, x) ->
          l, match x with
          | `Contents (c, m) -> `Contents (Contents.of_key t c, m)
          | `Node n          -> `Node (of_key t n)
        ) alist in
      List.fold_left (fun acc (l, x) -> StepMap.add l x acc) StepMap.empty alist

    let rec key t =
      match t.v with
      | Key (_, k) | Both (_, k, _) -> k
      | Map m -> key_of_map m

    and export_map map =
      let alist =
        StepMap.fold (fun step v acc ->
            let v = match v with
              | `Contents (c, m) -> `Contents (Contents.key c, m)
              | `Node n          -> `Node (key n)
            in
            (step, v) :: acc
          ) map []
      in
      P.Node.Val.v alist

    and key_of_map map =
      let v = export_map map in
      P.Hash.digest (Type.encode_bin P.Node.Val.t v)

    let to_map t = match t.v with
      | Map m | Both (_, _, m) -> Lwt.return (Some m)
      | Key (db, k) ->
        Log.debug (fun l -> l "Node.to_map %a" (Type.pp P.Node.Key.t) k);
        P.Node.find (P.Repo.node_t db) k >|= function
        | None   -> None
        | Some n ->
          let n = import db n in
          t.v <- Both (db, k, n);
          Some n

    let key_equal x y =
      x == y ||
      Type.equal P.Node.Key.t x y

    let contents_equal (c1, m1 as x1) (c2, m2 as x2) =
      x1 == x2 ||
      (Contents.equal c1 c2 && Type.equal Metadata.t m1 m2)

    let equal (x:t) (y:t) =
      x == y ||
      key_equal (key x) (key y)

    let is_empty t =
      to_map t >|= function
      | None   -> false
      | Some m -> StepMap.is_empty m

    let list t =
      to_map t >|= function
      | None   -> []
      | Some m ->
        let kvs = StepMap.bindings m in
        List.map (fun (k, v) ->
            k, match v with
            | `Contents _ -> `Contents
            | `Node _     -> `Node
          ) kvs

    let listv t =
      to_map t >|= function
      | None   -> []
      | Some m -> StepMap.bindings m

    let findv t step =
      to_map t >>= function
      | None   -> Lwt.return None
      | Some m ->
        match StepMap.find step m with
        | exception Not_found -> Lwt.return None
        | `Node n             -> Lwt.return (Some (`Node n))
        | `Contents (c, m)     ->
          Contents.v c >|= function
          | None   -> None
          | Some c -> Some (`Contents (c, m))

    module Keys = Hashtbl.Make(struct
        type t = P.Node.key
        let hash = P.Node.Key.hash
        let equal = Type.equal P.Node.Key.t
      end)

    let dummy_marks = Keys.create 0
    type marks = unit Keys.t
    let empty_marks () = Keys.create 39

    let fold ~force ~uniq ~pre ~post ~path f t acc =
      let marks = match uniq with
        | `False   -> dummy_marks
        | `True    -> empty_marks ()
        | `Marks n -> n
      in
      let rec aux ~path acc t k = match force with
        | `True -> to_map t >>= fun m -> map ~path acc m k
        | `False skip ->
          match t.v with
          | Key _ -> skip path acc
          | Both (_, _, n) | Map n -> map ~path acc (Some n) k
      and aux_uniq ~path acc t k =
        if uniq = `False then aux ~path acc t k
        else
          let key = key t in
          if Keys.mem marks key then k acc
          else (
            Keys.add marks key ();
            aux ~path acc t k
          )
      and step ~path acc (s, v) k =
        let path = Path.rcons path s in
        match v with
        | `Contents c -> Contents.fold ~force ~path f (fst c) acc >>= k
        | `Node n     -> aux_uniq ~path acc n k
      and steps ~path acc s k = match s with
        | []   -> k acc
        | h::t -> step ~path acc h (fun acc -> steps ~path acc t k)
      and map ~path acc m k = match m with
        | None   -> k acc
        | Some m ->
          let bindings = StepMap.bindings m in
          let s = List.map fst bindings in
          pre path s acc >>= fun acc ->
          steps ~path acc bindings (fun acc -> post path s acc >>= k)
      in
      aux_uniq ~path acc t Lwt.return

    let remove t step =
      to_map t >|= function
      | None   -> t
      | Some n ->
        if not (StepMap.mem step n) then t
        else of_map (StepMap.remove step n)

    let add t step = function
      | `Node _ | `Contents _ as v ->
        let v = match v with
          | `Node _ as n            -> (fun _ -> n)
          | `Contents (`Set (c, m)) ->
            (fun _ -> `Contents (Contents.of_contents c, m))
          | `Contents (`Keep c) ->
            (function
              | Some m -> `Contents (Contents.of_contents c, m)
              | None   -> `Contents (Contents.of_contents c, Metadata.default))
        in
        to_map t >>= function
        | None   ->
          v (Some Metadata.default)
          |> StepMap.singleton step
          |> of_map
          |> Lwt.return
        | Some m ->
          let previous =
            try Some (StepMap.find step m)
            with Not_found -> None
          in
          let previous_m = match previous with
            | None | Some (`Node _)   -> None
            | Some (`Contents (_, m)) -> Some m
          in
          let v = v previous_m in
          Lwt.return (of_map (StepMap.add step v m))

    let rec merge () =
      let f ~old x y =
        let old =
          Merge.bind_promise old (fun old () ->
              to_map old >|= fun map -> Ok (Some map)
            )
        in
        to_map x >>= fun x ->
        to_map y >>= fun y ->
        let m = StepMap.merge value_t (fun _step ->
            Merge.option (merge_value ())
          ) in
        Merge.(f @@ option m) ~old x y
        >|= function
        | Ok (Some map) -> Ok (of_map map)
        | Ok None       -> Error (`Conflict "empty map")
        | Error _ as e  -> e
      in
      Merge.v t f

    and merge_value () =
      let open Merge.Infix in
      let f ~old x y =
        match x, y with
        | `Contents (x, cx), `Contents (y, cy) ->
          let mold =
            Merge.bind_promise old (fun old () ->
                match old with
                | `Contents (_, m) -> Lwt.return (Ok (Some m))
                | `Node _          -> Lwt.return (Ok None)
              )
          in
          Merge.(f Metadata.merge) ~old:mold cx cy >>=* fun m ->
          let old =
            Merge.bind_promise old (fun old () ->
                match old with
                | `Contents (c, _) -> Lwt.return (Ok (Some c))
                | `Node _          -> Lwt.return (Ok None)
              )
          in
          Merge.(f Contents.merge) ~old x y >>=* fun c ->
          Merge.ok (`Contents (c, m))
        | `Node x, `Node y ->
          let old =
            Merge.bind_promise old (fun old () ->
                match old with
                | `Contents _ -> Lwt.return (Ok None)
                | `Node n     -> Lwt.return (Ok (Some n))
              )
          in
          Merge.(f @@ merge ()) ~old x y >>=* fun n ->
          Merge.ok (`Node n)
        | _ -> Merge.conflict "add/add values"
      in
      Merge.seq [
        Merge.default value_t;
        Merge.v value_t f
      ]

    let merge_value = merge_value ()

    let rec clear_caches t = match t.v with
      | Key _          -> ()
      | Both (r, k, _) -> t.v <- Key (r, k)
      | Map m          ->
        StepMap.iter (fun _ -> function
            | `Contents (c, _) -> Contents.clear_caches c
            | `Node n          -> clear_caches n
          ) m

    (*
    let contents  =
      let open Type in
      variant "Node.contents" (fun keep set -> function
          | `Keep x -> keep x
          | `Set x  -> set x)
      |~ case1 "Keep" P.Contents.Val.t (fun x -> `Keep x)
      |~ case1 "Set" (pair P.Contents.Val.t Metadata.t) (fun x -> `Set x)
      |> sealv

    type update = [
      | `Contents of [ `Keep of contents | `Set of contents * Metadata.t ]
      | `Node of t ]

    let update: update Type.t =
      let open Type in
      variant "Node.update" (fun contents node -> function
          | `Contents x -> contents x
          | `Node x     -> node x)
      |~ case1 "Contents" contents (fun x -> `Contents x)
      |~ case1 "Node" t (fun x -> `Node x)
      |> sealv
    *)
  end

  type node = Node.t
  type metadata = Metadata.t
  type tree = [ `Node of node | `Contents of contents * metadata ]

  let node_t = Node.t

  let tree_t =
    let open Type in
    variant "tree" (fun node contents -> function
        | `Node n     -> node n
        | `Contents c -> contents c)
    |~ case1 "node" Node.t (fun n -> `Node n)
    |~ case1 "contents" (pair P.Contents.Val.t Metadata.t) (fun c -> `Contents c)
    |> sealv

  let dump ppf = function
    | `Node n          -> Fmt.pf ppf "node: %a" Node.dump n
    | `Contents (c, _) -> Fmt.pf ppf "contents: %a" (Type.pp P.Contents.Val.t) c

  let contents_equal (c1, m1 as x1) (c2, m2 as x2) =
    x1 == x2 ||
    (Type.equal P.Contents.Val.t c1 c2 && Type.equal Metadata.t m1 m2)

  let equal (x:tree) (y:tree) =
    x == y ||
    match x, y with
    | `Node x    , `Node y     -> Node.equal x y
    | `Contents x, `Contents y -> contents_equal x y
    | `Node _    , `Contents _
    | `Contents _, `Node _     -> false

  let empty = `Node Node.empty
  let is_empty = function
    | `Node n     -> Node.is_empty n
    | `Contents _ -> Lwt.return false

  let of_node n = `Node n
  let of_contents ?(metadata=Metadata.default) c = `Contents (c, metadata)

  let clear_caches = function
    | `Node n     -> Node.clear_caches n
    | `Contents _ -> ()

  let sub t path =
    let rec aux node path =
      match Path.decons path with
      | None        -> Lwt.return (Some node)
      | Some (h, p) ->
        Node.findv node h >>= function
        | None | Some (`Contents _) -> Lwt.return_none
        | Some (`Node n) -> aux n p
    in
    match t with
    | `Node n     -> aux n path
    | `Contents _ -> Lwt.return_none

  let pp_path = Type.pp Path.t

  let find_tree (t:tree) path =
    Log.debug (fun l -> l "Tree.find_tree %a" pp_path path);
    match t, Path.rdecons path with
    | v, None              -> Lwt.return (Some v)
    | _, Some (path, file) ->
      sub t path >>= function
      | None   -> Lwt.return None
      | Some n -> Node.findv n file

  type marks = Node.marks
  let empty_marks = Node.empty_marks
  type 'a force = [`True | `False of (key -> 'a -> 'a Lwt.t)]
  type uniq = [`False | `True | `Marks of marks]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  let id _ _ acc = Lwt.return acc

  let fold ?(force=`True) ?(uniq=`False) ?(pre=id) ?(post=id) f (t:tree) acc =
    match t with
    | `Contents v -> f Path.empty (fst v) acc
    | `Node n     -> Node.fold ~force ~uniq ~pre ~post ~path:Path.empty f n acc

  type stats = {
    nodes: int;
    leafs: int;
    skips: int;
    depth: int;
    width: int;
  }

  let empty_stats = { nodes=0; leafs=0; skips=0; depth=0; width=0 }

  let pp_stats ppf { nodes; leafs; skips; depth; width; } =
    Fmt.pf ppf "{@[nodes=%d; leafs=%d; skips=%d; depth=%d; width=%d]}"
      nodes leafs skips depth width

  let incr_nodes s = { s with nodes = s.nodes + 1 }
  let incr_leafs s = { s with leafs = s.leafs + 1 }
  let incr_skips s = { s with skips = s.skips + 1 }

  let set_depth p s =
    let n_depth = List.length (Path.map p (fun _ -> ())) in
    let depth = max n_depth s.depth in
    { s with depth }

  let set_width childs s =
    let width = max s.width (List.length childs) in
    { s with width }

  let stats ?(force=false) (t:tree) =
    let force =
      if force then `True
      else `False (fun k s -> set_depth k s |> incr_skips |> Lwt.return)
    in
    let f k _ s = set_depth k s |> incr_leafs |> Lwt.return in
    let pre k childs s =
      if childs = [] then Lwt.return s
      else
        set_depth k s |>
        set_width childs |>
        incr_nodes |>
        Lwt.return
    in
    let post _ _ acc = Lwt.return acc in
    fold ~force ~pre ~post f t empty_stats

  let err_not_found n k =
    Fmt.kstrf invalid_arg "Irmin.Tree.%s: %a not found" n pp_path k

  let get_tree (t:tree) path =
    find_tree t path >|= function
    | None   -> err_not_found "get_tree" path
    | Some v -> v

  let find_all t k =
    find_tree t k >|= function
    | None
    | Some (`Node _)     -> None
    | Some (`Contents c) -> Some c

  let find t k =
    find_all t k >|= function
    | None -> None
    | Some (c, _) -> Some c

  let get_all t k =
    find_all t k >>= function
    | None   -> err_not_found "get" k
    | Some v -> Lwt.return v

  let get t k = get_all t k >|= fun (c, _) -> c

  let mem t k =
    find t k >|= function
    | None -> false
    | _    -> true

  let mem_tree t k =
    find_tree t k >|= function
    | None -> false
    | _    -> true

  let kind t path =
    Log.debug (fun l -> l "Tree.kind %a" pp_path path);
    match t, Path.rdecons path with
    | `Contents _, None -> Lwt.return (Some `Contents)
    | _          , None -> Lwt.return None
    | _          , Some (dir, file) ->
      sub t dir >>= function
      | None   -> Lwt.return None
      | Some m ->
        Node.findv m file >>= function
        | None               -> Lwt.return None
        | Some (`Contents _) -> Lwt.return (Some `Contents)
        | Some (`Node _)     -> Lwt.return (Some `Node)

  let list t path =
    Log.debug (fun l -> l "Tree.list %a" pp_path path);
    sub t path >>= function
    | None   -> Lwt.return []
    | Some n -> Node.list n

  let may_remove t k =
    Node.findv t k >>= function
    | None -> Lwt.return_none
    | Some _ -> Node.remove t k >>= fun t -> Lwt.return (Some t)

  let remove t k =
    Log.debug (fun l -> l "Tree.remove %a" pp_path k);
    match Path.rdecons k with
    | None ->
      is_empty t >>= fun is_empty ->
      if is_empty then Lwt.return t else Lwt.return empty
    | Some (path, file) ->
      let rec aux view path : Node.t option Lwt.t =
        match Path.decons path with
        | None        -> may_remove view file
        | Some (h, p) ->
          Node.findv view h >>= function
          | None | Some (`Contents _) -> Lwt.return_none
          | Some (`Node child) ->
            aux child p >>= function
            | None -> Lwt.return_none
            | Some child' ->
              (* remove empty dirs *)
              Node.is_empty child' >>= function
              | true  -> may_remove view h
              | false ->
                Node.add view h (`Node child') >>= fun t ->
                Lwt.return (Some t)
      in
      let n = match t with `Node n -> n | _ -> Node.empty in
      aux n path >>= function
      | None -> Lwt.return t
      | Some node -> Lwt.return (`Node node)

  let with_setm = function
    | `Node _ as n -> n
    | `Contents c  -> `Contents (`Set c)

  let add_tree t k v =
    Log.debug (fun l -> l "Tree.add_tree %a" pp_path k);
    match Path.rdecons k with
    | None              -> Lwt.return v
    | Some (path, file) ->
      let rec aux view path =
        match Path.decons path with
        | None        -> begin
            Node.findv view file >>= function old ->
            match old with
            | Some old ->
              if equal old v then Lwt.return_none
              else
                Node.add view file (with_setm v) >|= fun t ->
                Some t
            | None ->
              Node.add view file (with_setm v) >|= fun t ->
              Some t
          end
        | Some (h, p) ->
          Node.findv view h >>= function
          | None | Some (`Contents _) -> begin
              aux Node.empty p >>= function
              | None -> Lwt.return_none
              | Some child' ->
                Node.add view h (`Node child') >|= fun t ->
                Some t
            end
          | Some (`Node child) ->
            aux child p >>= function
            | None -> Lwt.return_none
            | Some child' ->
              Node.add view h (`Node child') >|= fun t ->
              Some t
      in
      let n = match t with `Node n -> n | _ -> Node.empty in
      aux n path >|= function
      | None -> t
      | Some node -> `Node node

  let with_optm m c = match m with
    | None   -> `Contents (`Keep c)
    | Some m -> `Contents (`Set (c, m))

  let add t k ?metadata c =
    Log.debug (fun l -> l "Tree.add %a" pp_path k);
    match Path.rdecons k with
    | None ->
      (match metadata, t with
       | None, `Contents (c', _)
         when Type.equal P.Contents.Val.t c' c -> Lwt.return t
       | None, `Contents (_, m)   -> Lwt.return (`Contents (c, m))
       | None, _ -> Lwt.return (`Contents (c, Metadata.default))
       | Some m, `Contents c' when contents_equal c' (c, m) -> Lwt.return t
       | Some m, _ -> Lwt.return (`Contents (c, m)))
    | Some (path, file) ->
      let rec aux view path =
        match Path.decons path with
        | None        -> begin
            Node.findv view file >>= function old ->
            match old with
            | Some (`Node _) | None ->
              Node.add view file (with_optm metadata c) >>= fun t ->
              Lwt.return (Some t)
            | Some (`Contents (_, oldm) as old) ->
              let m = match metadata with None -> oldm | Some m -> m in
              if equal old (`Contents (c,  m)) then Lwt.return_none
              else
                Node.add view file (`Contents (`Set (c,  m))) >|= fun t ->
                Some t
          end
        | Some (h, p) ->
          Node.findv view h >>= function
          | None | Some (`Contents _) -> begin
              aux Node.empty p >>= function
              | None -> assert false
              | Some child ->
                Node.add view h (`Node child) >|= fun t ->
                Some t
            end
          | Some (`Node child) ->
            aux child p >>= function
            | None -> Lwt.return_none
            | Some child' ->
              Node.add view h (`Node child') >|= fun t ->
              Some t
      in
      let n = match t with `Node n -> n | _ -> Node.empty in
      aux n path >>= function
      | None      -> Lwt.return t
      | Some node -> Lwt.return (`Node node)

  let import repo k = Node.of_key repo k

  let export repo contents_t node_t n =
    let node n = P.Node.add node_t (Node.export_map n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match n.Node.v with
      | Node.Both (repo, k, x) when StepMap.is_empty x ->
        Stack.push (fun () ->
            P.Node.mem  (P.Repo.node_t repo) k >>= function
            | true  -> Lwt.return_unit
            | false ->
              node x >>= fun k ->
              n.Node.v <- Node.Both (repo, k, x);
              Lwt.return_unit
          ) todo
      | (Node.Key _ | Node.Both _ ) -> ()
      | Node.Map x ->
        (* 1. we push the current node job on the stack. *)
        Stack.push (fun () ->
            node x >>= fun k ->
            n.Node.v <- Node.Both (repo, k, x);
            Lwt.return_unit
          ) todo;
        let contents = ref [] in
        let nodes = ref [] in
        StepMap.iter (fun _ -> function
            | `Contents c -> contents := c :: !contents
            | `Node n     -> nodes := n :: !nodes
          ) x;
        (* 2. we push the contents job on the stack. *)
        List.iter (fun (c, _) ->
            match c.Contents.v with
            | Contents.Both _
            | Contents.Key _       -> ()
            | Contents.Contents x  ->
              Stack.push (fun () ->
                  P.Contents.add contents_t x >|= fun k ->
                  c.Contents.v <- Contents.Both (repo, k, x);
                ) todo
          ) !contents;
        (* 3. we push the children jobs on the stack. *)
        List.iter (fun n ->
            Stack.push (fun () -> add_to_todo n; Lwt.return_unit) todo
          ) !nodes
    in
    let rec loop () =
      let task =
        try Some (Stack.pop todo)
        with Stack.Empty -> None
      in
      match task with
      | None   -> Lwt.return_unit
      | Some t -> t () >>= loop
    in
    add_to_todo n;
    loop () >|= fun () ->
    let x = Node.key n in
    Log.debug (fun l -> l "Tree.export -> %a" (Type.pp P.Node.Key.t) x);
    x

  let merge: tree Merge.t =
    let f ~old (x:tree) y =
      let to_node x =
        match x with
        | `Node _ as x     -> x
        | `Contents (c, m) -> `Contents (Contents.of_contents c, m)
      in
      let x = to_node x in
      let y = to_node y in
      let old = Merge.bind_promise old (fun old ->
          Merge.promise (to_node old)
        ) in
      Merge.(f Node.merge_value) ~old x y >>= function
      | Ok (`Contents (c, m)) ->
        ( Contents.v c >>= function
            | None   -> Merge.conflict "conflict: contents"
            | Some c -> Merge.ok (`Contents (c, m)) )
      | Ok (`Node _ as n) -> Merge.ok n
      | Error _ as e -> Lwt.return e
    in
    Merge.v tree_t f

  let entries path tree =
    let rec aux acc = function
      | []              -> Lwt.return acc
      | (path, h)::todo ->
        Node.listv h >>= fun childs ->
        let acc, todo =
          List.fold_left (fun (acc, todo) (k, v) ->
              let path = Path.rcons path k in
              match v with
              | `Node v     -> acc, (path, v) :: todo
              | `Contents c -> (path, c) :: acc, todo
            ) (acc, todo) childs
        in
        aux acc todo
    in
    aux [] [path, tree]

  let diff_node (x:node) (y:node) =
    let bindings n =
      Node.to_map n >|= function
      | None   -> []
      | Some m -> StepMap.bindings m
    in
    let removed acc (k, (c, m)) =
      Contents.v c >|= function
      | None   -> acc
      | Some c -> (k, `Removed (c, m)) :: acc
    in
    let added acc (k, (c, m)) =
      Contents.v c >|= function
      | None   -> acc
      | Some c -> (k, `Added (c, m)) :: acc
    in
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (path, x, y) :: todo ->
        if Node.equal x y then aux acc todo
        else
          bindings x >>= fun x ->
          bindings y >>= fun y ->
          let acc = ref acc in
          let todo = ref todo in
          alist_iter2_lwt Type.(compare @@ Path.step_t) (fun key v ->
              let path = Path.rcons path key in
              match v with

              (* Left *)
              | `Left (`Contents x)  ->
                removed !acc (path, x) >|= fun x ->
                acc := x
              | `Left (`Node x) ->
                entries path x >>= fun xs ->
                Lwt_list.fold_left_s removed !acc xs >|= fun xs ->
                acc := xs

              (* Right *)
              | `Right (`Contents y) ->
                added !acc (path, y) >|= fun y ->
                acc := y
              | `Right (`Node y) ->
                entries path y >>= fun ys ->
                Lwt_list.fold_left_s added !acc ys >|= fun ys ->
                acc := ys

              (* Both *)
              | `Both (`Node x, `Node y) ->
                todo := (path, x , y) :: !todo;
                Lwt.return_unit

              | `Both (`Contents x, `Node y) ->
                entries path y >>= fun ys ->
                removed !acc (path, x) >>= fun x ->
                Lwt_list.fold_left_s added x ys >|= fun ys ->
                acc := ys

              | `Both (`Node x, `Contents y) ->
                entries path x >>= fun xs ->
                added !acc (path, y) >>= fun y ->
                Lwt_list.fold_left_s removed y xs >|= fun ys ->
                acc := ys

              | `Both (`Contents x, `Contents y) ->
                if Node.contents_equal x y then Lwt.return_unit
                else
                  Contents.v (fst x) >>= fun cx ->
                  Contents.v (fst y) >|= fun cy ->
                  match cx, cy with
                  | None   , None    -> ()
                  | Some cx, None    ->
                    let x = cx, snd x in
                    acc := (path, `Removed x) :: !acc
                  | None   , Some cy ->
                    let y = cy, snd y in
                    acc := (path, `Added y) :: !acc
                  | Some cx, Some cy ->
                    let x = cx, snd x in
                    let y = cy, snd y in
                    acc := (path, `Updated (x, y)) :: !acc
            ) x y
          >>= fun () ->
          aux !acc !todo
    in
    aux [] [Path.empty, x, y]

  let diff (x:tree) (y:tree) = match x, y with
    | `Contents x, `Contents y ->
      if contents_equal x y then Lwt.return []
      else Lwt.return [ Path.empty, `Updated (y, x) ]
    | `Node x    , `Node y    -> diff_node x y
    | `Contents x, `Node y    ->
      diff_node Node.empty y >|= fun diff -> (Path.empty, `Removed x) :: diff
    | `Node x    , `Contents y ->
      diff_node x Node.empty >|= fun diff -> (Path.empty, `Added y) :: diff

  type concrete =
    [ `Tree of (step * concrete) list
    | `Contents of contents * metadata ]

  let of_concrete c =
    let rec concrete k = function
      | `Contents _ as v -> k v
      | `Tree childs -> tree StepMap.empty (fun n -> k (`Node n)) childs
    and contents k (c, m) =
      k (`Contents (Contents.of_contents c, m))
    and tree map k = function
      | []        -> k (Node.of_map map)
      | (s, n)::t -> concrete (function
          | `Contents c  -> contents (fun v -> tree (StepMap.add s v map) k t) c
          | `Node _ as v -> tree (StepMap.add s v map) k t
        ) n
    in
    concrete (fun x -> x) c

  let to_concrete t =
    let rec tree k = function
      | `Contents _ as v -> k v
      | `Node n ->
        Node.to_map n >>= function
        | None   -> k (`Tree [])
        | Some n -> node [] (fun n -> k (`Tree n)) (StepMap.bindings n)
    and contents k (c, m) =
      Contents.v c >>= function
      | None   -> k None
      | Some c -> k @@ Some (`Contents (c, m))
    and node childs k = function
      | []          -> k childs
      | (s, n) :: t -> match n with
        | `Node _ as n -> tree (fun tree -> node ((s, tree) :: childs) k t) n
        | `Contents c  -> contents (function
            | None   -> node childs k t
            | Some c -> node ((s, c) :: childs) k t
          ) c
    in
    tree (fun x -> Lwt.return x) t

  let hash (t:tree) = match t with
    | `Node n -> `Node (Node.key n)
    | `Contents (c, m) ->
      let str = Type.encode_bin P.Contents.Val.t c in
      `Contents (P.Hash.digest str, m)
end
