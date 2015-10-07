(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

open Lwt.Infix
open Ir_misc.OP
open Ir_merge.OP

module Log = Log.Make(struct let section = "NODE" end)

module type S = sig
  include Tc.S0
  type contents
  type node
  type step

  val create: (step * [`Contents of contents | `Node of node]) list -> t
  val alist: t -> (step * [`Contents of contents | `Node of node]) list

  val empty: t
  val is_empty: t -> bool

  val contents: t -> step -> contents option
  val iter_contents: t -> (step -> contents -> unit) -> unit
  val with_contents: t -> step -> contents option -> t

  val succ: t -> step -> node option
  val iter_succ: t -> (step -> node -> unit) -> unit
  val with_succ: t -> step -> node option -> t
end

module Make (K_c: Tc.S0) (K_n: Tc.S0) (P: Ir_path.S) = struct

  type contents = K_c.t
  type node = K_n.t
  type step = P.step

  module Path = P
  module StepMap = Ir_misc.Map(P.Step)

  module X = struct

    type t = [`Contents of contents | `Node of node ]

    let compare x y = match x, y with
      | `Contents x, `Contents y -> K_c.compare x y
      | `Node x    , `Node y     -> K_n.compare x y
      | `Contents _, _           -> 1
      | _ -> -1

    let equal x y = match x, y with
      | `Contents x, `Contents y -> K_c.equal x y
      | `Node x    , `Node y     -> K_n.equal x y
      | _ -> false

    let hash = Hashtbl.hash

    let to_json = function
      | `Contents c -> `O [ "contents", K_c.to_json c ]
      | `Node n     -> `O [ "node"    , K_n.to_json n ]

    let of_json = function
      | `O [ "contents", j ] -> `Contents (K_c.of_json j)
      | `O [ "node"    , j ] -> `Node (K_n.of_json j)
      | j -> Ezjsonm.parse_error j "Node.of_json"

    let write t buf = match t with
      | `Contents x -> K_c.write x (Ir_misc.tag buf 0)
      | `Node x -> K_n.write x (Ir_misc.tag buf 1)

    let size_of t = 1 + match t with
      | `Contents x -> K_c.size_of x
      | `Node x -> K_n.size_of x

    let read buf = match Ir_misc.untag buf with
      | 0 -> `Contents (K_c.read buf)
      | 1 -> `Node (K_n.read buf)
      | n -> Tc.Reader.error "Vertex.read parse error (tag=%d)" n

  end

  type t = {
    contents: contents StepMap.t Lazy.t;
    succ    : node StepMap.t Lazy.t;
    alist   : (step * X.t) list;
  }

  let alist t = t.alist

  let mk_index alist =
    lazy (
      List.fold_left (fun (contents, succ) (l, x) ->
          match x with
          | `Contents c -> StepMap.add l c contents, succ
          | `Node n     -> contents, StepMap.add l n succ
        ) (StepMap.empty, StepMap.empty) alist
    )

  let create alist =
    let maps = mk_index alist in
    let contents = lazy (fst (Lazy.force maps)) in
    let succ = lazy (snd (Lazy.force maps)) in
    { contents; succ; alist }

  let iter_contents t f =
    List.iter (fun (l, x) -> match x with
        | `Contents c -> f l c
        | `Node _     -> ()
      ) t.alist

  let iter_succ t f =
    List.iter (fun (l, x) -> match x with
        | `Node n     -> f l n
        | `Contents _ -> ()
      ) t.alist

  let contents t l =
    try Some (StepMap.find l (Lazy.force t.contents))
    with Not_found -> None

  let succ t l =
    try Some (StepMap.find l (Lazy.force t.succ))
    with Not_found -> None

  module Y = Tc.List (Tc.Pair (P.Step)(X) )

  let to_json t = Y.to_json t.alist
  let of_json j = create (Y.of_json j)
  let write t = Y.write t.alist
  let size_of t = Y.size_of t.alist
  let read b = create (Y.read b)
  let hash t = Y.hash t.alist
  let compare x y = Y.compare x.alist y.alist
  let equal x y = Y.equal x.alist y.alist

  let empty =
    { contents = lazy StepMap.empty;
      succ = lazy StepMap.empty;
      alist = []; }

  let is_empty e = e.alist = []

  let with_contents t step contents =
    let return ~acc rest = match contents with
      | None   -> t.alist
      | Some c -> List.rev_append acc ((step, `Contents c) :: rest)
    in
    let rec aux acc = function
      | [] -> return ~acc []
      | (s, x as h) :: l ->
        if P.Step.equal step s then match contents with
          | None   -> List.rev_append acc l (* remove *)
          | Some c ->
            match x with
            | `Contents x -> if K_c.equal c x then t.alist else return ~acc l
            | `Node _     -> return ~acc l
        else aux (h :: acc) l
    in
    let alist = aux [] t.alist in
    if t.alist == alist then t else create alist

  let with_succ t step succ =
    let return ~acc rest = match succ with
      | None   -> t.alist
      | Some s -> List.rev_append acc ((step, `Node s) :: rest)
    in
    let rec aux acc = function
      | [] -> return ~acc []
      | (s, x as h) :: l ->
        if P.Step.equal step s then match succ with
          | None   -> List.rev_append acc l (* remove *)
          | Some c ->
            match x with
            | `Node x     -> if K_n.equal c x then t.alist else return ~acc l
            | `Contents _ -> return ~acc l
        else aux (h :: acc) l
    in
    let alist = aux [] t.alist in
    if t.alist == alist then t else create alist

end

module type STORE = sig
  include Ir_ao.STORE
  module Path: Ir_path.S
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type node = key
     and type step = Path.step
end

module type GRAPH = sig
  type t
  type contents
  type node
  type step
  type path

  val empty: t -> node Lwt.t
  val create: t -> (step * [`Contents of contents | `Node of node]) list -> node Lwt.t

  val contents: t -> node -> step -> contents option Lwt.t
  val succ: t -> node -> step -> node option Lwt.t
  val steps: t -> node -> step list Lwt.t

  val iter_contents: t -> node -> (step -> contents -> unit) -> unit Lwt.t
  val iter_succ: t -> node -> (step -> node -> unit) -> unit Lwt.t

  val mem_contents: t -> node -> path -> bool Lwt.t
  val read_contents: t -> node -> path -> contents option Lwt.t
  val read_contents_exn: t -> node -> path -> contents Lwt.t
  val add_contents: t -> node -> path -> contents -> node Lwt.t
  val remove_contents: t -> node -> path -> node Lwt.t

  val mem_node: t -> node -> path -> bool Lwt.t
  val read_node: t -> node -> path -> node option Lwt.t
  val read_node_exn: t -> node -> path -> node Lwt.t
  val add_node: t -> node -> path -> node -> node Lwt.t
  val remove_node: t -> node -> path -> node Lwt.t

  val closure: t -> min:node list -> max:node list -> node list Lwt.t
  module Store: Ir_contents.STORE
    with type t = t
     and type key = node
     and type Path.t = path
     and type Path.step = step
end

module Graph (C: Ir_contents.STORE)
    (S: STORE with type Val.contents = C.key and module Path = C.Path) =
struct

  module Path = S.Path
  module Step = S.Path.Step
  type step = Step.t
  type contents = C.key
  type node = S.key
  type path = Path.t

  module Store = struct

    type t = C.t * S.t

    let create config =
      C.create config >>= fun c ->
      S.create config >>= fun s ->
      Lwt.return (c, s)

    type key = S.key
    type value = S.value
    let mem (_, t) = S.mem t
    let read (_, t) = S.read t
    let read_exn (_, t) = S.read_exn t
    let add (_, t) = S.add t

    module XContents = Tc.List(Tc.Pair(Step)(C.Key))
    module XParents = Tc.List(Tc.Pair(Step)(S.Key))
    module XP = Tc.Pair(XContents)(XParents)

    let all_contents t =
      let r = ref [] in
      S.Val.iter_contents t (fun s c -> r := (s, c) :: !r);
      List.rev !r

    let all_succ t =
      let r = ref [] in
      S.Val.iter_succ t (fun s c -> r := (s, c) :: !r);
      List.rev !r

    let merge_xcontents path c =
      Ir_merge.alist (module Step) (module C.Key) (fun k ->
          C.merge (Path.rcons path k) c
        )

    let merge_xparents path merge_key =
      Ir_merge.alist (module Step) (module S.Key) (fun k ->
          merge_key (Path.rcons path k)
        )

    let merge_value path (c, _) merge_key =
      Log.debug "merge_value %a" force (show (module Path) path);
      let explode t = all_contents t, all_succ t in
      let implode (contents, succ) =
        let xs = List.map (fun (s, c) -> s, `Contents c) contents in
        let ys = List.map (fun (s, n) -> s, `Node n) succ in
        S.Val.create (xs @ ys)
      in
      let merge =
        Ir_merge.pair (module XContents) (module XParents)
          (merge_xcontents path c) (merge_xparents path merge_key)
      in
      Ir_merge.biject (module S.Val) merge explode implode

    let merge path t ~old x y =
      let rec merge_key path =
        let merge = merge_value path t merge_key in
        let read = function
          | None   -> Lwt.return S.Val.empty
          | Some k -> read_exn t k
        in
        let add v =
          if S.Val.is_empty v then Lwt.return_none
          else add t v >>= fun k -> Lwt.return (Some k)
        in
        Ir_merge.biject' (module Tc.Option(S.Key)) merge read add
      in
      merge_key path ~old x y

    let iter (_, t) fn = S.iter t fn

    module Key = S.Key
    module Val = struct
      include S.Val
      let merge _path ~old:_ _ _ = conflict "Node.Val"
      module Path = Path
    end
    module Path = Path
  end

  type t = Store.t

  let empty (_, t) = S.add t S.Val.empty

  module StepSet = Ir_misc.Set(Step)
  module StepMap = Ir_misc.Map(Step)

  let iter_contents t n fn =
    Log.debug "iter_contents";
    Store.read t n >>= function
    | None   -> Lwt.return_unit
    | Some n -> Lwt.return (S.Val.iter_contents n fn)

  let iter_succ t n fn =
    Log.debug "iter_succ";
    Store.read t n >>= function
    | None   -> Lwt.return_unit
    | Some n -> Lwt.return (S.Val.iter_succ n fn)

  let steps t n =
    Log.debug "steps";
    Store.read t n >>= function
    | None   -> Lwt.return_nil
    | Some n ->
      let steps = ref StepSet.empty in
      S.Val.iter_contents n (fun l _ -> steps := StepSet.add l !steps);
      S.Val.iter_succ n (fun l _ -> steps := StepSet.add l !steps);
      Lwt.return (StepSet.to_list !steps)

  module Graph = Ir_graph.Make(C.Key)(S.Key)(Ir_hum.Unit)(Ir_hum.Unit)

  let edges t =
    let edges = ref [] in
    S.Val.iter_contents t (fun _ k -> edges := `Contents k :: !edges);
    S.Val.iter_succ t (fun _ k -> edges := `Node k :: !edges);
    !edges

  let closure t ~min ~max =
    Log.debug "closure min=%a max=%a"
      force (shows (module S.Key) min)
      force (shows (module S.Key) max);
    let pred = function
      | `Node k -> Store.read_exn t k >>= fun node -> Lwt.return (edges node)
      | _       -> Lwt.return_nil
    in
    let min = List.map (fun x -> `Node x) min in
    let max = List.map (fun x -> `Node x) max in
    Graph.closure ~pred ~min ~max () >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Node x -> Some x | _ -> None)
        (Graph.vertex g)
    in
    Lwt.return keys

  let create t xs =
    Store.add t (S.Val.create xs)

  let contents t node step =
    Log.debug "contents %a" force (show (module S.Key) node);
    Store.read t node >>= function
    | None   -> Lwt.return_none
    | Some n -> Lwt.return (S.Val.contents n step)

  let succ t node step =
    Log.debug "succ %a %a"
      force (show (module S.Key) node)
      force (show (module Step) step);
    Store.read t node >>= function
    | None   -> Lwt.return_none
    | Some n -> Lwt.return (S.Val.succ n step)

  let err_not_found n k =
    Ir_misc.invalid_arg "Irmin.Node.%s: %s not found" n (Path.to_hum k)

  let read_node_exn t node path =
    Log.debug "read_node_exn %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let rec aux node path =
      match Path.decons path with
      | None         -> Lwt.return node
      | Some (h, tl) ->
        succ t node h >>= function
        | None      -> Lwt.fail Not_found
        | Some node -> aux node tl
    in
    aux node path

  let read_node t node path =
    Lwt.catch
      (fun () ->
         read_node_exn t node path >>= fun node ->
         Lwt.return (Some node))
      (function Not_found -> Lwt.return_none | e -> Lwt.fail e)

  let read_node_exn t node path =
    Lwt.catch
      (fun () -> read_node_exn t node path)
      (function Not_found -> err_not_found "read_node" path | e -> Lwt.fail e)

  let err_empty_path () = invalid_arg "Irmin.node: empty path"

  let read_contents_exn t node path0 =
    Log.debug "read_contents_exn %a" force (show (module S.Path) path0);
    match Path.rdecons path0 with
    | None -> err_not_found "read_contents" path0
    | Some (path, file) ->
      read_node_exn t node path >>= fun node ->
      contents t node file >>= function
      | None   -> err_not_found "read_contents" path0
      | Some c -> Lwt.return c

  let read_contents t node path =
    Log.debug "read_contents %a" force (show (module S.Path) path);
    match Path.rdecons path with
    | None -> Lwt.return_none
    | Some (path, file) ->
      read_node t node path >>= function
      | None      -> Lwt.return_none
      | Some node ->
        contents t node file >>= function
        | None   -> Lwt.return_none
        | Some c -> Lwt.return (Some c)

  let mem_node t node path =
    Log.debug "mem_node %a" force (show (module S.Path) path);
    read_node t node path >>= function
    | None   -> Lwt.return_false
    | Some _ -> Lwt.return_true

  let mem_contents t node path =
    Log.debug "mem_contents %a" force (show (module S.Path) path);
    match Path.rdecons path with
    | None -> Lwt.return_false
    | Some (path, file) ->
      read_node t node path >>= function
      | None   -> Lwt.return_false
      | Some n ->
        contents t n file >>= function
        | None   -> Lwt.return_false
        | Some _ -> Lwt.return_true

  let map_one t node f label =
    Log.debug "map_one %a" force (show (module Step) label);
    let old_key = S.Val.succ node label in
    begin match old_key with
      | None   -> Lwt.return S.Val.empty
      | Some k -> Store.read_exn t k
    end >>= fun old_node ->
    f old_node >>= fun new_node ->
    if S.Val.equal old_node new_node then
      Lwt.return node
    else (
      if S.Val.is_empty new_node then (
        let node = S.Val.with_succ node label None in
        if S.Val.is_empty node then Lwt.return S.Val.empty
        else Lwt.return node
      ) else
        Store.add t new_node >>= fun k ->
        let node = S.Val.with_succ node label (Some k) in
        Lwt.return node
    )

  let map t node path f =
    Log.debug "map %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let rec aux node path =
      match Path.decons path with
      | None         -> Lwt.return (f node)
      | Some (h, tl) -> map_one t node (fun node -> aux node tl) h
    in
    begin Store.read t node >>= function
      | None   -> Lwt.return S.Val.empty
      | Some n -> Lwt.return n
    end >>= fun node ->
    aux node path >>=
    Store.add t

  let update_node t node path n =
    Log.debug "update_node %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    match Path.rdecons path with
    | Some (path, file) ->
      map t node path (fun node -> S.Val.with_succ node file n)
    | None ->
      match n with
      | None   -> empty t
      | Some n -> Lwt.return n

  let add_node t node path n = update_node t node path (Some n)
  let remove_node t node path =
    if Path.is_empty path then empty t
    else update_node t node path None

  let rdecons_exn path =
    match Path.rdecons path with
    | Some (l,t) -> l, t
    | None       -> err_empty_path ()

  let update_contents t node path c =
    Log.debug "update_contents %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let path, file = rdecons_exn path in
    map t node path (fun node -> S.Val.with_contents node file c)

  let add_contents t node path c = update_contents t node path (Some c)
  let remove_contents t node path = update_contents t node path None

end
