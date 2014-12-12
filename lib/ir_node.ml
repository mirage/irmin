(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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
open Ir_misc.OP
open Ir_merge.OP

module Log = Log.Make(struct let section = "NODE" end)

module type S = sig
  include Tc.S0
  type contents
  type node
  type step

  val create: contents:(step * contents) list -> succ:(step * node) list -> t

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

  type t = {
    contents: contents StepMap.t;
    succ    : node StepMap.t;
  }

  let create ~contents ~succ =
    let contents = StepMap.of_alist contents in
    let succ = StepMap.of_alist succ in
    { contents; succ }

  let iter_contents t f = StepMap.iter f t.contents
  let iter_succ t f = StepMap.iter f t.succ

  let contents t l =
    try Some (StepMap.find l t.contents)
    with Not_found -> None

  let succ t l =
    try Some (StepMap.find l t.succ)
    with Not_found -> None

  let to_sexp t =
    let open Sexplib.Type in
    List [
      List [ Atom "contents"; StepMap.to_sexp K_c.to_sexp t.contents ];
      List [ Atom "succ"    ; StepMap.to_sexp K_n.to_sexp t.succ ];
    ]

  let to_json t =
    `O [
      ("contents", StepMap.to_json K_c.to_json t.contents);
      ("succ"    , StepMap.to_json K_n.to_json t.succ);
    ]

  let of_json j =
    let contents =
      try Ezjsonm.find j ["contents"] |> StepMap.of_json K_c.of_json
      with Not_found -> StepMap.empty
    in
    let succ =
      try Ezjsonm.find j ["succ"] |> StepMap.of_json K_n.of_json
      with Not_found -> StepMap.empty
    in
    { contents; succ }

  module Contents = Tc.App1(StepMap)(K_c)
  module Parents = Tc.App1(StepMap)(K_n)
  module P = Tc.Pair(Contents)(Parents)

  let explode t = (t.contents, t.succ)
  let implode (contents, succ) = { contents; succ }
  let write t = P.write (explode t)
  let size_of t = P.size_of (explode t)
  let read b = implode (P.read b)
  let hash t = P.hash (explode t)
  let compare x y = P.compare (explode x) (explode y)
  let equal x y = P.equal (explode x) (explode y)

  let empty =
    { contents = StepMap.empty;
      succ = StepMap.empty }

  let is_empty e =
    StepMap.is_empty e.contents && StepMap.is_empty e.succ

  let with_contents t step contents =
    let contents = match contents with
      | None   -> StepMap.remove step t.contents
      | Some c -> StepMap.add step c t.contents
    in
    { t with contents }

  let with_succ t step succ =
    let succ = match succ with
      | None   -> StepMap.remove step t.succ
      | Some s -> StepMap.add step s t.succ
    in
    { t with succ }

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

  val empty: t -> node Lwt.t
  val node: t ->
    contents:(step * contents) list -> succ:(step * node) list -> node Lwt.t

  val contents: t -> node -> step -> contents option Lwt.t
  val succ: t -> node -> step -> node option Lwt.t
  val steps: t -> node -> step list Lwt.t

  val iter_contents: t -> node -> (step -> contents -> unit) -> unit Lwt.t
  val iter_succ: t -> node -> (step -> node -> unit) -> unit Lwt.t

  val mem_contents: t -> node -> step list -> bool Lwt.t
  val read_contents: t -> node -> step list -> contents option Lwt.t
  val read_contents_exn: t -> node -> step list -> contents Lwt.t
  val add_contents: t -> node -> step list -> contents -> node Lwt.t
  val remove_contents: t -> node -> step list -> node Lwt.t

  val mem_node: t -> node -> step list -> bool Lwt.t
  val read_node: t -> node -> step list -> node option Lwt.t
  val read_node_exn: t -> node -> step list -> node Lwt.t
  val add_node: t -> node -> step list -> node -> node Lwt.t
  val remove_node: t -> node -> step list -> node Lwt.t

  val merge: t -> node Ir_merge.t
  val closure: t -> min:node list -> max:node list -> node list Lwt.t
  module Store: Ir_contents.STORE with type t = t and type key = node
end

module Graph (C: Ir_contents.STORE) (S: STORE with type Val.contents = C.key) =
struct

  module Step = S.Path.Step
  type step = Step.t
  type contents = C.key
  type node = S.key

  module Store = struct

    type t = C.t * S.t

    let create config task =
      C.create config task >>= fun c ->
      S.create config task >>= fun s ->
      return (fun a -> c a, s a)

    type key = S.key
    type value = S.value
    let task (_, t) = S.task t
    let config (_, t) = S.config t
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

    let merge_value (c, _) merge_key =
      let explode t = all_contents t, all_succ t in
      let implode (contents, succ) = S.Val.create ~contents ~succ in
      let merge_pair =
        Ir_merge.pair (module XContents) (module XParents)
        (Ir_merge.alist (module Step) (module C.Key) (C.merge c))
        (Ir_merge.alist (module Step) (module S.Key) merge_key)
      in
      Ir_merge.biject (module XP) (module S.Val) merge_pair implode explode

    let merge t ~old x y =
      let rec merge_key () =
        Log.debugf "merge";
        let merge = merge_value t (Ir_merge.apply merge_key ()) in
        Ir_merge.biject'
        (module S.Val) (module S.Key) merge (add t) (read_exn t)
      in
      merge_key () ~old x y

    module Key = S.Key
    module Val = struct
      include S.Val
      let merge ~old:_ _ _ = conflict "Node.Val"
    end
  end

  type t = Store.t
  let merge = Store.merge

  let empty (_, t) = S.add t S.Val.empty

  module StepSet = Ir_misc.Set(Step)
  module StepMap = Ir_misc.Map(Step)

  let iter_contents t n fn =
    Log.debugf "iter_contents";
    Store.read t n >>= function
    | None   -> return_unit
    | Some n -> return (S.Val.iter_contents n fn)

  let iter_succ t n fn =
    Log.debugf "iter_succ";
    Store.read t n >>= function
    | None   -> return_unit
    | Some n -> return (S.Val.iter_succ n fn)

  let steps t n =
    Log.debugf "steps";
    Store.read t n >>= function
    | None   -> return_nil
    | Some n ->
      let steps = ref StepSet.empty in
      S.Val.iter_contents n (fun l _ -> steps := StepSet.add l !steps);
      S.Val.iter_succ n (fun l _ -> steps := StepSet.add l !steps);
      return (StepSet.to_list !steps)

  module Graph = Ir_graph.Make(C.Key)(S.Key)(Tc.Unit)(Tc.Unit)

  let edges t =
    let edges = ref [] in
    S.Val.iter_contents t (fun _ k -> edges := `Contents k :: !edges);
    S.Val.iter_succ t (fun _ k -> edges := `Node k :: !edges);
    !edges

  let closure t ~min ~max =
    Log.debugf "closure min=%a max=%a"
      force (shows (module S.Key) min)
      force (shows (module S.Key) max);
    let pred = function
      | `Node k -> Store.read_exn t k >>= fun node -> return (edges node)
      | _       -> return_nil
    in
    let min = List.map (fun x -> `Node x) min in
    let max = List.map (fun x -> `Node x) max in
    Graph.closure ~pred ~min ~max () >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Node x -> Some x | _ -> None)
        (Graph.vertex g)
    in
    return keys

  let node t ~contents ~succ =
    Store.add t (S.Val.create ~contents ~succ)

  let contents t node step =
    Log.debugf "contents %a" force (show (module S.Key) node);
    Store.read t node >>= function
    | None   -> return_none
    | Some n -> return (S.Val.contents n step)

  let succ t node step =
    Log.debugf "succ %a %a"
      force (show (module S.Key) node)
      force (show (module Step) step);
    Store.read t node >>= function
    | None   -> return_none
    | Some n -> return (S.Val.succ n step)

  let read_node_exn t node path =
    Log.debugf "read_node_exn %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let rec aux node = function
      | []    -> return node
      | h::tl ->
        succ t node h >>= function
        | None      -> fail Not_found
        | Some node -> aux node tl
    in
    aux node path

  let read_node t node path =
    catch
      (fun () ->
         read_node_exn t node path >>= fun node ->
         return (Some node))
      (function Not_found -> return_none | e -> fail e)

  let mk_path path =
    try Ir_misc.list_end path
    with Not_found -> [], Step.of_hum ""

  let read_contents_exn t node path =
   Log.debugf "read_contents_exn %a %a"
     force (show (module S.Key) node)
     force (show (module S.Path) path);
     let path, file = mk_path path in
     read_node t node path >>= function
     | None      ->
       Log.debugf "subpath not found";
       fail Not_found
     | Some node ->
       contents t node file >>= function
       | None   -> fail Not_found
       | Some c -> return c

  let read_contents t node path =
    Log.debugf "read_contents %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let path, file = mk_path path in
    read_node t node path >>= function
    | None      -> return_none
    | Some node ->
      contents t node file >>= function
      | None   -> return_none
      | Some c -> return (Some c)

  let mem_node t node path =
    Log.debugf "mem_node %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    read_node t node path >>= function
    | None   -> return false
    | Some _ -> return true

  let mem_contents t node path =
    Log.debugf "mem_contents %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let path, file = mk_path path in
    read_node t node path >>= function
    | None   -> return false
    | Some n ->
      contents t n file >>= function
      | None   -> return false
      | Some _ -> return true

  let map_one t node f label =
    Log.debugf "map_one %a %a"
      force (show (module S.Val) node)
      force (show (module Step) label);
    let old_key = S.Val.succ node label in
    begin match old_key with
      | None   -> return S.Val.empty
      | Some k -> Store.read_exn t k
    end >>= fun old_node ->
    f old_node >>= fun new_node ->
    if S.Val.equal old_node new_node then
      return node
    else (
      if S.Val.is_empty new_node then (
        let node = S.Val.with_succ node label None in
        if S.Val.is_empty node then return S.Val.empty
        else return node
      ) else
        Store.add t new_node >>= fun k ->
        let node = S.Val.with_succ node label (Some k) in
        return node
    )

  let map t node path f =
    Log.debugf "map %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let rec aux node = function
      | []      -> return (f node)
      | h :: tl -> map_one t node (fun node -> aux node tl) h
    in
    begin Store.read t node >>= function
      | None   -> return S.Val.empty
      | Some n -> return n
    end >>= fun node ->
    aux node path >>=
    Store.add t

  let update_node t node path n =
    Log.debugf "update_node %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let path, file = mk_path path in
    map t node path (fun node -> S.Val.with_succ node file n)

  let add_node t node path n = update_node t node path (Some n)
  let remove_node t node path =
    match path with
    | [] -> empty t
    | _  -> update_node t node path None

  let update_contents t node path c =
    Log.debugf "update_contents %a %a"
      force (show (module S.Key) node)
      force (show (module S.Path) path);
    let path, file = mk_path path in
    map t node path (fun node -> S.Val.with_contents node file c)

  let add_contents t node path c = update_contents t node path (Some c)
  let remove_contents t node path = update_contents t node path None

end
