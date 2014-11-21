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
open Sexplib.Std
open Ir_misc.OP

module Log = Log.Make(struct let section = "NODE" end)

module type S = sig
  include Ir_contents.S
  type contents
  type node
  type 'a step_map
  val contents: t -> contents option
  val contents_exn: t -> contents
  val succ: t -> node step_map
  val edges: t -> [`Contents of contents | `Node of node] list
  val empty: t
  val leaf: contents -> t
  val create: ?contents:contents -> node step_map -> t
  val is_empty: t -> bool
  val is_leaf: t -> bool
end

module type STORE = sig
  include Ir_ao.STORE
  type contents
  type step
  module Step: Tc.I0 with type t = step
  val empty: value
  val node: t -> origin -> ?contents:contents -> ?succ:(step * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> origin -> value -> contents Lwt.t option
  val succ: t -> origin -> value -> value Lwt.t Map.Make(Step).t
  val sub: t -> origin -> value -> step list -> value option Lwt.t
  val sub_exn: t -> origin -> value -> step list -> value Lwt.t
  val map: t -> origin -> value -> step list -> (value -> value) -> value Lwt.t
  val update: t -> origin -> value -> step list -> contents -> value Lwt.t
  val find: t -> origin -> value -> step list -> contents option Lwt.t
  val find_exn: t -> origin -> value -> step list -> contents Lwt.t
  val remove: t -> origin -> value -> step list -> value Lwt.t
  val valid: t -> origin -> value -> step list -> bool Lwt.t
  val merge: t -> (key, origin) Ir_merge.t
  module Contents: Ir_contents.STORE
    with type value = contents
     and type origin = origin
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type node = key
     and type contents = Contents.key
     and type 'a step_map = 'a Map.Make(Step).t
end

module type MAKER =
  functor (K: Ir_hash.S) ->
  functor (S: Ir_step.S) ->
  functor (C: Ir_contents.STORE) ->
    STORE with type key = K.t
           and type step = S.t
           and type contents = C.value
           and type origin = C.origin
           and module Contents = C

module Make (Node: Ir_ao.MAKER)
    (K: Ir_hash.S) (S: Ir_step.S)  (C: Ir_contents.STORE)
= struct

  module Contents = C
  module Step = S
  module StepMap = Ir_misc.Map(Step)

  module Val = struct

    module Origin = C.Val.Origin
    type origin = Origin.t
    type contents = C.key
    type node = K.t
    type 'a step_map = 'a StepMap.t

    type t = {
      contents: contents option;
      succ    : node StepMap.t;
    }

    let create ?contents succ = { contents; succ = succ }
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let contents t = t.contents
    let succ t = t.succ

    let equal t1 t2 =
      begin match t1.contents, t2.contents with
        | Some _ , None
        | None   , Some _  -> false
        | Some k1, Some k2 -> C.Key.equal k1 k2
        | None   , None    -> true
      end
      && StepMap.equal K.equal t1.succ t2.succ

    let to_sexp t =
      let open Sexplib.Type in
      let open Sexplib.Conv in
      List [
        List [ Atom "contents"; sexp_of_option C.Key.to_sexp t.contents ];
        List [ Atom "succ"    ; StepMap.to_sexp K.to_sexp t.succ ];
      ]

    let to_json t =
      `O [
        ("contents", Ezjsonm.option C.Key.to_json t.contents);
        ("succ"    , StepMap.to_json K.to_json t.succ);
      ]

    let of_json j =
      let contents =
        try Ezjsonm.find j ["contents"] |> Ezjsonm.get_option C.Key.of_json
        with Not_found -> None
      in
      let succ =
        try Ezjsonm.find j ["succ"] |> StepMap.of_json K.of_json
        with Not_found -> StepMap.empty
      in
      { contents; succ }

    module Contents = Tc.App1(Tc.O)(C.Key)
    module Parents = Tc.App1(StepMap)(K)
    module P = Tc.App2(Tc.P)(Contents)(Parents)

    let write t = P.write (t.contents, t.succ)
    let size_of t = P.size_of (t.contents, t.succ)
    let read b = let contents, succ = P.read b in { contents; succ }

    let edges t =
      begin match t.contents with
        | None   -> []
        | Some k -> [`Contents k]
      end
      @ StepMap.fold
        (fun _ k acc -> `Node k :: acc)
        t.succ []

    let empty =
      { contents = None;
        succ = StepMap.empty }

    let is_empty e =
      e.contents = None && StepMap.is_empty e.succ

    let leaf e =
      { contents = Some e;
        succ = StepMap.empty }

    let is_leaf e =
      e.contents <> None && StepMap.is_empty e.succ

    let contents_exn e =
      match e.contents with
      | None   -> raise Not_found
      | Some c -> c

    let merge _ ~old:_ _ _ = Ir_merge.OP.conflict "node"

  end

  module N = Node(K)(Val)(Val.Origin)

  module Key = K

  type origin = N.origin
  type step = S.t

  type key = K.t
  type contents = C.value
  type value = Val.t
  type t = C.t * N.t

  let empty = Val.empty

  let create () =
    C.create (), N.create ()

  let add (_, t) origin n =
    N.add t origin n

 let read (_, t) origin key =
    N.read t origin key

  let read_exn t origin key =
    read t origin key >>= function
    | None   ->
      Log.debugf "Not_found: %a" force (show (module K) key);
      fail Not_found
    | Some v -> return v

  let mem (_, t) origin key =
    N.mem t origin key

  module Graph = Ir_graph.Make(C.Key)(K)(Tc.U)(Tc.U)

  let list t origin keys =
    Log.debugf "list %a" force (shows (module K) keys);
    let pred = function
      | `Node k -> read_exn t origin k >>= fun node -> return (Val.edges node)
      | _       -> return_nil in
    let max = List.map (fun x -> `Node x) keys in
    Graph.closure ~pred max >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Node x -> Some x | _ -> None)
        (Graph.vertex g)
    in
    return keys

  let dump (_, t) origin =
    N.dump t origin

  let node (c, _ as t) origin ?contents ?(succ=[]) () =
    begin match contents with
      | None          -> return_none
      | Some contents ->
        C.add c origin contents >>= fun k ->
        return (Some k)
    end >>= fun contents ->
    begin
      Lwt_list.map_p (fun (l, node) ->
          add t origin node >>= fun k ->
          return (l, k)
        ) succ
    end >>= fun succ ->
    let succ = StepMap.of_alist succ in
    let node = { Val.contents; succ } in
    add t origin node >>= fun key ->
    return (key, node)

  (* Ir_merge the contents values together. *)
  let merge_contents c =
    Ir_merge.some (module C.Key) (C.merge c)

  module MMap = Ir_merge.Map(Step)

  let merge_value (c, _) merge_key =
    let explode { Val.contents; succ } = (contents, succ) in
    let implode (contents, succ) = { Val.contents; succ } in
    let merge_pair =
      Ir_merge.pair (module Val.Contents) (module Val.Parents)
        (merge_contents c)
        (MMap.merge (module K) merge_key)
    in
    Ir_merge.biject (module Val.P) (module Val) merge_pair implode explode

  let merge t origin ~old x y =
    let rec merge_key () =
      Log.debugf "merge";
      let merge = merge_value t (Ir_merge.apply merge_key ()) in
      Ir_merge.biject'
        (module Val) (module K) merge (add t origin) (read_exn t origin)
    in
    merge_key () origin ~old x y

  let contents (c, _) origin n =
    match n.Val.contents with
    | None   -> None
    | Some k -> Some (C.read_exn c origin k)

  let succ t origin node =
    StepMap.map (read_exn t origin) node.Val.succ

  let next t origin node label =
    try read t origin (StepMap.find label node.Val.succ)
    with Not_found -> return_none

  let sub_exn t origin node path =
    let rec aux node path =
      match path with
      | []    -> return node
      | h::tl ->
        next t origin node h >>= function
        | None      -> fail Not_found
        | Some node -> aux node tl in
    aux node path

  let sub t origin node path =
    catch
      (fun () ->
         sub_exn t origin node path >>= fun node ->
         return (Some node))
      (function Not_found -> return_none | e -> fail e)

  let find_exn t origin node path =
    Log.debugf "find_exn %a" force (shows (module S) path);
    sub t origin node path >>= function
    | None      ->
      Log.debugf "subpath not found";
      fail Not_found
    | Some node ->
      match contents t origin node with
      | None   ->
        Log.debugf "contents not found";
        fail Not_found
      | Some b -> b

  let find t origin node path =
    Log.debugf "find %a" force (shows (module S) path);
    sub t origin node path >>= function
    | None      -> return_none
    | Some node ->
      match contents t origin node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t origin node path =
    Log.debugf "valid %a" force (shows (module S) path);
    sub t origin node path >>= function
    | None      -> return false
    | Some node ->
      match contents t origin node with
      | None   -> return false
      | Some _ -> return true

  let map_children t origin children f label =
    Log.debugf "map_children %a" force (show (module S) label);
    let old_key =
      try Some (StepMap.find label children)
      with Not_found -> None in
    begin match old_key with
      | None   -> return Val.empty
      | Some k -> read_exn t origin k
    end >>= fun old_node ->
    f old_node >>= fun node ->
    if Val.equal old_node node then
      return children
    else (
      begin
        if Val.is_empty node then return_none
        else
          add t origin node >>= fun k ->
          return (Some k)
      end >>= fun key ->
      let children = match old_key, key with
        | None  , None     -> children
        | Some _, None     -> StepMap.remove label children
        | None  , Some k   -> StepMap.add label k children
        | Some k1, Some k2 ->
          if K.equal k1 k2 then children
          else StepMap.add label k2 children in
      return children
    )

  let map t origin node path f =
    let rec aux node = function
      | []      -> return (f node)
      | h :: tl ->
        map_children t origin node.Val.succ (fun node -> aux node tl) h
        >>= fun succ ->
        return { node with Val.succ } in
    aux node path

  let remove t origin node path =
    Log.debugf "remove %a" force (shows (module S) path);
    map t origin node path (fun _ -> Val.empty)

  let update (c, _ as t) origin node path value =
    Log.debugf "update %a" force (shows (module S) path);
    C.add c origin value >>= fun k  ->
    map t origin node path (fun node -> { node with Val.contents = Some k })

end

module Rec (S: STORE) = struct
  include S.Key
  module Origin = S.Contents.Val.Origin
  type origin = Origin.t
  let merge = S.merge (S.create ())
end
