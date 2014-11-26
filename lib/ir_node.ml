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

module Log = Log.Make(struct let section = "NODE" end)

module type S = sig
  include Ir_contents.S
  type contents
  type node
  type 'a step_map
  val contents: t -> contents option
  val contents_exn: t -> contents
  val with_contents: t -> contents option -> t
  val succ: t -> node step_map
  val with_succ: t -> node step_map -> t
  val edges: t -> [> `Contents of contents | `Node of node] list
  val empty: t
  val leaf: contents -> t
  val create: ?contents:contents -> node step_map -> t
  val is_empty: t -> bool
  val is_leaf: t -> bool
end

module Node (K_c: Tc.S0) (K_n: Tc.S0) (StepMap: Ir_misc.MAP) = struct

  type contents = K_c.t
  type node = K_n.t

  type 'a step_map = 'a StepMap.t

  type t = {
    contents: contents option;
    succ    : node StepMap.t;
  }

  let create ?contents succ = { contents; succ = succ }
  let contents t = t.contents
  let succ t = t.succ

  let to_sexp t =
    let open Sexplib.Type in
    let open Sexplib.Conv in
    List [
      List [ Atom "contents"; sexp_of_option K_c.to_sexp t.contents ];
      List [ Atom "succ"    ; StepMap.to_sexp K_n.to_sexp t.succ ];
    ]

  let to_json t =
    `O [
      ("contents", Ezjsonm.option K_c.to_json t.contents);
      ("succ"    , StepMap.to_json K_n.to_json t.succ);
    ]

  let of_json j =
    let contents =
      try Ezjsonm.find j ["contents"] |> Ezjsonm.get_option K_c.of_json
      with Not_found -> None
    in
    let succ =
      try Ezjsonm.find j ["succ"] |> StepMap.of_json K_n.of_json
      with Not_found -> StepMap.empty
    in
    { contents; succ }

  module Contents = Tc.Option(K_c)
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

  let with_contents node contents = { node with contents }

  let with_succ node succ = { node with succ }

  let merge ~old:_ _ _ = Ir_merge.OP.conflict "node"

end

module type RAW_STORE = sig
  include Ir_ao.STORE
  module Step: Tc.S0
  module StepMap: Map.S with type key = Step.t
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type node = key
     and type 'a step_map = 'a StepMap.t
end

module type STORE = sig
  type step
  module Contents: Ir_contents.STORE
  include RAW_STORE
    with type Step.t = step
     and type Val.contents = Contents.key
  type contents = Contents.value
  val empty: value
  val node: t -> ?contents:contents -> ?succ:(step * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> value -> contents Lwt.t option
  val succ: t -> value -> value Lwt.t StepMap.t
  val sub: t -> value -> step list -> value option Lwt.t
  val sub_exn: t -> value -> step list -> value Lwt.t
  val map: t -> value -> step list -> (value -> value) -> value Lwt.t
  val update: t -> value -> step list -> contents -> value Lwt.t
  val find: t -> value -> step list -> contents option Lwt.t
  val find_exn: t -> value -> step list -> contents Lwt.t
  val remove: t -> value -> step list -> value Lwt.t
  val valid: t -> value -> step list -> bool Lwt.t
  val merge: t -> key Ir_merge.t
  val contents_t: t -> Contents.t
end

module Make
  (C: Ir_contents.RAW_STORE)
  (S: RAW_STORE with type Val.contents = C.key)
= struct

  module Contents = Ir_contents.Make(C)

  module Step = S.Step
  module StepMap = Ir_misc.Map(S.StepMap)(Step)

  module Val = S.Val
  module Key = S.Key

  type step = Step.t
  type key = Key.t
  type value = Val.t
  type contents = C.value
  type t = C.t * S.t

  let empty = Val.empty
  let contents_t = fst

  let create config task =
    let c = C.create config task in
    let s = S.create config task in
    c, s

  let task (_, t) =
    S.task t

  let config (_, t) =
    S.config t

  let add (_, t) n =
    S.add t n

 let read (_, t) key =
    S.read t key

  let read_exn t key =
    read t key >>= function
    | None   ->
      Log.debugf "Not_found: %a" force (show (module Key) key);
      fail Not_found
    | Some v -> return v

  let mem (_, t) key =
    S.mem t key

  module Graph = Ir_graph.Make(C.Key)(Key)(Tc.Unit)(Tc.Unit)

  let list t keys =
    Log.debugf "list %a" force (shows (module Key) keys);
    let pred = function
      | `Node k -> read_exn t k >>= fun node -> return (Val.edges node)
      | _       -> return_nil in
    let max = List.map (fun x -> `Node x) keys in
    Graph.closure ~pred max >>= fun g ->
    let keys =
      Ir_misc.list_filter_map
        (function `Node x -> Some x | _ -> None)
        (Graph.vertex g)
    in
    return keys

  let dump (_, t) =
    S.dump t

  let node (c, _ as t) ?contents ?(succ=[]) () =
    begin match contents with
      | None          -> return_none
      | Some contents ->
        C.add c contents >>= fun k ->
        return (Some k)
    end >>= fun contents ->
    begin
      Lwt_list.map_p (fun (l, node) ->
          add t node >>= fun k ->
          return (l, k)
        ) succ
    end >>= fun succ ->
    let succ = StepMap.of_alist succ in
    let node = Val.create ?contents succ in
    add t node >>= fun key ->
    return (key, node)

  (* Ir_merge the contents values together. *)
  let merge_contents (c, _) =
    Ir_merge.some (module C.Key) (Contents.merge c)

  module XMap = Ir_merge.Map(StepMap)(S.Step)
  module XContents = Tc.Option(C.Key)
  module XParents = Tc.App1(StepMap)(Key)
  module XP = Tc.Pair(XContents)(XParents)

  let merge_value t merge_key =
    let explode t = Val.contents t, Val.succ t in
    let implode (contents, succ) = Val.create ?contents succ in
    let merge_pair =
      Ir_merge.pair (module XContents) (module XParents)
        (merge_contents t)
        (XMap.merge (module Key) merge_key)
    in
    Ir_merge.biject (module XP) (module Val) merge_pair implode explode

  let merge t ~old x y =
    let rec merge_key () =
      Log.debugf "merge";
      let merge = merge_value t (Ir_merge.apply merge_key ()) in
      Ir_merge.biject'
        (module Val) (module Key) merge (add t) (read_exn t)
    in
    merge_key () ~old x y

  let contents (c, _) n =
    match Val.contents n with
    | None   -> None
    | Some k -> Some (C.read_exn c k)

  let succ t node =
    StepMap.map (read_exn t) (Val.succ node)

  let next t node label =
    try read t (StepMap.find label (Val.succ node))
    with Not_found -> return_none

  let sub_exn t node path =
    let rec aux node path =
      match path with
      | []    -> return node
      | h::tl ->
        next t node h >>= function
        | None      -> fail Not_found
        | Some node -> aux node tl in
    aux node path

  let sub t node path =
    catch
      (fun () ->
         sub_exn t node path >>= fun node ->
         return (Some node))
      (function Not_found -> return_none | e -> fail e)

  let find_exn t node path =
    Log.debugf "find_exn %a" force (shows (module Step) path);
    sub t node path >>= function
    | None      ->
      Log.debugf "subpath not found";
      fail Not_found
    | Some node ->
      match contents t node with
      | None   ->
        Log.debugf "contents not found";
        fail Not_found
      | Some b -> b

  let find t node path =
    Log.debugf "find %a" force (shows (module Step) path);
    sub t node path >>= function
    | None      -> return_none
    | Some node ->
      match contents t node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t node path =
    Log.debugf "valid %a" force (shows (module Step) path);
    sub t node path >>= function
    | None      -> return false
    | Some node ->
      match contents t node with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    Log.debugf "map_children %a" force (show (module Step) label);
    let old_key =
      try Some (StepMap.find label children)
      with Not_found -> None in
    begin match old_key with
      | None   -> return Val.empty
      | Some k -> read_exn t k
    end >>= fun old_node ->
    f old_node >>= fun node ->
    if Val.equal old_node node then
      return children
    else (
      begin
        if Val.is_empty node then return_none
        else
          add t node >>= fun k ->
          return (Some k)
      end >>= fun key ->
      let children = match old_key, key with
        | None  , None     -> children
        | Some _, None     -> StepMap.remove label children
        | None  , Some k   -> StepMap.add label k children
        | Some k1, Some k2 ->
          if Key.equal k1 k2 then children
          else StepMap.add label k2 children in
      return children
    )

  let map t node path f =
    let rec aux node = function
      | []      -> return (f node)
      | h :: tl ->
        map_children t (Val.succ node) (fun node -> aux node tl) h
        >>= fun succ ->
        return (Val.with_succ node succ) in
    aux node path

  let remove t node path =
    Log.debugf "remove %a" force (shows (module Step) path);
    map t node path (fun _ -> Val.empty)

  let update (c, _ as t) node path value =
    Log.debugf "update %a" force (shows (module Step) path);
    C.add c value >>= fun k  ->
    map t node path (fun node -> Val.with_contents node (Some k) )

end
