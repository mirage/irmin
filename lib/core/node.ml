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
open Bin_prot.Std
open Misc.OP

module Log = Log.Make(struct let section = "NODE" end)

module StringMap = struct
  include Misc.StringMap
  let t_of_sexp fn s =
    let fn' = Sexplib.Conv.(pair_of_sexp string_of_sexp fn) in
    of_alist (Sexplib.Conv.list_of_sexp fn' s)
  let sexp_of_t = to_sexp
  let bin_size_t = size_of
  let bin_write_t fn = Tc.Writer.(to_bin_prot (write (of_bin_prot fn)))
  let bin_read_t fn = Tc.Reader.(to_bin_prot (read (of_bin_prot fn)))
end

module T_ = struct
  type 'key t = {
    contents: 'key option;
    succ    : 'key StringMap.t;
  } with bin_io, compare, sexp
end
include T_
module T = Tc.I1(T_)

let equal key_equal t1 t2 =
  begin match t1.contents, t2.contents with
    | Some _ , None
    | None   , Some _  -> false
    | Some k1, Some k2 -> key_equal k1 k2
    | None   , None    -> true
  end
  && StringMap.equal key_equal t1.succ t2.succ

let edges t =
  begin match t.contents with
    | None   -> []
    | Some k -> [`Contents k]
  end
  @ StringMap.fold
    (fun _ k acc -> `Node k :: acc)
    t.succ []

let empty =
  { contents = None;
    succ = StringMap.empty }

let is_empty e =
  e.contents = None && StringMap.is_empty e.succ

let leaf e =
  { contents = Some e;
    succ = StringMap.empty }

let is_leaf e =
  e.contents <> None && StringMap.is_empty e.succ

let contents_exn e =
  match e.contents with
  | None   -> raise Not_found
  | Some c -> c

module type S = sig
  type key
  type nonrec t = key t
  include Sig.Contents with type t := t
end

module type STORE = sig
  type key
  type contents
  type path = Path.t
  type value = key t
  include Sig.AO with type key := key and type value := value
  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> value -> contents Lwt.t option
  val succ: t -> value -> value Lwt.t StringMap.t
  val sub: t -> value -> path -> value option Lwt.t
  val sub_exn: t -> value -> path -> value Lwt.t
  val map: t -> value -> path -> (value -> value) -> value Lwt.t
  val update: t -> value -> path -> contents -> value Lwt.t
  val find: t -> value -> path -> contents option Lwt.t
  val find_exn: t -> value -> path -> contents Lwt.t
  val remove: t -> value -> path -> value Lwt.t
  val valid: t -> value -> path -> bool Lwt.t
  val merge: t -> key Merge.t
  module Key: Sig.Uid with type t = key
  module Value: S with type key = key
end

module S (K: Sig.Uid) = struct
  type key = K.t
  module S = Tc.App1(T)(K)
  include S
  let merge = Merge.default (module S)
end

module SHA1 = S(Uid.SHA1)

module Make
    (K: Sig.Uid)
    (C: Sig.Contents)
    (Contents: Contents.STORE with type key = K.t and type value = C.t)
    (Node: Sig.AO with type key = K.t and type value = K.t t) =
 struct

  module Key = K

  module Value = S(K)

  type key = K.t

  type contents = C.t

  type path = Path.t

  type value = K.t t

  type t = Contents.t * Node.t

  let create () =
    Contents.create () >>= fun c ->
    Node.create ()     >>= fun t ->
    return (c, t)

  let add (_, t) n = match n with
    | { contents = Some k } ->
      if StringMap.is_empty n.succ then return k else Node.add t n
    | _                     -> Node.add t n

  (* "leaf" nodes (ie. with no succ and some contents) are not
     duplicated: they are isomorphic to the contents itself and so:
     they live in the contents store and have the same key than their
     contents. *)
 let read (c, t) key =
    Node.read t key >>= function
    | Some _ as x -> return x
    | None        ->
      Contents.mem c key >>= function
      | true  -> return (Some (leaf key))
      | false -> return_none

  let read_exn t key =
    read t key >>= function
    | None   ->
      Log.debugf "Not_found: %a" force (show (module K) key);
      fail Not_found
    | Some v -> return v

  let mem (c, t) key =
    Node.mem t key >>= function
    | false -> Contents.mem c key
    | true  -> return true

  module Graph = Digraph.Make(K)(Tag.String)

  let list t keys =
    Log.debugf "list %a" force (shows (module K) keys);
    let pred = function
      | `Node k -> read_exn t k >>= fun node -> return (edges node)
      | _       -> return_nil in
    let max = Digraph.of_nodes keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = Digraph.to_nodes (Graph.vertex g) in
    return keys

  let dump (_, t) =
    Node.dump t

  let node (c, _ as t) ?contents ?(succ=[]) () =
    begin match contents with
      | None          -> return_none
      | Some contents ->
        Contents.add c contents >>= fun k ->
        return (Some k)
    end >>= fun contents ->
    begin
      Lwt_list.map_p (fun (l, node) ->
          add t node >>= fun k ->
          return (l, k)
        ) succ
    end >>= fun succ ->
    let succ = StringMap.of_alist succ in
    let node = { contents; succ } in
    add t node >>= fun key ->
    return (key, node)

  (* Merge the contents values together. *)
  let merge_contents c =
    Merge.some (Contents.merge c)

  let merge_value (c, _) merge_key =
    let explode n = (n.contents, n.succ) in
    let implode (contents, succ) = { contents; succ } in
    let merge_pair = Merge.pair
        (merge_contents c)
        (Merge.string_map merge_key)
    in
    Merge.biject (module Value) merge_pair implode explode

  let merge (c, _ as t) =
    let rec merge_key () =
      Log.debugf "merge";
      let merge = merge_value t (Merge.apply (module K) merge_key ()) in
      Merge.biject' (module K) merge (add t) (read_exn t) in
    merge_key ()

  let contents (c, _) n =
    match n.contents with
    | None   -> None
    | Some k -> Some (Contents.read_exn c k)

  let succ t node =
    StringMap.map (fun k -> read_exn t k) node.succ

  let next t node label =
    try read t (StringMap.find label node.succ)
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
    Log.debugf "find_exn %a" force (show (module Path) path);
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
    Log.debugf "find %a" force (show (module Path) path);
    sub t node path >>= function
    | None      -> return_none
    | Some node ->
      match contents t node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t node path =
    Log.debugf "valid %a" force (show (module Path) path);
    sub t node path >>= function
    | None      -> return false
    | Some node ->
      match contents t node with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    Log.debugf "map_children %s" label;
    let old_key =
      try Some (StringMap.find label children)
      with Not_found -> None in
    begin match old_key with
      | None   -> return empty
      | Some k -> read_exn t k
    end >>= fun old_node ->
    f old_node >>= fun node ->
    if equal K.equal old_node node then
      return children
    else (
      begin
        if is_empty node then return_none
        else
          add t node >>= fun k ->
          return (Some k)
      end >>= fun key ->
      let children = match old_key, key with
        | None  , None     -> children
        | Some _, None     -> StringMap.remove label children
        | None  , Some k   -> StringMap.add label k children
        | Some k1, Some k2 ->
          if K.equal k1 k2 then children
          else StringMap.add label k2 children in
      return children
    )

  let map t node path f =
    let rec aux node = function
      | []      -> return (f node)
      | h :: tl ->
        map_children t node.succ (fun node -> aux node tl) h >>= fun succ ->
        return { node with succ } in
    aux node path

  let remove t node path =
    Log.debugf "remove %a" force (show (module Path) path);
    map t node path (fun node -> empty)

  let update (c, _ as t) node path value =
    Log.debugf "update %a" force (show (module Path) path);
    Contents.add c value >>= fun k  ->
    map t node path (fun node -> { node with contents = Some k }) >>= fun n ->
    return n

end

module Rec (S: STORE) = struct
  include S.Key
  let merge =
    let merge ~origin ~old k1 k2 =
      S.create ()  >>= fun t  ->
      Merge.merge (S.merge t) ~origin ~old k1 k2
    in
    Merge.create' (module S.Key) merge
end
