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
open IrminCore

module Log = Log.Make(struct let section = "NODE" end)

type 'key t = {
  contents: 'key option;
  succ    : 'key String.Map.t;
} with bin_io, compare, sexp

let equal key_equal t1 t2 =
  begin match t1.contents, t2.contents with
    | Some _ , None
    | None   , Some _  -> false
    | Some k1, Some k2 -> key_equal k1 k2
    | None   , None    -> true
  end
  && String.Map.equal key_equal t1.succ t2.succ

let edges t =
  begin match t.contents with
    | None   -> []
    | Some k -> [`Contents k]
  end
  @ String.Map.fold t.succ ~init:[]
    ~f:(fun ~key:_ ~data:k acc -> `Node k :: acc)

let empty =
  { contents = None;
    succ = String.Map.empty }

let is_empty e =
  e.contents = None && String.Map.is_empty e.succ

let leaf e =
  { contents = Some e;
    succ = String.Map.empty }

let is_leaf e =
  e.contents <> None && String.Map.is_empty e.succ

let contents_exn e =
  match e.contents with
  | None   -> raise Not_found
  | Some c -> c

module type S = sig
  type key
  type nonrec t = key t
  include IrminContents.S with type t := t
end

module type STORE = sig
  type key
  type contents
  type path = IrminPath.t
  type value = key t
  include IrminStore.AO with type key := key
                         and type value := value
  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> value -> contents Lwt.t option
  val succ: t -> value -> value Lwt.t String.Map.t
  val sub: t -> value -> IrminPath.t -> value option Lwt.t
  val sub_exn: t -> value -> IrminPath.t -> value Lwt.t
  val map: t -> value -> IrminPath.t -> (value -> value) -> value Lwt.t
  val update: t -> value -> IrminPath.t -> contents -> value Lwt.t
  val find: t -> value -> IrminPath.t -> contents option Lwt.t
  val find_exn: t -> value -> IrminPath.t -> contents Lwt.t
  val remove: t -> value -> IrminPath.t -> value Lwt.t
  val valid: t -> value -> IrminPath.t -> bool Lwt.t
  val merge: t -> key IrminMerge.t
  module Key: IrminKey.S with type t = key
  module Value: S with type key = key
end

module S (K: IrminKey.S) = struct

  type key = K.t

  module S = IrminIdent.Make(struct
      type nonrec t = K.t t with bin_io, compare, sexp
    end)

  include S

  let merge =
    IrminMerge.default (module S)

end

module SHA1 = S(IrminKey.SHA1)

module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (Contents: IrminContents.STORE with type key = K.t and type value = C.t)
    (Node    : IrminStore.AO       with type key = K.t and type value = K.t t) =
 struct

  module Key = K

  module Value = S(K)

  type key = K.t

  type contents = C.t

  type path = IrminPath.t

  type value = K.t t

  type t = Contents.t * Node.t

  let create () =
    Contents.create () >>= fun c ->
    Node.create ()     >>= fun t ->
    return (c, t)

  let add (_, t) n = match n with
    | { contents = Some k } ->
      if String.Map.is_empty n.succ then return k else Node.add t n
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
      Log.debugf "Not_found: %s" (K.to_string key);
      fail Not_found
    | Some v -> return v

  let mem (c, t) key =
    Node.mem t key >>= function
    | false -> Contents.mem c key
    | true  -> return true

  module Graph = IrminGraph.Make(K)(IrminTag.String)

  let list t keys =
    Log.debugf "list %s" (IrminMisc.pretty_list K.to_string keys);
    let pred = function
      | `Node k -> read_exn t k >>= fun node -> return (edges node)
      | _       -> return_nil in
    let max = IrminGraph.of_nodes keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = IrminGraph.to_nodes (Graph.vertex g) in
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
    let succ = String.Map.of_alist_exn succ in
    let node = { contents; succ } in
    add t node >>= fun key ->
    return (key, node)

  (* Merge the contents values together. *)
  let merge_contents c =
    IrminMerge.some (Contents.merge c)

  let merge_value (c, _) merge_key =
    let explode n = (n.contents, n.succ) in
    let implode (contents, succ) = { contents; succ } in
    let merge_pair = IrminMerge.pair (merge_contents c) (IrminMerge.map merge_key) in
    IrminMerge.biject (module Value) merge_pair implode explode

  let merge (c, _ as t) =
    let rec merge_key () =
      Log.debugf "merge";
      let merge = merge_value t (IrminMerge.apply (module K) merge_key ()) in
      IrminMerge.biject' (module K) merge (add t) (read_exn t) in
    merge_key ()

  let contents (c, _) n =
    match n.contents with
    | None   -> None
    | Some k -> Some (Contents.read_exn c k)

  let succ t node =
    String.Map.map ~f:(fun k -> read_exn t k) node.succ

  let next t node label =
    match String.Map.find node.succ label with
    | None   -> return_none
    | Some k -> read t k

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
    Log.debugf "find_exn %S" (IrminPath.to_string path);
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
    Log.debugf "find %S" (IrminPath.to_string path);
    sub t node path >>= function
    | None      -> return_none
    | Some node ->
      match contents t node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t node path =
    Log.debugf "valid %S" (IrminPath.to_string path);
    sub t node path >>= function
    | None      -> return false
    | Some node ->
      match contents t node with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    Log.debugf "map_children %s" label;
    let old_key = String.Map.find children label in
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
        | Some _, None     -> String.Map.remove children label
        | None  , Some k   -> String.Map.add children label k
        | Some k1, Some k2 ->
          if K.equal k1 k2 then children
          else
            String.Map.add children label k2 in
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
    Log.debugf "remove %S" (IrminPath.to_string path);
    map t node path (fun node -> empty)

  let update (c, _ as t) node path value =
    Log.debugf "update %S" (IrminPath.to_string path);
    Contents.add c value >>= fun k  ->
    map t node path (fun node -> { node with contents = Some k }) >>= fun n ->
    return n

end

module Rec (S: STORE) = struct
  include S.Key
  let merge =
    let merge ~origin ~old k1 k2 =
      S.create ()  >>= fun t  ->
      IrminMerge.merge (S.merge t) ~origin ~old k1 k2
    in
    IrminMerge.create' (module S.Key) merge
end
