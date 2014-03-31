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

open Core_kernel.Std

module L = Log.Make(struct let section = "NODE" end)

type 'key t = {
  contents: 'key option;
  succ    : (string * 'key) list;
} with bin_io, compare, sexp

let to_json json_of_key t =
  let contents = match t.contents with
    | None   -> []
    | Some k -> [ "contents", json_of_key k ] in
  let succ = match t.succ with
    | [] -> []
    | s  -> [ "succ", Ezjsonm.list (Ezjsonm.pair IrminMisc.json_encode json_of_key) s ] in
  `O (contents @ succ)

let of_json key_of_json json =
  let contents =
    try
      let leaf = Ezjsonm.find json ["contents"] in
      Some (key_of_json leaf)
    with Not_found ->
      None in
  let succ =
    try
      let children = Ezjsonm.find json ["succ"] in
      let children =
        Ezjsonm.get_list
          (Ezjsonm.get_pair IrminMisc.json_decode_exn key_of_json)
          children in
      children
    with Not_found -> [] in
  { contents; succ }

let empty =
  { contents = None;
    succ = [] }

let singleton elt =
  { contents = Some elt;
    succ = [] }

module type S = sig
  type key
  type nonrec t = key t
  include IrminContents.S with type t := t
end

module type STORE = sig
  type key
  type contents
  type value = key t
  include IrminStore.AO with type key := key
                         and type value := value
  val node: t -> ?contents:contents -> ?succ:(string * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> value -> contents Lwt.t option
  val succ: t -> value -> (string * value Lwt.t) list
  val sub: t -> value -> IrminPath.t -> value option Lwt.t
  val sub_exn: t -> value -> IrminPath.t -> value Lwt.t
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

  module M = struct
    type nonrec t = K.t t
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Node"
  end
  include M
  include Identifiable.Make (M)

  let of_json = of_json K.of_json

  let to_json = to_json K.to_json

  let merge =
    IrminMerge.default equal

  let of_bytes str =
    IrminMisc.read bin_t (Bigstring.of_string str)

  let of_bytes_exn str =
    match of_bytes str with
    | None   -> raise (IrminContents.Invalid str)
    | Some t -> t

  let key t =
    K.of_bigarray (IrminMisc.write bin_t t)

end

module SHA1 = S(IrminKey.SHA1)

module Make
    (K: IrminKey.S)
    (C: IrminContents.S)
    (Contents: IrminStore.AO with type key = K.t and type value = C.t)
    (Node: IrminStore.AO with type key = K.t and type value = K.t t)
= struct

  module Key = K
  module Value = S(K)
  module Contents = IrminContents.Make(K)(C)(Contents)

  type key = K.t

  type contents = C.t

  type value = K.t t

  type t = Contents.t * Node.t

  open Lwt

  let create () =
    Contents.create () >>= fun c ->
    Node.create () >>= fun t ->
    return (c, t)

  let add (_, t) = function
    | { succ = []; contents = Some k } -> return k
    | node                             -> Node.add t node

  (* "leaf" nodes (ie. with no succ and some contents) are not
     duplicated: they are isomorphic to the contents itself and so:
     they live in the contents store and have the same key than their
     contents. *)
 let read (c, t) key =
    Node.read t key >>= function
    | Some _ as x -> return x
    | None        ->
      Contents.mem c key >>= function
      | true  -> return (Some (singleton key))
      | false -> return_none

  let read_exn t key =
    read t key >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem (c, t) key =
    Node.mem t key >>= function
    | false -> Contents.mem c key
    | true  -> return true

  module Graph = IrminGraph.Make(K)

  let list t key =
    L.debugf "list %s" (K.to_string key);
    read_exn t key >>= fun _ ->
    let pred = function
      | `Node k ->
        read_exn t k >>= fun node ->
        return (IrminGraph.of_nodes (List.map ~f:snd node.succ))
      | _       -> return_nil in
    Graph.closure pred ~min:[] ~max:[`Node key] >>= fun g ->
    return (IrminGraph.to_nodes (Graph.vertex g))

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
    let node = { contents; succ } in
    add t node >>= fun key ->
    return (key, node)

  (* Merge the contents values together. *)
  let merge_contents c =
    IrminMerge.some (Contents.merge c)

  let merge (c, _ as t) =
    let explode n = (n.contents, n.succ) in
    let implode (contents, succ) = { contents; succ } in
    let merge_pair merge=
      IrminMerge.pair (merge_contents c) (IrminMerge.assoc merge) in
    let merge_value merge =
      IrminMerge.map (merge_pair merge) implode explode in
    let rec merge () =
      Log.debugf "merge";
      IrminMerge.map' (merge_value (IrminMerge.apply merge ())) (add t) (read_exn t) in
    merge ()

  let contents (c, _) n =
    match n.contents with
    | None   -> None
    | Some k -> Some (Contents.read_exn c k)

  let succ t node =
    List.map ~f:(fun (l, k) -> l, read_exn t k) node.succ

  let next t node label =
    List.Assoc.find (succ t node) label

  let sub_exn t node path =
    let rec aux node path =
      match path with
    | []    -> return node
    | h::tl ->
      match next t node h with
      | None      -> fail Not_found
      | Some node -> node >>= fun node -> aux node tl in
    aux node path

  let sub t node path =
    catch
      (fun () ->
         sub_exn t node path >>= fun node ->
         return (Some node))
      (function Not_found -> return_none | e -> fail e)

  let find_exn t node path =
    sub t node path >>= function
    | None      -> fail Not_found
    | Some node ->
      match contents t node with
      | None   -> fail Not_found
      | Some b -> b

  let find t node path =
    sub t node path >>= function
    | None      -> return_none
    | Some node ->
      match contents t node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t node path =
    sub t node path >>= function
    | None      -> return false
    | Some node ->
      match contents t node with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    let rec aux acc = function
      | [] ->
        f empty >>= fun node ->
        if node = empty then return (List.rev acc)
        else
          add t node >>= fun k ->
          return (List.rev_append acc [label, k])
      | (l, k) as child :: children ->
        if l = label then
          read t k >>= function
          | None      -> fail (IrminKey.Invalid (K.to_string k))
          | Some node ->
            f node >>= fun node ->
            if node = empty then return (List.rev_append acc children)
            else
              add t node >>= fun k ->
              return (List.rev_append acc ((l, k) :: children))
        else
          aux (child::acc) children
    in
    aux [] children

  let map_subnode t node path f =
    let rec aux node = function
      | []      -> return (f node)
      | h :: tl ->
        map_children t node.succ (fun node -> aux node tl) h >>= fun succ ->
        return { node with succ } in
    aux node path

  let remove t node path =
    map_subnode t node path (fun node -> empty)

  let update (c, _ as t) node path value =
    Contents.add c value >>= fun k  ->
    map_subnode t node path (fun node -> { node with contents = Some k })

end
