(*
 * Copyright (c) 2013 Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type ('a, 'b) node = {
  value   : 'a option;
  children: (string * 'b) list;
}

type ('a, 'b) store = {
  v: 'a;
  t: 'b;
}

module type STORE = sig
  type key
  type value
  type tree = (key, key) node
  type path = string list
  include IrminBase.S with type t := tree
  include IrminStore.A with type key := key
                        and type value := tree
  val empty: tree
  val tree: t -> ?value:value -> (string * tree) list -> key Lwt.t
  val value: t -> tree -> value Lwt.t option
  val children: t -> tree -> (string * tree Lwt.t) list
  val sub: t -> tree -> path -> tree option Lwt.t
  val update: t -> tree -> path -> value -> tree Lwt.t
  val find: t -> tree -> path -> value option Lwt.t
  val find_exn: t -> tree -> path -> value Lwt.t
  val remove: t -> tree -> path -> tree Lwt.t
  val mem: t -> tree -> path -> bool Lwt.t
end

module Make
    (S: IrminStore.A_RAW)
    (K: IrminKey.S with type t = S.key)
    (V: IrminValue.STORE with type key = S.key) =
struct

  open Lwt

  type key = K.t

  type value = V.value

  type tree = (K.t, K.t) node

  type path = string list

  module Tree = struct

    type t = tree

    module XValue = struct
      include IrminBase.Option(K)
      let name = "value"
    end

    module XChildren = struct
      include IrminBase.List(IrminBase.Pair(IrminBase.String)(K))
      let name = "children"
    end

    module XTree = struct
      include IrminBase.Pair(XValue)(XChildren)
      let name = "tree"
    end

    let name = XTree.name

    let compare t1 t2 =
      XTree.compare (t1.value, t1.children) (t2.value, t2.children)

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      XTree.hash (t.value, t.children)

    let pretty t =
      XTree.pretty (t.value, t.children)

    let to_json t =
      XTree.to_json (t.value, t.children)

    let of_json j =
      let value, children = XTree.of_json j in
      { value; children }

    let sizeof t =
      XTree.sizeof (t.value, t.children)

    let key t =
      let init = match t.value with
        | None   -> []
        | Some v -> [v] in
      let keys = List.fold_left (fun acc (l,k) ->
          K.create l :: k :: acc
        ) init t.children in
      K.concat keys

    let set buf t =
      XTree.set buf (t.value, t.children)

    let get buf =
      let value, children = XTree.get buf in
      { value; children }

    let dump t =
      XTree.dump (t.value, t.children)
  end

  module Store = IrminStore.MakeI(S)(K)(Tree)

  include (Tree: IrminBase.S with type t := tree)

  type t = (V.t, Store.t) store

  let create t =
    V.create () >>= fun v ->
    Store.create () >>= fun t ->
    return { v; t }

  let add t tree =
    Store.add t.t tree

  let read t key =
    Store.read t.t key

  let read_exn t key =
    Store.read_exn t.t key

  let mem t key =
    Store.mem t.t key

  let list t key =
    Store.list t.t key

  let empty = {
    value = None;
    children = [];
  }

  let tree t ?value children =
    begin match value with
      | None   -> return_none
      | Some v -> V.add t.v v >>= fun k -> return (Some k)
    end
    >>= fun value ->
    Lwt_list.map_p (fun (l, tree) ->
        add t tree >>= fun k ->
        return (l, k)
      ) children
    >>= fun children ->
    let tree = { value; children } in
    add t tree

  let value t tree =
    match tree.value with
    | None   -> None
    | Some k -> Some (V.read_exn t.v k)

  let children t tree =
    List.map (fun (l, k) -> l, read_exn t k) tree.children

  let child t tree label =
    try Some (List.assoc label (children t tree))
    with Not_found -> None

  let sub t tree path =
    let rec aux tree = function
    | []    -> return (Some tree)
    | h::tl ->
      match child t tree h with
      | None      -> return None
      | Some tree -> tree >>= fun tree -> aux tree tl in
    aux tree path

  let find_exn t tree path =
    sub t tree path >>= function
    | None      -> fail Not_found
    | Some tree ->
      match value t tree with
      | None   -> fail Not_found
      | Some v -> v

  let find t tree path =
    sub t tree path >>= function
    | None      -> return_none
    | Some tree ->
      match value t tree with
      | None   -> return_none
      | Some v -> v >>= fun v -> return (Some v)

  let mem t tree path =
    sub t tree path >>= function
    | None      -> return false
    | Some tree ->
      match value t tree with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    let rec aux acc = function
      | [] ->
        f empty >>= fun tree ->
        if tree = empty then return (List.rev acc)
        else
          add t tree >>= fun k ->
          return (List.rev_append acc [label, k])
      | (l, k) as child :: children ->
        if l = label then
          read t k >>= function
          | None      -> fail (K.Invalid k)
          | Some tree ->
            f tree >>= fun tree ->
            if tree = empty then return (List.rev_append acc children)
            else
              add t tree >>= fun k ->
              return (List.rev_append acc ((l, k) :: children))
        else
          aux (child::acc) children
    in
    aux [] children

  let map_subtree t tree path f =
    let rec aux tree = function
      | []      -> return (f tree)
      | h :: tl ->
        map_children t tree.children (fun t -> aux tree tl) h
        >>= fun children ->
        return { tree with children } in
    aux tree path

  let remove t tree path =
    map_subtree t tree path (fun tree -> { tree with value = None })

  let update t tree path value =
    V.add t.v value >>= fun k ->
    map_subtree t tree path (fun tree -> { tree with value = Some k })

end
