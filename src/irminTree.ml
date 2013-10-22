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

module type STORE = sig
  type t
  include IrminStore.S with type value := t
  type value
  val empty: t
  val create: ?value:key -> (string * key) list -> t
  val value: t ->  value Lwt.t option
  val children: t -> (string * t Lwt.t) list
  val sub: t -> string list -> t option Lwt.t
  val add: t -> string list -> value -> t Lwt.t
  val find: t -> string list -> value Lwt.t
  val remove: t -> string list -> t Lwt.t
  val mem: t -> string list -> bool Lwt.t
  val iter: (string list -> value -> unit Lwt.t) -> t -> unit Lwt.t
end

module Make
    (S: IrminStore.RAW)
    (K: IrminKey.S)
    (V: IrminValue.STORE with type key = K.t) =
struct

  open Lwt

  module Tree = struct

    type t = {
      value   : K.t option;
      children: (string * K.t) list;
    }

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

  module Store = IrminStore.Make(S)(K)(Tree)

  include Tree

  include (Store: module type of Store with type value := Tree.t)

  type value = V.value

  let empty = {
    value = None;
    children = [];
  }

  let create ?value children =
    { value; children }

  let value t =
    match t.value with
    | None   -> None
    | Some k -> Some (V.read_exn k)

  let children t =
    List.map (fun (l, k) ->
        l,
        S.read_exn (K.dump k) >>= fun b ->
        return (Tree.get b)
      ) t.children

  let child t label =
    try Some (List.assoc label (children t))
    with Not_found -> None

  let sub t path =
    let rec aux t = function
    | []    -> return (Some t)
    | h::tl ->
      match child t h with
      | None   -> return None
      | Some t -> t >>= fun t -> aux t tl in
    aux t path

  let find t path =
    sub t path >>= function
    | None   -> fail Not_found
    | Some t ->
      match value t with
      | None   -> fail Not_found
      | Some v -> v

  let mem t path =
    sub t path >>= function
    | None   -> return false
    | Some t ->
      match value t with
      | None   -> return false
      | Some _ -> return true

  let iter (f:string list -> value -> unit Lwt.t) t =
    let rec aux path t =
      let apply = match value t with
        | None   -> return ()
        | Some v -> bind v (f (List.rev path)) in
      apply >>= fun () ->
      Lwt_list.iter_s
        (fun (l, t) -> t >>= aux (l::path))
        (children t) in
    aux [] t

  let map_children children f label =
    let rec aux acc = function
      | [] ->
        f empty >>= fun t ->
        if t = empty then return (List.rev acc)
        else
          write t >>= fun k ->
          return (List.rev_append acc [label, k])
      | (l, k) as child :: children ->
        if l = label then
          read k >>= function
          | None   -> fail (K.Invalid k)
          | Some t ->
            f t >>= fun t ->
            if t = empty then return (List.rev_append acc children)
            else
              write t >>= fun k ->
              return (List.rev_append acc ((l, k) :: children))
        else
          aux (child::acc) children
    in
    aux [] children

  let map_subtree t path f =
    let rec aux t = function
      | []      -> return (f t)
      | h :: tl ->
        map_children
          t.children
          (fun t -> aux t tl)
          h
        >>= fun children ->
        return { t with children } in
    aux t path

  let remove tree path =
    map_subtree tree path (fun t -> { t with value = None })

  let add tree path value =
    V.write value >>= fun k ->
    map_subtree tree path (fun t -> { t with value = Some k })

end
