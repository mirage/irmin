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

open Core_kernel.Std

module L = Log.Make(struct let section = "TREE" end)

type 'key t = {
  blob    : 'key option;
  children: (string * 'key) list;
} with bin_io, compare, sexp


let empty = {
  blob = None;
  children = [];
}


module type S = sig
  type key
  include IrminBase.S with type t = key t
end

module type STORE = sig
  type key
  type blob
  type value = key t
  include IrminStore.AO with type key := key
                         and type value := value
  val tree: t -> ?value:blob -> (string * value) list -> key Lwt.t
  val blob: t -> value -> blob Lwt.t option
  val children: t -> value -> (string * value Lwt.t) list
  val sub: t -> value -> IrminPath.t -> value option Lwt.t
  val sub_exn: t -> value -> IrminPath.t -> value Lwt.t
  val update: t -> value -> IrminPath.t -> blob -> value Lwt.t
  val find: t -> value -> IrminPath.t -> blob option Lwt.t
  val find_exn: t -> value -> IrminPath.t -> blob Lwt.t
  val remove: t -> value -> IrminPath.t -> value Lwt.t
  val valid: t -> value -> IrminPath.t -> bool Lwt.t
  module Key: IrminKey.S with type t = key
  module Value: S with type key = key
end

module S (K: IrminBase.S) = struct

  type key = K.t
  module M = struct
    type nonrec t = K.t t
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Tree"
    let name = "tree"
  end
  include M
  include Identifiable.Make (M)

  module XValue = struct
    include IrminBase.Option(K)
    let module_name = "Value"
  end

  module XChildren = struct
    include IrminBase.List(IrminBase.Pair(IrminBase.String)(K))
    let module_name = "Children"
  end

  module XTree = struct
    include IrminBase.Pair(XValue)(XChildren)
    let module_name = "Tree"
  end

  let pretty t =
    XTree.pretty (t.blob, t.children)

  let to_json t =
    XTree.to_json (t.blob, t.children)

  let of_json j =
    let blob, children = XTree.of_json j in
    { blob; children }

  (* |-----|---------| *)
  (* | 'T' | PAYLOAD | *)
  (* |-----|---------| *)

  let header = "T"

  let sizeof t =
    1 + XTree.sizeof (t.blob, t.children)

  let set buf t =
    Mstruct.set_string buf header;
    XTree.set buf (t.blob, t.children)

  let get buf =
    let h = Mstruct.get_string buf 1 in
    if String.(header <> h) then None
    else match XTree.get buf with
      | None                  -> None
      | Some (blob, children) -> Some { blob; children }


end

module SHA1 = S(IrminKey.SHA1)

module Make
    (K: IrminKey.S)
    (B: IrminBlob.S with type key = K.t)
    (Blob: IrminStore.AO with type key = K.t and type value = B.t)
    (Tree: IrminStore.AO with type key = K.t and type value = K.t t)
= struct

  type key = K.t

  type blob = B.t

  type value = K.t t

  type t = Blob.t * Tree.t

  module Key = K
  module Value = S(K)

  open Lwt

  let create () =
    Blob.create () >>= fun b ->
    Tree.create () >>= fun t ->
    return (b, t)

  let add (_, t) tree =
    Tree.add t tree

  let read (_, t) key =
    Tree.read t key

  let read_exn (_, t) key =
    Tree.read_exn t key

  let mem (_, t) key =
    Tree.mem t key

  module Graph = IrminGraph.Make(K)

  let list t key =
    L.debugf "list %s" (K.pretty key);
    read_exn t key >>= fun _ ->
    let pred k =
      read_exn t k >>= fun r -> return (List.map ~f:snd r.children) in
    Graph.closure pred ~min:[] ~max:[key] >>= fun g ->
    return (Graph.vertex g)

  let contents (_, t) =
    Tree.contents t

  let tree (b, _ as t) ?value children =
    L.debug (lazy "tree");
    begin match value with
      | None   -> return_none
      | Some v -> Blob.add b v >>= fun k -> return (Some k)
    end
    >>= fun blob ->
    Lwt_list.map_p (fun (l, tree) ->
        add t tree >>= fun k ->
        return (l, k)
      ) children
    >>= fun children ->
    let tree = { blob; children } in
    add t tree

  let blob (b, _) tree =
    match tree.blob with
    | None   -> None
    | Some k -> Some (Blob.read_exn b k)

  let children t tree =
    List.map ~f:(fun (l, k) -> l, read_exn t k) tree.children

  let child t tree label =
    List.Assoc.find (children t tree) label

  let sub_exn t tree path =
    let rec aux tree path =
      match path with
    | []    -> return tree
    | h::tl ->
      match child t tree h with
      | None      -> fail Not_found
      | Some tree -> tree >>= fun tree -> aux tree tl in
    aux tree path

  let sub t tree path =
    catch
      (fun () ->
         sub_exn t tree path >>= fun tree ->
         return (Some tree))
      (function Not_found -> return_none | e -> fail e)

  let find_exn t tree path =
    sub t tree path >>= function
    | None      -> fail Not_found
    | Some tree ->
      match blob t tree with
      | None   -> fail Not_found
      | Some b -> b

  let find t tree path =
    sub t tree path >>= function
    | None      -> return_none
    | Some tree ->
      match blob t tree with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t tree path =
    sub t tree path >>= function
    | None      -> return false
    | Some tree ->
      match blob t tree with
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
          | None      -> fail (IrminKey.Invalid (K.pretty k))
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
        map_children t tree.children (fun tree -> aux tree tl) h
        >>= fun children ->
        return { tree with children } in
    aux tree path

  let remove t tree path =
    map_subtree t tree path (fun tree -> { tree with blob = None })

  let update (b, _ as t) tree path value =
    Blob.add b value >>= fun k  ->
    map_subtree t tree path (fun tree -> { tree with blob = Some k })

end
