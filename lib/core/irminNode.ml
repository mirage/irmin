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

module L = Log.Make(struct let section = "NODE" end)

type 'key t =
  | Leaf of 'key
  | Node of (string * 'key) list
with bin_io, compare, sexp

let to_json json_of_key = function
  | Leaf k        -> `O ["leaf", json_of_key k]
  | Node children ->
    `O ["children",
        Ezjsonm.list (Ezjsonm.pair IrminMisc.json_encode json_of_key) children ]

let of_json key_of_json json =
  if Ezjsonm.mem json ["leaf"] then
    let leaf = Ezjsonm.find json ["leaf"] in
    Leaf (key_of_json leaf)
  else
    let children = Ezjsonm.find json ["children"] in
    let children =
      Ezjsonm.get_list
        (Ezjsonm.get_pair IrminMisc.json_decode_exn key_of_json)
        children in
    Node children

let empty = Node []

module type S = sig
  type key
  include IrminBlob.S with type t = key t
end

module type STORE = sig
  type key
  type blob
  type value = key t
  include IrminStore.AO with type key := key
                         and type value := value
  val leaf: t -> blob -> key Lwt.t
  val node: t -> (string * value) list -> key Lwt.t
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

  let merge ~old:_ _ _ =
    failwith "Node.merge: TODO"

  let of_bytes str =
    IrminMisc.read bin_t (Bigstring.of_string str)

  let of_bytes_exn str =
    match of_bytes str with
    | None   -> raise (IrminBlob.Invalid str)
    | Some t -> t

  let key t =
    K.of_bigarray (IrminMisc.write bin_t t)

end

module SHA1 = S(IrminKey.SHA1)

module Make
    (K: IrminKey.S)
    (B: IrminBlob.S)
    (Blob: IrminStore.AO with type key = K.t and type value = B.t)
    (Node: IrminStore.AO with type key = K.t and type value = K.t t)
= struct

  type key = K.t

  type blob = B.t

  type value = K.t t

  type t = Blob.t * Node.t

  module Key = K
  module Value = S(K)

  open Lwt

  let create () =
    Blob.create () >>= fun b ->
    Node.create () >>= fun t ->
    return (b, t)

  let add (_, t) = function
    | Leaf k -> return k
    | node   -> Node.add t node

  let read (b, t) key =
    Node.read t key >>= function
    | Some _ as x -> return x
    | None        ->
      Blob.mem b key >>= function
      | true  -> return (Some (Leaf key))
      | false -> return_none

  let read_exn t key =
    read t key >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem (b, t) key =
    Node.mem t key >>= function
    | false -> Blob.mem b key
    | true  -> return true

  module Graph = IrminGraph.Make(K)

  let list t key =
    L.debugf "list %s" (K.to_string key);
    read_exn t key >>= fun _ ->
    let pred = function
      | `Node k ->
        begin
          read_exn t k >>= function
          | Leaf b  -> return_nil
          | Node ts -> return (IrminGraph.of_nodes (List.map ~f:snd ts))
        end
      | _ -> return_nil in
    Graph.closure pred ~min:[] ~max:[`Node key] >>= fun g ->
    return (IrminGraph.to_nodes (Graph.vertex g))

  let contents (_, t) =
    Node.contents t

  let leaf (b, _ as t) blob =
    Blob.add b blob >>= fun k ->
    add t (Leaf k)

  let node t children =
    Lwt_list.map_p (fun (l, node) ->
        add t node >>= fun k ->
        return (l, k)
      ) children
    >>= fun children ->
    add t (Node children)

  let blob (b, _) = function
    | Node _ -> None
    | Leaf k -> Some (Blob.read_exn b k)

  let children_raw = function
    | Leaf _  -> []
    | Node ts -> ts

  let children t node =
    List.map ~f:(fun (l, k) -> l, read_exn t k) (children_raw node)

  let child t node label =
    List.Assoc.find (children t node) label

  let sub_exn t node path =
    let rec aux node path =
      match path with
    | []    -> return node
    | h::tl ->
      match child t node h with
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
      match blob t node with
      | None   -> fail Not_found
      | Some b -> b

  let find t node path =
    sub t node path >>= function
    | None      -> return_none
    | Some node ->
      match blob t node with
      | None   -> return_none
      | Some b -> b >>= fun b -> return (Some b)

  let valid t node path =
    sub t node path >>= function
    | None      -> return false
    | Some node ->
      match blob t node with
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
        map_children t (children_raw node) (fun node -> aux node tl) h
        >>= fun children ->
        return (Node children) in
    aux node path

  let remove t node path =
    map_subnode t node path (fun node -> empty)

  let update (b, _ as t) node path value =
    Blob.add b value >>= fun k  ->
    map_subnode t node path (fun node -> Leaf k)

end
