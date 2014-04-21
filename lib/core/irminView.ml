(*
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
open Core_kernel.Std

module Log = Log.Make(struct let section = "tree" end)

module type S = sig
  include IrminStore.RW with type key = IrminPath.t
  type internal_key
  val make:
    contents:(internal_key -> value option Lwt.t) ->
    node:(internal_key ->  internal_key IrminNode.t option Lwt.t) ->
    internal_key ->
    t Lwt.t
end

module Make (Store: IrminContents.STORE) = struct

  module K = Store.Key
  module C = Store.Value

  type internal_key = Store.key

  module Contents = struct

    type contents_or_key =
      | Key of K.t
      | Contents of C.t

    type t = contents_or_key ref
    (* Same as [IrminContents.t] but can either be a raw contents or a
       key that will be fetched lazily. *)

    let create c =
      ref (Contents c)

    let key k =
      ref (Key k)

    let read ~contents t =
      match !t with
      | Contents c -> return (Some c)
      | Key k      ->
        contents k >>= function
        | None   -> return_none
        | Some c ->
          t := Contents c;
          return (Some c)

  end

  module Node = struct

    type node = {
      contents: Contents.t option;
      succ    : t String.Map.t;
    }

    and node_or_key  =
      | Key of K.t
      | Node of node

    and t = node_or_key ref
    (* Similir to [IrminNode.t] but using where all of the values can
       just be keys. *)

    let create' contents succ =
      Node { contents; succ }

    let create contents succ =
      ref (create' contents succ)

    let key k =
      ref (Key k)

    let empty () =
      create None String.Map.empty

    let import' n =
      let contents = match n.IrminNode.contents with
        | None   -> None
        | Some k -> Some (Contents.key k) in
      let succ = String.Map.map ~f:key n.IrminNode.succ in
      { contents; succ }

    let import n =
      ref (import' n)

    let read ~node t =
      match !t with
      | Node n   -> return (Some n)
      | Key k    ->
        node k >>= function
        | None   -> return_none
        | Some n ->
          t := Node n;
          return (Some n)

    let contents ~node ~contents t =
     read node t >>= function
     | None   -> return_none
     | Some c ->
       match c.contents with
       | None   -> return_none
       | Some c -> Contents.read contents c

    let update_contents ~node t v =
      read node t >>= function
      | None   -> if v = None then return_unit else fail Not_found (* XXX ? *)
      | Some n ->
        let n = match v with
          | None   -> { n with contents = None }
          | Some c -> { n with contents = Some (Contents.create c) } in
        t := Node n;
        return_unit

    let update_succ ~node t succ =
      read node t >>= function
      | None   -> if Map.is_empty succ then return_unit else fail Not_found (* XXX ? *)
      | Some n ->
        t := Node { n with succ };
        return_unit

  end

  type t = {
    node    : K.t -> Node.node option Lwt.t;
    contents: K.t -> C.t option Lwt.t;
    view    : Node.t;
  }

  type key = IrminPath.t

  type value = C.t

  let create () =
    let node _ = return_none in
    let contents _ = return_none in
    let view = Node.empty () in
    return { node; contents; view }

  let mapo fn = function
    | None   -> return_none
    | Some x -> fn x >>= fun y -> return (Some y)

  let make  ~contents ~node key =
    node key >>= function
    | None   -> fail Not_found
    | Some n ->
      let node k =
        node k >>= function
        | None   -> return_none
        | Some n -> return (Some (Node.import' n)) in
      let view = Node.key key in
      return { node; contents; view }

  let read { node; contents; view } path =
    let rec aux = function
      | []   -> Node.contents ~node ~contents view
      | h::p ->
        Node.read node view >>= function
        | None               -> return_none
        | Some { Node.succ } ->
          match Map.find succ h with
          | None   -> return_none
          | Some v -> aux p in
    aux path

  let read_exn t k =
    read t k >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem t k =
    read t k >>= function
    | None  -> return false
    | _     -> return true

  let list t k =
    return k

  let dump t =
    failwith "TODO"

  (* XXX: clean the tree on remove (not sure it's worth, though, as
     they will be GCed when the view is merged back to the store) *)
  let update' t k v =
    let rec aux view = function
      | []   -> Node.update_contents t.node view v
      | h::p ->
        Node.read t.node view >>= function
        | None   -> if v = None then return_unit else fail Not_found (* XXX ?*)
        | Some n ->
          match Map.find n.Node.succ h with
          | Some view -> aux view p
          | None      ->
            if v = None then return_unit
            else
              let child = Node.empty () in
              let succ = Map.add n.Node.succ h child in
              Node.update_succ t.node view succ >>= fun () ->
              aux child p in
    aux t.view k

  let update t k v =
    update' t k (Some v)

  let remove t k =
    update' t k None

  let watch _ =
    failwith "TODO"

end
