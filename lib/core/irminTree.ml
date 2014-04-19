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

module type S = IrminStore.RW with type key = IrminPath.t
module Log = Log.Make(struct let section = "tree" end)
module Make (C: IrminContents.S) = struct

  type elt = (C.t option * t String.Table.t)
  and node =
    | Empty
    | Elt of elt
  and t = node ref

  type key = IrminPath.t

  type value = C.t

  let create () =
    return (ref Empty)

  let rec read' t = function
    | [] -> begin
        match !t with
        | Empty      -> None
        | Elt (c, _) -> c
      end
    | h::p -> begin
        match !t with
        | Empty        -> None
        | Elt (_, map) ->
          match Hashtbl.find map h with
          | None   -> None
          | Some t -> read' t p
      end

  let read t k =
    return (read' t k)

  let read_exn t k =
    match read' t k with
    | None   -> fail Not_found
    | Some v -> return v

  let mem t k =
    match read' t k with
    | None  -> return false
    | _     -> return true

  let list t k =
    return k

  let dump t =
    failwith "TODO"

  (* XXX: we do not clean the tree on remove *)
  let update' t k v =
    let update_contents t = match !t with
      | Empty      ->
        let new_t =
          if v = None then Empty
          else Elt (v, String.Table.create ()) in
        t := new_t
      | Elt (_, m) ->
        let new_t =
          if v = None && String.Table.is_empty m then Empty
          else Elt (v, m) in
        t := new_t in
    let rec aux t = function
      | []   -> update_contents t
      | h::p ->
        let c, m = match !t with
          | Empty      -> None, String.Table.create ()
          | Elt (c, m) -> c, m in
        t := Elt (c, m);
        match String.Table.find m h with
        | Some t -> aux t p
        | None   ->
          if v = None then ()
          else
            let child = ref Empty in
            String.Table.add_exn m h child;
            aux child p in
    aux t k

  let update t k v =
    update' t k (Some v);
    return_unit

  let remove t k =
    update' t k None;
    return_unit

  let watch _ =
    failwith "TODO"

end
