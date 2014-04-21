(*
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

open Lwt
open Test_common
open Core_kernel.Std

let suite k stores =

  let (module K), (module C), (module R) = modules k in
  let module Dispatch = IrminDispatch.Make(K)(C)(R) in
  let module V = IrminValue.S(K)(C) in

  let store =
    let keys = K.Table.create () in
    let values = V.Table.create () in
    let key k =
      match Hashtbl.find keys k with
      | None   -> failwith ("Cannot find " ^ K.to_string k)
      | Some i -> i in
    let value v =
      match Hashtbl.find values v with
      | Some n -> (fun _ -> ()), n
      | None   ->
        let i = Random.int (Array.length stores) in
        Hashtbl.replace values v i;
        (fun k -> Hashtbl.replace keys k i), i
    in
    let stores = Array.map ~f:(fun t -> t.store) stores in
    (* XXX: sorry ... *)
    let stores = (Obj.magic stores: (K.t, C.t, R.t) Irmin.t array) in
    Dispatch.(cast (create ~key ~value stores)) in
  let stores = Array.to_list stores in

  let init () =
    Lwt_list.iter_p (fun t -> t.init ()) stores in
  let clean () =
    Lwt_list.iter_p (fun t -> t.clean ()) stores in
  {
    name  = "DISPATCH" ^ string_of_kind k;
    kind  = k;
    init; clean; store;
  }
