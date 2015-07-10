(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let todo m name = failwith (m ^ "." ^ name ^ ": TODO")

let ok_or_duplicated_tag =
  let module M = struct
    type t = [ `Ok | `Duplicated_tag | `Empty_head ]
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash

    let to_string = function
      | `Ok -> "ok"
      | `Duplicated_tag -> "duplicated-tag"
      | `Empty_head -> "empty-head"
    let to_json t = `String (to_string t)

    let of_json = function
      | `String "ok" -> `Ok
      | `String "duplicated-tag" -> `Duplicated_tag
      | `String "empty-head" -> `Empty_head
      | j -> Ezjsonm.parse_error j "ok_or_duplicated_tag"

    let todo = todo "ok_or_duplicated_tag"
    let write _ = todo "write"
    let read _ = todo "read"
    let size_of _ = todo "size_of"
  end in
  (module M: Tc.S0 with type t = M.t)

let ok_or_error =
  let module M = struct
    type t  = [`Ok | `Error]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_json = function
      | `Ok    -> `String "ok"
      | `Error -> `String "error"

    let of_json = function
      | `String "ok"    -> `Ok
      | `String "error" -> `Error
      | j -> Ezjsonm.parse_error j "ok_or_error"

    let todo = todo "ok_or_error"
    let read _ = todo "read"
    let write _ = todo "write"
    let size_of _ = todo "size_of"
  end in
  (module M: Tc.S0 with type t = M.t)

let lca (type a) (head: a Tc.t) =
  let (module Head: Tc.S0 with type t = a) = head in
  let module M = struct
    module HL = Tc.List(Head)
    type t = [`Ok of Head.t list | `Max_depth_reached | `Too_many_lcas]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let to_json = function
      | `Ok x -> `O ["ok", HL.to_json x]
      | `Max_depth_reached -> `A [`String "max-depth-reached" ]
      | `Too_many_lcas -> `A [`String "too-many-lcas"]

    let of_json = function
      | `O [ "ok", j ] -> `Ok (HL.of_json j)
      | `A [`String "max-depth-reached" ] -> `Max_depth_reached
      | `A [`String "too-many-lcas"] -> `Too_many_lcas
      | j -> Ezjsonm.parse_error j "LCA.of_json"

    let todo name = todo "lca." name
    let read _ = todo "read"
    let write _ = todo "write"
    let size_of _ = todo "size_of"
  end in
  (module M: Tc.S0 with type t = M.t)

let start_stream = "¡start!"
let irmin_header = "X-IrminVersion"
