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

(* In memory representation of Irminsule *)
module Types = struct

  type blob = B of string

  type key = string

  type label = string

  type tree = (label, key) Lib.Trie.t

  type revision = {
    parents: revision list;
    tree   : tree;
  }

  type value =
    | Blob of blob
    | Tree of tree
    | Revision of revision

  type tag = string

  type t = {
    store: (key, value) Hashtbl.t;
    tags : (tag, key  ) Hashtbl.t;
  }

end

let sha1 (value: Types.value) =
  let str = Marshal.to_string value [] in
  Lib.Misc.sha1 str

module Low: Database.LOW with module T = Types = struct

  module T = Types
  open T

  let create () = {
    store = Hashtbl.create 1024;
    tags  = Hashtbl.create 64;
  }

  let write t value =
    let sha1 = sha1 value in
    Hashtbl.add t.store sha1 value;
    sha1

  let read t sha1 =
    try Some (Hashtbl.find t.store sha1)
    with Not_found -> None

  let list t =
    Hashtbl.fold (fun k _ l -> k::l) t.store []

end

module Tree: Database.TREE with module T = Types = struct

  module T = Types
  open T

  let get t tree labels =
    try Some (Lib.Trie.find tree labels)
    with Not_found -> None

  let callback t tree =
    let _key = Low.write t (Types.Tree tree) in
    ()

  let set t tree labels value =
    let key = Low.write t value in
    Lib.Trie.set (callback t) tree labels key

  exception EAGAIN
  exception Invalid_values

  let merge t fn t1 t2 =
    let values l1 l2 =
      match l1, l2 with
      | [k1], [k2] ->
        begin match Low.read t k1, Low.read t k2 with
          | None, _ | _, None -> raise Not_found
          | Some v1, Some v2  ->
            match fn v1 v2 with
            | Some v3 -> let k3 = Low.write t v3 in [k3]
            | None    -> raise EAGAIN
        end
      | _ -> raise Invalid_values
    in
    try Some (Lib.Trie.merge (callback t) ~values t1 t2)
    with EAGAIN | Not_found -> None

  let succ t tree =
    Lib.Trie.children tree

end

module Revision: Database.REVISION with module T = Types = struct

  module T = Types
  open T

  let pred t rev =
    rev.parents

  let tree t rev =
    rev.tree

  let commit t parents tree =
    let rev = { parents; tree } in
    let _key = Low.write t (Revision rev) in
    rev

end

module Tag: Database.TAG with module T = Types = struct

  module T = Types
  open T

  let tags t =
    Hashtbl.fold (fun t _ l -> t::l) t.tags []

  let revision t tag =
    try
      let key = Hashtbl.find t.tags tag in
      match Hashtbl.find t.store key with
      | Revision r -> Some r
      | _          -> None
    with Not_found -> None

  let tag t tag revision =
    let key = sha1 (Revision revision) in
    if Hashtbl.mem t.store key then
      Hashtbl.replace t.tags tag key
    else
      raise Not_found

end
