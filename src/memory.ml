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

  type key = K of string

  type label = L of string

  type tree = {
    value   : key option;
    children: (label * key) list;
  }

  type revision = {
    parents: revision list;
    tree   : tree;
  }

  type value =
    | Blob of blob
    | Tree of tree
    | Revision of revision

  type tag = T of string

  type t = {
    store: (key, value) Hashtbl.t;
    tags : (tag, key  ) Hashtbl.t;
  }

  type remote = Uri.t

end

let sha1 (value: Types.value) =
  let str = Marshal.to_string value [] in
  Types.K (Lib.Misc.sha1 str)

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

  type trie = (label, key) Lib.Trie.t

  (* Convert a tree into a lazy trie *)
  let rec mktrie t tree: trie =
    let child (label, key) =
      match Low.read t key with
      | Some (Tree tree) -> (label, mktrie t tree)
      | _                -> failwith "mktree" in
    let children = lazy (
      List.map child tree.children
    ) in
    Lib.Trie.create ?value:tree.value ~children ()

  (* Save a lazy trie into a the database *)
  (* XXX: not very ineficient *)
  let rec save t (trie:trie) =
    let children = List.map (fun (label, tree) ->
        (label, save t tree)
      ) (Lib.Trie.children trie) in
    let value =
      try Some (Lib.Trie.find trie [])
      with Not_found -> None in
    Low.write t (Tree { children; value })

  (* Save a trie in the database and return its corresponding tree.*)
  (* XXX: not very efficient *)
  let mktree t trie =
    let key = save t trie in
    match Low.read t key with
    | Some (Tree t) -> t
    | _             -> failwith "tree"

  let get t tree labels =
    let trie = mktrie t tree in
    try Some (Lib.Trie.find trie labels)
    with Not_found -> None

  (* XXX: not very efficient *)
  let set t tree labels value =
    let trie = mktrie t tree in
    let key = Low.write t value in
    let trie = Lib.Trie.set trie labels key in
    mktree t trie

  exception EAGAIN
  exception Invalid_values

  let merge t fn t1 t2 =
    let t1 = mktrie t t1 in
    let t2 = mktrie t t2 in
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
    try Some (mktree t (Lib.Trie.merge ~values t1 t2))
    with EAGAIN | Not_found -> None

  let succ t tree =
    let trie = mktrie t tree in
    let children = Lib.Trie.children trie in
    List.map (fun (label,trie) -> (label, mktree t trie)) children

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
