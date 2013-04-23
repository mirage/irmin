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

module J = struct

  open Types

  (* From http://erratique.ch/software/jsonm/doc/Jsonm.html#datamodel *)
  type json =
    [ `Null | `Bool of bool | `Float of float| `String of string
    | `A of json list | `O of (string * json) list ]

  exception Escape of ((int * int) * (int * int)) * Jsonm.error

  let json_of_src ?encoding src =
    let dec d = match Jsonm.decode d with
      | `Lexeme l -> l
      | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
      | `End | `Await -> assert false
    in
    let rec value v k d = match v with
      | `Os -> obj [] k d  | `As -> arr [] k d
      | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
      | _ -> assert false
    and arr vs k d = match dec d with
      | `Ae -> k (`A (List.rev vs)) d
      | v -> value v (fun v -> arr (v :: vs) k) d
    and obj ms k d = match dec d with
      | `Oe -> k (`O (List.rev ms)) d
      | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
      | _ -> assert false
    in
    let d = Jsonm.decoder ?encoding src in
    try `JSON (value (dec d) (fun v _ -> v) d) with
    | Escape (r, e) -> `Error (r, e)

  let json_of_blob  (B b) = `String b
  let json_of_key   (K k) = `String k
  let json_of_tag   (T t) = `String t
  let json_of_label (L l) = `String l

  let json_of_tree tree =
    let value = match tree.value with
      | None    -> `Null
      | Some  v -> json_of_key v in
    let child (l, k) = `A [json_of_label l; json_of_key k] in
    let children = match tree.children with
      | [] -> `Null
      | l  -> `A (List.map child l) in
    `O [ ("value", value); ("children", children) ]

  let json_of_revision r =
    let parents =
      List.map (fun r -> sha1 (Revision r)) r.parents in
    let tree =
      sha1 (Tree r.tree) in
    `O [ ("parents", `A (List.map json_of_key parents));
         ("tree"   , json_of_key tree) ]

  let json_of_value = function
    | Blob b     -> `O ("blob", json_of_blob b)
    | Tree t     -> `O ("tree", json_of_tree t)
    | Revision r -> `O ("revision", json_of_revision r)


  let json_to_dst ~minify dst (json:json) =
    let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
    let rec value v k e = match v with
      | `A vs -> arr vs k e
      | `O ms -> obj ms k e
      | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
    and arr vs k e = enc e `As; arr_vs vs k e
    and arr_vs vs k e = match vs with
      | v :: vs' -> value v (arr_vs vs' k) e
      | [] -> enc e `Ae; k e
    and obj ms k e = enc e `Os; obj_ms ms k e
    and obj_ms ms k e = match ms with
      | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
      | [] -> enc e `Oe; k e
    in
    let e = Jsonm.encoder ~minify dst in
    let finish e = ignore (Jsonm.encode e `End) in
    match json with
    | `A _ | `O _ as json -> value json finish e
    | _ -> invalid_arg "invalid json text"

  let string_of_json fn (json:json) = match json with
    | `String k -> Some (fn k)
    | _         -> None

  let key_of_json = string_of_json (fun x -> K x)
  let blob_of_json = string_of_json (fun x -> B x)

end


module Remote: Database.REMOTE with module T = Types = struct

  module T = Types
  open T

  let watch _ = failwith "TODO"
  let push _ = failwith "TODO"
  let pull _ = failwith "TODO"
  let discover _ = failwith "TODO"

end
