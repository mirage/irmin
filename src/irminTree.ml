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
  type tree
  include IrminStore.S
  val create: t -> ?value:key -> (string * key) list -> tree
  val value: tree ->  (key * value Lwt.t) option
  val children: tree -> (string * key * tree Lwt.t) list
  val subtree: tree -> string list -> tree option Lwt.t
  val add: tree -> string list -> value -> tree Lwt.t
  val find: tree -> string list -> value option Lwt.t
  val remove: tree -> string list -> tree Lwt.t
  val mem: tree -> string list -> bool Lwt.t
  val iter: (string list -> value -> unit Lwt.t) -> tree -> unit Lwt.t
  val iter_all: (string list -> value option -> unit Lwt.t) -> tree -> unit Lwt.t
end

module Make
    (S: IrminBase.RAW)
    (K: IrminKey.S)
    (V: IrminValue.Store) =
struct

  type t = {
    v: V.t;
    t: S.t;
  }

  type key = K.key

  type node = {
    value   : key option;
    children: (label * key) list;
  }

  let create ?value children =
    { value; children }

  let value t = t.value

  let children t = t.children

  let child t label =
    try Some (List.assoc label t.children)
    with Not_found -> None

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
          K.of_string l :: k :: acc
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

module Simple = Make(IrminKey.SHA1)
