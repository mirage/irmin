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

type ('a, 'b) revision = {
  tree   : 'a option;
  parents: 'b list;
}

module type STORE = sig
  type key
  type t = (key, key) revision
  include IrminBase.S with type t := t
  type tree
  module Graph: IrminGraph.S with type Vertex.t = t
  include IrminStore.A with type key := key
                        and type value := t
  val create: ?tree:tree -> t list -> key Lwt.t
  val tree: t -> tree Lwt.t option
  val parents: t -> t Lwt.t list
  val cut: ?roots:key list -> key list -> Graph.t Lwt.t
end

module Make
    (S: IrminStore.IRAW)
    (K: IrminKey.S with type t = S.key)
    (T: IrminTree.STORE with type key = S.key) =
struct

  open Lwt

  module Revision = struct

    type t = (K.t, K.t) revision

    module XTree = struct
      include IrminBase.Option(K)
      let name = "tree"
    end
    module XParents = struct
      include IrminBase.List(K)
      let name = "parents"
    end
    module XRevision = struct
      include IrminBase.Pair(XTree)(XParents)
      let name = "revision"
    end

    let name = XRevision.name

    let set buf t =
      XRevision.set buf (t.tree, t.parents)

    let get buf =
      let tree, parents = XRevision.get buf in
      { tree; parents }

    let sizeof t =
      XRevision.sizeof (t.tree, t.parents)

    let to_json t =
      XRevision.to_json (t.tree, t.parents)

    let of_json j =
      let tree, parents = XRevision.of_json j in
      { tree; parents }

    let dump t =
      XRevision.dump (t.tree, t.parents)

    let pretty t =
      XRevision.pretty (t.tree, t.parents)

    let hash t =
      XRevision.hash (t.tree, t.parents)

    let compare t1 t2 =
      XRevision.compare (t1.tree, t1.parents) (t2.tree, t2.parents)

    let equal t1 t2 =
      compare t1 t2 = 0

  end

  module Store = IrminStore.MakeI(S)(K)(Revision)

  module Graph = IrminGraph.Make(Revision)

  include Revision

  include (Store: module type of Store with type value := t)

  type tree = T.t

  let tree t =
    match t.tree with
    | None   -> None
    | Some k -> Some (T.read_exn k)

  let create ?tree parents =
    begin match tree with
      | None   -> return_none
      | Some t -> T.write t >>= fun t -> return (Some t)
    end
    >>= fun tree ->
    Lwt_list.map_p write parents
    >>= fun parents ->
    write { tree; parents }

  let parents t =
    List.map (fun k ->
        S.read_exn k >>= fun b ->
        return (Revision.get b)
      ) t.parents

  let cut ?roots keys =
    Lwt_list.map_p read_exn keys >>= fun keys ->
    let pred t =
      Lwt_list.map_p
        (fun r -> r)
        (parents t) in
    let (keys: t list) = keys in
    let aux roots =
      let (roots: t list option) = roots in
      Graph.closure ?roots pred keys in
    match roots with
    | None    -> aux None
    | Some ks -> Lwt_list.map_p read_exn ks >>= fun ts -> aux (Some ts)

end
