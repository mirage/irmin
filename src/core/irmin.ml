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

module type S = sig
  type key
  type value
  type tag
  module Key: IrminKey.S
    with type t = key
  module Value: IrminValue.STORE
    with type key = key
     and type value = value
  module Tree: IrminTree.STORE
    with type key = key
     and type value = value
  module Revision: IrminRevision.STORE
    with type key = key
     and type tree = Tree.tree
  module Tag: IrminTag.STORE
    with type tag = tag
     and type key = key
  type t = {
    value   : Value.t;
    tree    : Tree.t;
    revision: Revision.t;
    tag     : Tag.t;
    branch  : Tag.tag;
  }
  include IrminStore.S with type t := t
                        and type key := Tree.path
                        and type value := value
                        and type revision := key

  val tag: Tag.tag -> t Lwt.t
end

module Make
    (Key: IrminKey.S)
    (Value: IrminValue.S)
    (Tag: IrminTag.S)
    (SValue: IrminStore.A_RAW with type key = Key.t)
    (STree: IrminStore.A_RAW with type key = Key.t)
    (SRevision: IrminStore.A_RAW with type key = Key.t)
    (STag: IrminStore.M_RAW with type value = Key.t) =
struct

  open Lwt

  module Key = Key
  module Value = IrminValue.Make(SValue)(Key)(Value)
  module Tree = IrminTree.Make(STree)(Key)(Value)
  module Revision = IrminRevision.Make(SRevision)(Key)(Tree)
  module Tag = IrminTag.Make(STag)(Tag)(Key)

  type key = Key.t
  type value = Value.value
  type tree = Tree.tree
  type revision = Revision.revision
  type path = Tree.path
  type tag = Tag.tag

  type t = {
    value: Value.t;
    tree: Tree.t;
    revision: Revision.t;
    tag: Tag.t;
    branch: Tag.tag;
  }

  let create () =
    Revision.create () >>= fun revision ->
    Tag.create () >>= fun tag ->
    let tree = revision.IrminRevision.t in
    let value = revision.IrminRevision.t.IrminTree.v in
    let branch = Tag.master in
    return { value; tree; revision; tag; branch }

  let init t =
    Revision.init t.revision >>= fun () ->
    Tag.init t.tag

  let tag branch =
    create () >>= fun t ->
    return { t with branch }

  let revision t =
    Tag.read t.tag t.branch >>= function
    | None   -> return_none
    | Some k -> Revision.read_exn t.revision k >>= function r -> return (Some r)

  let tree t = function
    | None     -> return Tree.empty
    | Some rev ->
      match Revision.tree t.revision rev with
      | None      -> return Tree.empty
      | Some tree -> tree

  let parents_of_revision = function
    | None   -> []
    | Some r -> [r]

  let update_tree t path fn =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    fn tree >>= fun tree ->
    Revision.revision t.revision ~tree (parents_of_revision revision) >>= fun key ->
    Tag.update t.tag t.branch key

  let update t path value =
    update_tree t path (fun tree ->
        Tree.update t.tree tree path value
      )

  let remove t path =
    update_tree t path (fun tree ->
        Tree.remove t.tree tree path
      )

  let read_tree fn t path =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    fn t.tree tree path

  let read =
    read_tree Tree.find

  let read_exn =
    read_tree Tree.find_exn

  let mem =
    read_tree Tree.mem

  let list _ =
    failwith "TODO"

  let snapshot _ = failwith "TODO"

  let revert _ = failwith "TODO"

  let watch _ = failwith "TODO"

end

module Simple
    (A: IrminStore.A_RAW with type key = IrminKey.SHA1.t)
    (M: IrminStore.M_RAW with type value = IrminKey.SHA1.t) =
struct

  include Make(IrminKey.SHA1)(IrminValue.Simple)(IrminTag.Simple)(A)(A)(A)(M)

end
