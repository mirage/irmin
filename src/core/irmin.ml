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
  }
  include IrminStore.S with type t := t
                        and type key := Tree.path
                        and type value := value
                        and type revision := key
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
  }

  let create () =
    Revision.create () >>= fun revision ->
    Tag.create () >>= fun tag ->
    let tree = revision.IrminRevision.t in
    let value = revision.IrminRevision.t.IrminTree.v in
    return { value; tree; revision; tag }

  let revision t =
    Tag.read_exn t.tag Tag.head >>=
    Revision.read_exn t.revision

  let tree t rev =
    match Revision.tree t.revision rev with
    | None      -> return Tree.empty
    | Some tree -> tree

  let update t path value =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    Tree.update t.tree tree path value >>= fun tree ->
    Revision.revision t.revision ~tree [revision] >>= fun key ->
    Tag.update t.tag Tag.head key

  let remove _ = failwith "TODO"

  let read _ = failwith "TODO"

  let read_exn _ = failwith "TODO"

  let mem _ = failwith "TODO"

  let list _ = failwith "TODO"

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
