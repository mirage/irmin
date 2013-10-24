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
  type t
  include IrminStore.M
  val snapshot: unit -> t Lwt.t
  val revert: t -> unit Lwt.t
  val watch: key -> (key * t option) Lwt_stream.t
end

module type STORE = sig
  type key
  type value
  type tag
  module Key: IrminKey.S
    with type t = key
  module Value: IrminValue.STORE
    with type key = key
     and type t = value
  module Tree: IrminTree.STORE
    with type key = key
     and type value = value
  module Revision: IrminRevision.STORE
    with type key = key
     and type tree = Tree.t
  module Tag: IrminTag.STORE
    with type t = tag
     and type key = key
  module type S = S with type t := key
                     and type key := Tree.path
                     and type value := value
  include S
  val create: Tag.t -> (module S)
end

module Make
    (Key: IrminKey.S)
    (Value: IrminValue.S)
    (Tag: IrminTag.S)
    (SValue: IrminStore.IRAW with type key = Key.t)
    (STree: IrminStore.IRAW with type key = Key.t)
    (SRevision: IrminStore.IRAW with type key = Key.t)
    (STag: IrminStore.MRAW with type value = Key.t) =
struct

  open Lwt

  module Key = Key
  module Value = IrminValue.Make(SValue)(Key)(Value)
  module Tree = IrminTree.Make(STree)(Key)(Value)
  module Revision = IrminRevision.Make(SRevision)(Key)(Tree)
  module Tag = IrminTag.Make(STag)(Tag)(Key)

  type key = Key.t
  type value = Value.t
  type tree = Tree.t
  type revision = Revision.t
  type path = Tree.path
  type tag = Tag.t
  module type S = S with type t := key
                     and type key := Tree.path
                     and type value := value

  module Make (T: sig val tag: Tag.t end) = struct

    type t = revision

    let init () =
      Value.init () >>= fun () ->
      Tree.init () >>= fun () ->
      Revision.init () >>= fun () ->
      Tag.init ()

    let revision () =
      Tag.read_exn T.tag >>= Revision.read_exn

    let tree rev =
      match Revision.tree rev with
      | None   -> return Tree.empty
      | Some t -> t

    let set path value =
      revision () >>= fun revision ->
      tree revision >>= fun tree ->
      Tree.add tree path value >>= fun tree ->
      Revision.create ~tree [revision] >>= fun key ->
      Tag.set T.tag key

    let remove _ = failwith "TODO"

    let read _ = failwith "TODO"

    let read_exn _ = failwith "TODO"

    let mem _ = failwith "TODO"

    let list _ = failwith "TODO"

    let snapshot _ = failwith "TODO"

    let revert _ = failwith "TODO"

    let watch _ = failwith "TODO"

  end

  let create tag =
    let module S = Make (struct let tag = tag end) in
    (module S: S)

  include Make (struct let tag = Tag.head end)

end

module Simple
    (I: IrminStore.IRAW with type key = IrminKey.SHA1.t)
    (M: IrminStore.MRAW with type value = IrminKey.SHA1.t) =
struct

  include Make(IrminKey.SHA1)(IrminValue.Simple)(IrminTag.Simple)(I)(I)(I)(M)

end
