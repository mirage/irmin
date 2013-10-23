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

module Key = IrminKey.SHA1
module Value = IrminValue.Simple
module Tag = IrminTag.Simple

module type S = sig
  type t
  type path
  include IrminStore.M with type key := path
  val snapshot: unit -> t Lwt.t
  val revert: t -> unit Lwt.t
  val watch: path -> (path * t option) Lwt_stream.t
end


module type STORE = sig
  module Value: IrminValue.STORE
    with type key = Key.t
     and type t = Value.t
  module Tree: IrminTree.STORE
    with type key = Key.t
     and type value = Value.t
  module Revision: IrminRevision.STORE
    with type key = Key.t
     and type tree = Tree.t
  module Tag: IrminTag.STORE
    with type t = Tag.t
     and type key = Key.t
(*  include S with type t = Revision.t
             and type path = Tree.path
             and type value = Value.t *)
  val master: unit -> (module S)
  val create: Tag.t -> (module S)
end

module Make
    (SValue: IrminStore.IRAW with type key = Key.t)
    (STree: IrminStore.IRAW with type key = Key.t)
    (SRevision: IrminStore.IRAW with type key = Key.t)
    (STag: IrminStore.MRAW with type value = Key.t) =
struct

  open Lwt

  module Value = IrminValue.Make(SValue)(Key)(Value)
  module Tree = IrminTree.Make(STree)(Key)(Value)
  module Revision = IrminRevision.Make(SRevision)(Key)(Tree)
  module Tag = IrminTag.Make(STag)(Tag)(Key)

  type t = Revision.t
  type path = Tree.path
  type value = Value.t

  module S (T: sig val tag: Tag.t end) = struct

    let init () =
    Value.init () >>= fun () ->
    Tree.init () >>= fun () ->
    Revision.init () >>= fun () ->
    Tag.init ()

  let set t path value =
    match Revision.tree t with
    | None      -> failwith "TODO"
    | Some tree ->
      tree >>= fun tree ->
      Tree.add tree path value >>= fun tree ->
      Revision.with_tree t (Some tree)

  let remove _ = failwith "TODO"

  let read _ = failwith "TODO"

  let read_exn _ = failwith "TODO"

  let mem _ = failwith "TODO"

  let list _ = failwith "TODO"

  let snapshot _ = failwith "TODO"

  let revert _ = failwith "TODO"
  end

  let master _ = failwith "TODO"

  let create _ = failwith "TODO"

end
