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
end

module Make
    (SValue: IrminStore.IRAW with type key = Key.t)
    (STree: IrminStore.IRAW with type key = Key.t)
    (SRevision: IrminStore.IRAW with type key = Key.t)
    (STag: IrminStore.MRAW with type key = Key.t) =
struct

  module Value = IrminValue.Make(SValue)(Key)(Value)
  module Tree = IrminTree.Make(STree)(Key)(Value)
  module Revision = IrminRevision.Make(SRevision)(Key)(Tree)
  module Tag = IrminTag.Make(STag)(Tag)(Key)

end
