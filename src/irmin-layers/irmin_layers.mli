(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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

module type S = Irmin_layers_intf.S

module Make_ext
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
    (Metadata : Irmin.Metadata.S)
    (Contents : Irmin.Contents.S)
    (Path : Irmin.Path.S)
    (Branch : Irmin.Branch.S)
    (Hash : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S
              with type metadata = Metadata.t
               and type hash = Hash.t
               and type step = Path.step)
    (Commit : Irmin.Private.Commit.S with type hash = Hash.t) :
  S
    with type key = Path.t
     and type contents = Contents.t
     and type branch = Branch.t
     and type hash = Hash.t
     and type step = Path.step
     and type metadata = Metadata.t
     and type Key.step = Path.step

module type S_MAKER = Irmin_layers_intf.S_MAKER

module Make
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER) : S_MAKER

module Stats = Stats
