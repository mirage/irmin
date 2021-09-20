(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  module Branch : Branch.S

  include
    Irmin.Schema.S
      with module Metadata = Metadata
       and module Branch := Branch
       and type Info.t = Irmin.Info.default
       and type Path.step = string
       and type Path.t = string list

  module Node :
    Irmin.Node.S
      with type metadata = Metadata.t
       and type step = Path.step
       and type hash = Hash.t

  module Commit : Irmin.Commit.S with module Info := Info and type hash = Hash.t
end

module Make (G : Git.S) (V : Irmin.Contents.S) (B : Branch.S) :
  S
    with type Hash.t = G.hash
     and module Contents = V
     and module Branch = B
     and type Node.t = G.Value.Tree.t
     and type Commit.t = G.Value.Commit.t = struct
  module Metadata = Metadata
  module Contents = V
  module Path = Irmin.Path.String_list
  module Branch = B
  module Hash = Irmin.Hash.Make (G.Hash)
  module Node = Node.Make (G) (Path)
  module Commit = Commit.Make (G)
  module Info = Irmin.Info.Default
end
