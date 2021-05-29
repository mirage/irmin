(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2020-2021 Craig Ferguson <me@craigfe.io>
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
  module Hash : Hash.S
  module Branch : Branch.S
  module Info : Info.S
  module Commit : Commit.S with type hash = Hash.t and module Info := Info
  module Metadata : Metadata.S
  module Path : Path.S

  module Node :
    Node.S
      with type metadata = Metadata.t
       and type hash = Hash.t
       and type step = Path.step

  module Contents : Contents.S
end

module type KV =
  S
    with type Hash.t = Hash.BLAKE2B.t
     and type Branch.t = string
     and type Info.t = Info.default
     and type Metadata.t = unit
     and type Path.step = string
     and type Path.t = string list

module KV (C : Contents.S) : KV with module Contents = C = struct
  module Hash = Hash.BLAKE2B
  module Info = Info.Default
  module Branch = Branch.String
  module Commit = Commit.Make (Hash)
  module Path = Path.String_list
  module Metadata = Metadata.None
  module Node = Node.Make (Hash) (Path) (Metadata)
  module Contents = C
end

module Abstract (S : S) :
  S
    with type Hash.t = S.Hash.t
     and type Branch.t = S.Branch.t
     and type Info.t = S.Info.t
     and type Commit.t = S.Commit.t
     and type Metadata.t = S.Metadata.t
     and type Path.step = S.Path.step
     and type Path.t = S.Path.t
     and type Contents.t = S.Contents.t =
  S
