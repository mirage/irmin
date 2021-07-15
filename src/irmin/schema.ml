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
  module Contents_key : Key.S with type hash = Hash.t
  module Node_key : Key.Hash_like with type hash = Hash.t
  module Commit_key : Key.Hash_like with type hash = Hash.t
  module Branch : Branch.S
  module Info : Info.S

  module Commit :
    Commit.S
      with module Info := Info
       and type node_key = Node_key.t
       and type commit_key = Commit_key.t

  module Metadata : Metadata.S
  module Path : Path.S
  module Contents : Contents.S

  module Node :
    Node.S
      with type metadata = Metadata.t
       and type step = Path.step
       and type node_key = Node_key.t
       and type contents_key = Contents_key.t
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
  module Key = Key.Of_hash (Hash)
  module Node_key = Key
  module Contents_key = Key
  module Commit_key = Key
  module Commit = Commit.Make (Hash) (Node_key) (Commit_key)
  module Path = Path.String_list
  module Metadata = Metadata.None

  module Node = Node.Make (struct
    module Hash = Hash
    module Path = Path
    module Node_key = Node_key
    module Contents_key = Contents_key
    module Metadata = Metadata
  end)

  module Contents = C
end
