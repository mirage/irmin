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
  type hash
  type branch
  type info
  type commit
  type metadata
  type step
  type path
  type node
  type contents

  module Hash : Hash.S with type t = hash
  module Branch : Branch.S with type t = branch
  module Info : Info.S with type t = info

  module Commit :
    Commit.S with type t = commit and type hash = Hash.t and module Info := Info

  module Metadata : Metadata.S with type t = metadata
  module Path : Path.S with type step = step and type t = path

  module Node :
    Node.S
      with type t = node
       and type metadata = Metadata.t
       and type hash = Hash.t
       and type step = Path.step

  module Contents : Contents.S with type t = contents
end

module type KV =
  S
    with type hash = Hash.BLAKE2B.t
     and type branch = string
     and type info = Info.default
     and type metadata = unit
     and type step = string
     and type path = string list

module type Sigs = sig
  module type S = S
  module type KV = KV

  module KV (C : Contents.S) : KV with type contents = C.t

  module Abstract (S : S) :
    S
      with type hash = S.hash
       and type branch = S.branch
       and type info = S.info
       and type commit = S.commit
       and type metadata = S.metadata
       and type step = S.step
       and type path = S.path
       and type contents = S.contents
end
