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
  module Metadata : Metadata.S
  module Path : Path.S
  module Contents : Contents.S
end

module type Extended = sig
  include S

  module Commit
      (Node_key : Key.S with type hash = Hash.t)
      (Commit_key : Key.S with type hash = Hash.t) :
    Commit.S
      with module Info := Info
       and type node_key = Node_key.t
       and type commit_key = Commit_key.t

  (* XXX
     I'm not sure what to do about this.

     - [Irmin_pack] wants to specify a non hash-like key implementation;
     - the user wants to specify the node implementation (e.g. for V1 serialisation);
  *)
  module Node
      (Contents_key : Key.S with type hash = Hash.t)
      (Node_key : Key.S with type hash = Hash.t) :
    Node.S
      with type metadata = Metadata.t
       and type step = Path.step
       and type contents_key = Contents_key.t
       and type node_key = Node_key.t
end

open struct
  module Extended_is_a_schema (X : Extended) : S = X
end

type default_hash = Hash.BLAKE2B.t

module type KV =
  Extended
    with type Hash.t = default_hash
     and type Branch.t = string
     and type Info.t = Info.default
     and type Metadata.t = unit
     and type Path.step = string
     and type Path.t = string list

module KV (C : Contents.S) : KV with module Contents = C = struct
  module Contents = C
  module Hash = Hash.BLAKE2B
  module Info = Info.Default
  module Branch = Branch.String
  module Path = Path.String_list
  module Metadata = Metadata.None
  module Commit = Commit.Make (Hash)
  module Node = Node.Make_generic_key (Hash) (Path) (Metadata)
end
