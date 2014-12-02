(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type STORE = sig
  type step
  include Ir_bc.STORE with type key = step list
  module Key: Ir_path.S with type step = step
  module Val: Ir_contents.S with type t = value
  module Private: sig
    include Ir_bc.PRIVATE
      with type Contents.value = value
       and type Node.Path.step = step
       and type Commit.key = head
       and type Tag.key = tag
    val contents_t: t -> Contents.t
    val node_t: t -> Contents.t * Node.t
    val commit_t: t -> Contents.t * Node.t * Commit.t
    val tag_t: t -> Tag.t
  end
end

module type MAKER =
  functor (P: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type step = P.step
           and type value = C.t
           and type tag = T.t
           and type head = H.t

module Make
    (AO: Ir_ao.MAKER)
    (RW: Ir_rw.MAKER)
    (P: Ir_path.S)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S) =
struct
  module X = struct
    module Contents = struct
      module Key = H
      module Val = C
      include AO (Key)(Val)
    end
    module Node = struct
      module Key = H
      module Val = Ir_node.Make (H)(H)(P)
      module Path = P
      include AO (Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Ir_commit.Make (H)(H)
      include AO (Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW (Key)(Val)
    end
  end
  include Ir_bc.Make_ext(X)
  module Private = struct
    include X
    let contents_t: t -> Contents.t = contents_t
    let node_t = node_t
    let commit_t = commit_t
    let tag_t = tag_t
  end
end
