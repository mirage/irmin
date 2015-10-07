(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type MAKER =
  functor (C: Ir_contents.S) ->
  functor (R: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    Ir_bc.STORE_EXT
      with type key = C.Path.t
       and type value = C.t
       and type branch_id = R.t
       and type head = H.t

module Make
    (AO: Ir_ao.MAKER)
    (RW: Ir_rw.MAKER)
    (C: Ir_contents.S)
    (R: Ir_tag.S)
    (H: Ir_hash.S) =
struct
  module X = struct
    module Contents = Ir_contents.Make(struct
        include AO(H)(C)
        module Key = H
        module Val = C
      end)
    module Node = struct
      module Key = H
      module Val = Ir_node.Make (H)(H)(C.Path)
      module Path = C.Path
      include AO (Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Ir_commit.Make (H)(H)
      include AO (Key)(Val)
    end
    module Ref = struct
      module Key = R
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Ir_slice.Make(Contents)(Node)(Commit)
    module Sync = Ir_sync.None(H)(R)
  end
  include Ir_bc.Make_ext(X)
end
