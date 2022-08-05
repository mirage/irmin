(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
  type tree
  type key

  val tree_t : tree Irmin.Type.t
  val hash_t : hash Irmin.Type.t
  val key_t : key Irmin.Type.t

  module Info : Irmin.Info.S

  type t = { info : Info.t; parents : key list; key : key; tree : tree }
  [@@deriving irmin]

  val info : t -> Info.t
  val key : t -> key
  val parents : t -> key list
  val tree : t -> tree
  val v : info:Info.t -> parents:key list -> key:key -> tree:tree -> t
end

module type Commit = sig
  module type S = S

  module Make (S : Irmin.Generic_key.S) (T : Tree.S) :
    S
      with type hash = S.Hash.t
       and type tree = T.t
       and type key = S.commit_key
       and module Info = S.Info
end
