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

include Commit_intf

module Make (St : Irmin.Generic_key.S) (T : Tree.S) = struct
  type tree = T.t
  type key = St.commit_key

  let tree_t = T.t

  type hash = St.Hash.t

  let hash_t = St.Hash.t
  let key_t = St.commit_key_t

  module Info = St.Info

  type t = { info : Info.t; parents : key list; key : key; tree : T.t }
  [@@deriving irmin]

  let info { info; _ } = info
  let key { key; _ } = key
  let parents { parents; _ } = parents
  let tree { tree; _ } = tree
  let v ~info ~parents ~key ~tree = { info; parents; key; tree }
end
