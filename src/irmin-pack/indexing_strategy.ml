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

type t = value_length:int -> Pack_value.Kind.t -> bool

let always ~value_length:_ _ = true

let minimal : t =
 fun ~value_length:_ -> function
  | Commit_v2 ->
      (* Commits must be indexed as the branch store contains only their
         hashes. All {i internal} references to V1 commits are via offset
         (from other V1 commit objects). *)
      true
  | Inode_v2_root ->
      (* It's safe not to index V1 root inodes because they are never
         referenced by V0 commit objects (only V1 commit objects, which
         contain direct pointers rather than hashes).*)
      false
  | Inode_v2_nonroot -> false
  | Contents -> false
  | Commit_v1 | Inode_v1_unstable | Inode_v1_stable ->
      (* We never append new V0 values, so this choice is irrelevant to the
         store implementation, but we do assume that existing V0 objects are
         indexed (as they may be referenced via hash by other V0 objects), and
         this must be accounted for when reconstructing the index. *)
      true
  | Dangling_parent_commit -> assert false

let minimal_with_contents : t =
 fun ~value_length:_ -> function
  | Commit_v2 -> true
  | Inode_v2_root -> false
  | Inode_v2_nonroot -> false
  | Contents -> true
  | Commit_v1 | Inode_v1_unstable | Inode_v1_stable -> true
  | Dangling_parent_commit -> assert false

let default = always
let is_minimal x = x == minimal
