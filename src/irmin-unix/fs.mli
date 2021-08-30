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

module Append_only : Irmin.Append_only.Maker
module Atomic_write : Irmin.Atomic_write.Maker

include
  Irmin.Maker
    with type 'h contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

module KV :
  Irmin.KV_maker
    with type 'h contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

(** {1 Extended Stores} *)

module Append_only_ext (C : Irmin_fs.Config) : Irmin.Append_only.Maker
module Atomic_write_ext (C : Irmin_fs.Config) : Irmin.Atomic_write.Maker
module Maker_ext (Obj : Irmin_fs.Config) (Ref : Irmin_fs.Config) : Irmin.Maker
