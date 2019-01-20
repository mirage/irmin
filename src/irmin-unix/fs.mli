(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Append_only: Irmin.APPEND_ONLY_STORE_MAKER
module Atomic_write: Irmin.ATOMIC_WRITE_STORE_MAKER
module Make: Irmin.S_MAKER
module KV: Irmin.KV_MAKER

module Append_only_ext (C: Irmin_fs.Config): Irmin.APPEND_ONLY_STORE_MAKER
module Atomic_write_ext (C: Irmin_fs.Config): Irmin.ATOMIC_WRITE_STORE_MAKER
module Make_ext (Obj: Irmin_fs.Config) (Ref: Irmin_fs.Config): Irmin.S_MAKER
