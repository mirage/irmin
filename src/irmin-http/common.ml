(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let irmin_version = "X-IrminVersion"

type status = { status : string } [@@deriving irmin]

type 'a set = { test : 'a option; set : 'a option; v : 'a option }
[@@deriving irmin]

type ('a, 'b) event = { branch : 'a; diff : 'b Irmin.Diff.t } [@@deriving irmin]
type ('a, 'b) init = { branch : 'a; commit : 'b } [@@deriving irmin]
type 'a merge = { old : 'a; left : 'a; right : 'a } [@@deriving irmin]

type 'a merge_result = ('a option, Irmin.Merge.conflict) result
[@@deriving irmin]
