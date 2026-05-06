(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

(** Tree module types only.

    The implementation functor [Tree.Make (B : Backend.S)] (~2800 lines,
    verbatim from main) used to live here. It was the in-memory tree machinery
    consumed exclusively by [Store.Make]. Both have been removed from
    [irmin-lwt]: backends now go through [Wrap_store.Make], which delegates the
    tree implementation to Irmin 4's [Tree] module via [Inner.Tree]. The
    Lwt-typed [Tree.S] surface is still used as a public module type
    ([module type Tree = Tree.S] in [irmin_lwt.ml]) so this file re-exports the
    signatures from [Tree_intf]. *)

include Tree_intf
