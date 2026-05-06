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

(** Store dumps. *)

module type S = sig
  (** {1 Dot Export} *)

  type db

  val output_buffer :
    db ->
    ?html:bool ->
    ?depth:int ->
    ?full:bool ->
    date:(int64 -> string) ->
    Buffer.t ->
    unit Lwt.t
  (** [output_buffer t ?html ?depth ?full buf] outputs the Graphviz
      representation of [t] in the buffer [buf].

      [html] (default is false) enables HTML labels.

      [depth] is used to limit the depth of the commit history. [None] here
      means no limitation.

      If [full] is set (default is not) the full graph, including the commits,
      nodes and contents, is exported, otherwise it is the commit history graph
      only. *)
end

module Make (S : Store.Generic_key.S) : S with type db = S.t
