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

(** Store dumps. *)

open IrminCore

module type S = sig

  (** Store with import/export capabilities. *)

  type t
  (** Local database handlers. *)

  val output_file:
    t -> ?depth:int -> ?call_dot:bool -> ?full:bool -> string -> unit Lwt.t
  (** [output_file name] creates a Graphviz file [name.dot]
      representing the store state. If [call_dot] is set (it is not by
      default), call the `dot` binary to generate a the corresponding
      `.png` file. If [full] is set (it is not by default), the full
      graph, included the filesystem and blobs is exported, otherwise
      it is the history graph only.  *)

  val output_buffer: t -> ?depth:int -> ?full:bool -> Buffer.t -> unit Lwt.t
  (** Same as [output_file] but writes in a buffer. *)

end

module Make (S: IrminBranch.STORE): S with type t = S.t
(** Extend a branch consistent store with import/export
    capabilities. *)
