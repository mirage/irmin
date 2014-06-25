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

open Core_kernel.Std

module type S = sig

  (** Store with import/export capabilities. *)

  type t
  (** Local database handlers. *)

  val output_file: t -> ?depth:int -> ?call_dot:bool -> ?commits_only:bool -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

  val output_buffer: t -> ?depth:int -> ?commits_only:bool -> Buffer.t -> unit Lwt.t
  (** Same as [output_file] but writes in a buffer. *)

end

module Make (S: IrminBranch.STORE): S with type t = S.t
(** Extend a branch consistent store with import/export
    capabilities. *)
