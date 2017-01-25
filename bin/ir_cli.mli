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

(** CLI commands. *)

type command = unit Cmdliner.Term.t * Cmdliner.Term.info
(** [Cmdliner] commands. *)

val default: command
(** The default command: show a summary of the commands. *)

val commands: command list
(** List of available sub-commands. *)

val run: default:command -> command list -> unit
(** Create a command-line tool with the given subcommands. *)

(** {2 Command-builder helper} *)

type sub = {
  name: string;
  doc : string;
  man : Cmdliner.Manpage.block list;
  term: unit Cmdliner.Term.t;
}
(** Subcommand. *)

val create_command: sub -> command
(** Build a subcommand. *)
