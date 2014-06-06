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

(** Irmin store resolver. *)

val init_hook: unit -> unit
(** Initialisation hooks. *)

val store: (module Irmin.S) Cmdliner.Term.t
(** Parse a store on the command-line. *)

val store_of_string: string -> (module Irmin.S) option
(** Parse a Irmin URI. *)

val store_of_string_exn: string -> (module Irmin.S)
(** Same as [store_of_string] but raises [Not_found] if it is not a
    valid URI. *)

val store_of_env_var: unit -> (module Irmin.S) option
(** Read the "IRMIN" env variable. *)

val remote: IrminSync.remote Cmdliner.Term.t
(** Parse a remote store. *)
