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

(** Default Irmin GraphQL store *)
module Store :
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Metadata.t = unit

type server = {
  event_loop : 'a. 'a Lwt.t;
      (** The server runtime. Cancelling this thread terminates the server. *)
  set_tree : Store.Tree.concrete -> unit Lwt.t;
      (** Set the state of the [main] branch in the underlying store. *)
}

val spawn_graphql_server : unit -> server Lwt.t
(** Initialise a GraphQL server. At most one server may be running concurrently. *)

val send_query : string -> (string, [ `Msg of string ]) result Lwt.t
(** Send a GraphQL query string to the currently running test GraphQL instance. *)
