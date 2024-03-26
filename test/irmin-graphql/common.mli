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

(** Default Irmin GraphQL store *)
module Store :
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = string
     and type Schema.Metadata.t = unit
     and type Schema.Branch.t = string

type server = {
  event_loop : 'a. 'a Lwt.t;
      (** The server runtime. Cancelling this thread terminates the server. *)
  store : Store.t;  (** The store used by the server *)
}

val spawn_graphql_server : sw:Eio.Switch.t -> server
(** Initialise a GraphQL server. At most one server may be running concurrently. *)

type param
(** Parameter to GraphQL function *)

val var : string -> param
(** Variable parameter *)

val string : string -> param
(** String parameter, a string with quotation marks added *)

val raw : string -> param
(** Raw parameter, will be sent without any modification *)

val int : int -> param
(** Int parameter *)

val float : float -> param
(** Float parameter *)

type query
(** GraphQL query

    All queries will begin with either [query] or [mutation], and contain a
    combination of [list], [func] and [field].

    For example, the following query returns the latest commit hash for the
    [main] branch:

    {[
      query (func "main" (field "hash"))
    ]}

    To avoid nesting parenthesis, you can use the [@@] operator to chain
    expressions:

    {[
      query @@ func "main" @@ field "hash"
    ]} *)

val query : query -> query
(** Start a query

    In GraphQL: [query { ... }] *)

val mutation : query -> query
(** Start a mutation

    In GraphQL: [mutation { ... }] *)

val list : query list -> query
(** List of [field] or [func]


    In GraphQL: {[
      {
        ...
      }
    ]}
*)

val func : string -> ?params:(string * param) list -> query -> query
(** GraphQL method

    In GraphQL: {[
      func(params...) {
        ...
      }
    ]}

    Without parameters: {[
      func {
        ...
      }
    ]}
 *)

val field : string -> query
(** Named field/attribute

    In GraphQL: {[
      {
        field
      }
    ]}
 *)

val string_of_query : query -> string
(** Convert [query] to [string] *)

val send_query :
  ?vars:(string * Yojson.Safe.t) list ->
  string ->
  (string, [ `Msg of string ]) result Lwt.t
(** Send a GraphQL query string to the currently running test GraphQL instance. *)

val members : string list -> Yojson.Safe.t -> Yojson.Safe.t
(** Get key from JSON object *)

val parse_result : string list -> (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a
(** Get key from JSON object and apply conversion function *)

val exec :
  ?vars:(string * Yojson.Safe.t) list -> query -> (Yojson.Safe.t -> 'a) -> 'a
(** Send a [query] to the running GraphQL instance and parse the JSON results
    using the provided conversion function *)
