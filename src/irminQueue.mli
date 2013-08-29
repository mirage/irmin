(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Distributed queues *)

open IrminTypes
open IrminLwt

(** - [front] tags values which are the queue heads. This tag is
    updated on each [Queue.take] by value consummers.

    - [back] tags values which are queue tails. This tag is updated on
      each [Queue.add] by value producers.

    An example of an distributed queue seen as a DAG:

        F -> .....>......-> B
        |     /       \     |
        |    F         B    |
        |    |         |    |
      front  |        back  |
           front           back

    There are few invariants on such graph:

    - the graph is acyclic and can be seen as a partially ordered multiset;

    - [front] values are all incomparable from each-other;

    - [back] values are all incomparable from each-other;

    - [front] values are predecessors of [back] values.
*)
type t

(** Create a queue abstract object. *)
val create: ?front:Tag.t -> ?back:Tag.t -> [`Dir of string] -> t

(** Default front tag. *)
val default_front: Tag.t

(** Default back tag. *)
val default_back: Tag.t

(** Init a queue. *)
val init: t -> unit Lwt.t

(** A queue is empty if there is no more front value. *)
val is_empty: t -> bool Lwt.t

(** [add t b] adds the value [b] at the back of the queue. Update
    [back] tags to reflect the new state.

    Before:

        F -> ............-> B
        |             \     |
        |              B    |
        |              |    |
      front           back  |
                          back

    After:

        F -> ............-> B -> b
        |             \        / |
        |              B-----/   |
        |                        |
      front                     back
*)
val add: t -> Value.t -> unit Lwt.t

(** [take t] returns a front value, and update the front tags
    accordingly.

    1. A front tag is removed if all the selected value successors are
    greater than another front value.

    Before:

        F ---> F ->......-> B
        |     /             |
        |    F*             |
        |    |              |
      front  |             back
           front

    After:

        F ---> F ->......-> B
        |                   |
        |                   |
        |                   |
      front                back

    And F* is return.

    2. Front tags are moved to the event successors wich are not
    greater than any existing front event.

    Before:

        F* -> F ->......-> B
        |                  |
        |                  |
        |                  |
      front               back

    After:

        F* -> F ->......-> B
              |            |
              |            |
              |            |
            front         back

    and F* is return.


*)
val take: t -> Value.t Lwt.t

(** [peek t] choose an arbitraty front value. *)
val peek: t -> Value.t Lwt.t

(** Return all the values between front and back values, in the
    topological order. *)
val to_list: t -> Value.t list Lwt.t

(* TODO *)

val watch: t -> unit

val pull: t -> unit

val push: t -> unit

val clone: t -> unit
