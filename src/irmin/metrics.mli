(*
 * Copyright (c) 2022 Etienne Marais <etienne@maiste.fr>
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

(** [Metrics] defines primitives to handle metrics inside of Irmin. Its purpose
    is to decouple the metrics type definition from the data manipulation.

    A {!t} can be modified in different ways, depending on the {!update_mode}. *)

type origin = ..
(** An extensible type to get the location of the definition. *)

type 'a t
(** {!t} is the object that describes how a {!t} is gathered and store. The ['a]
    parameter represents the type of the internal data. *)

val state : 'a t -> 'a
(** The internal state extracted from a {!t}. *)

val set_state : 'a t -> 'a -> unit
(** [set_state m v] updates the value in the {!t} object. *)

(** {!update_mode} describes how the data will be handled by the {!update}
    function.

    - Mutate: the value and the storage are not modified but the content of the
      value can be mutate.
    - Replace f: apply f to the value and updates its content.

    It gives the possibility to handle the same metric in different ways. *)
type 'a update_mode = Mutate of ('a -> unit) | Replace of ('a -> 'a)

val v : ?origin:origin -> name:string -> initial_state:'a -> 'a Repr.ty -> 'a t
(** [v ~origin ~name ~initial_state  repr ] create a new {!t}. The [origin] can
    be set to give an hint about where the data are gathered. [name] is a name
    to describe this metrics. [initial_state] is the first value to store in the
    metric object. [repr] describes the type representation to allow
    serialization. *)

val update : 'a t -> 'a update_mode -> unit
(** [update metrics mode] updates the metric by taking in consideration [mode]
    to define how it acts on [t] according to their specication. *)
