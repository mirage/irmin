(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
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

open Brands
open Subtyping

(** This library implements so-called 'optics': first-class representations of
    {i components} of composite data types. *)

type ('s, 't, 'a, 'b, 'm, 'feat) t
(** The type of optics. The type parameters are as follows:

    - [s]: the source/input type
    - [t]: the target/output type (following a transformation of focused [a]'s
      to [b]'s)
    - [a]: the type of focused elements
    - [b]: the type of {i transformed} focused elements
    - [m]: the brand of monadic effects associated with access via the optic
    - [feat]: is the set of operations supported by the optic *)

type ('s, 't, 'a, 'b, 'm, 'feat) optic = ('s, 't, 'a, 'b, 'm, 'feat) t
(** Alias of {!t}. *)

(** {3 Composing optics} *)

val ( >> ) :
  ('s, 't, 'mid_in, 'mid_out, 'm, 'feat) t ->
  ('mid_in, 'mid_out, 'a, 'b, 'm, 'feat) t ->
  ('s, 't, 'a, 'b, 'm, 'feat) t
(** [compose a b] is the composition of optics [a] and [b]. The non-operator
    form is {!compose}. *)

val compose :
  ('s, 't, 'mid_in, 'mid_out, 'm, 'feat) t ->
  ('mid_in, 'mid_out, 'a, 'b, 'm, 'feat) t ->
  ('s, 't, 'a, 'b, 'm, 'feat) t
(** A prefix alias of {!( >> )}. *)

(** {3 Using optics} *)

val get : ('s, 't, 'a, _, 'm, [> get | exactly_one ]) t -> 's -> ('a, 'm) app
(** [get t s] returns the focus of [t] in [s]. *)

val get_option :
  ('s, _, 'a, _, 'm, [> get | at_most_one ]) t -> 's -> ('a, 'm) app
(** [get_option t s] returns the focus of [t] in [s] (if it has one). *)

val map : ('s, 't, 'a, 'b, 'm, [> map ]) t -> 's -> ('a -> 'b) -> ('t, 'm) app
(** [map t s f] applies [f] to the focus of [t] in [s] and returns the result. *)

val update : ('s, 't, _, 'b, 'm, [> update ]) t -> 's -> 'b -> ('t, 'm) app
(** [update t s b] performs a functional update of setting [b] at the focus of
    [t] in [s] (constructing the focus point if it does not exist).

    {b N.B.} this is not the same as [{!map} t s (fun _ -> b)], as the focus
    point may be {!construct}ed if it does not yet exist in [s]. *)

val construct : (_, 't, _, 'b, 'm, [> construct ]) t -> 'b -> ('t, 'm) app
(** [construct t b] builds a new composite type [t] by supplying the focused
    value [b]. *)

module Optic_list : sig
  type ('list, 'm, 'k) t =
    | ( :: ) :
        ('s, 't, 'a, 'b, 'm, 'k) optic * ('list, 'm, 'k) t
        -> (('s, 't, 'a, 'b, 'm, 'k) optic * 'list, 'm, 'k) t
    | [] : (unit, 'm, 'k) t
end

module Subtyping : sig
  include module type of Subtyping
end

(** {2 Type aliases} *)

include module type of Optic_types.Aliases
(** @inline *)
