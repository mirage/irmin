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

(** Optics in OCaml *)

(** First-class product components with monadic access. *)
module Lens (F : S.MONAD) : sig
  type (-'s, +'t, +'a, -'b) t
  (** The type of lenses, where the type parameters are as follows:

      - ['s]: the source type
      - ['t]: the output type
      - ['a]: the focus of the lens
      - ['b]: is the type such that ['b/'a]'s = 't, i.e. the result type of the
        focused transformation that produces the necessary output type 't. *)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t
  (** The type of monomorphic lenses (lenses such that the internal transform is
      type-preserving) *)

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
        (** Convenient syntax for a heterogeneous list of lenses. *)

  val v : ('s -> 'a F.t) -> ('s -> 'b -> 't F.t) -> ('s, 't, 'a, 'b) t
  (** [v f m] is the lens given by the focusing function [f] and the
      modification function [m]. *)

  val view : ('s, 't, 'a, 'b) t -> 's -> 'a F.t
  (** [view l] is the focusing function of [l]. *)

  val modify : ('s, 't, 'a, 'b) t -> ('a -> 'b) -> 's -> 't F.t
  (** [modify l f s] is the result of applying [f] to the focus of [l] in [s]. *)

  val update : ('s, 't, 'a, 'b) t -> 'b -> 's -> 't F.t
  (** [update l b s] replaces the focus of [l] in [s] with [b]. *)

  val ( >> ) : ('a, 'b, 'c, 'd) t -> ('c, 'd, 'e, 'f) t -> ('a, 'b, 'e, 'f) t
  (** [l1 >> l2] is the composition of lenses [l1] and [l2]. *)

  (** {3 Common lenses} *)

  val id : ('a, 'a) mono

  val fst : ('a1 * 'b, 'a2 * 'b, 'a1, 'a2) t

  val snd : ('a * 'b1, 'a * 'b2, 'b1, 'b2) t

  val head : ('a list, 'a) mono
end

(** First-class sum components with monadic access. *)
module Prism (F : S.MONAD) : sig
  type (-'s, +'t, +'a, -'b) t
  (** The type of prisms.

      The prism is the categorical dual of the lens (it operates on sum types
      where lenses operate on product types). As such, the access function is
      non-total *)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list

  val v : ('b -> 't F.t) -> ('s -> 'a option F.t) -> ('s, 't, 'a, 'b) t

  val ( >> ) : ('a, 'b, 'c, 'd) t -> ('c, 'd, 'e, 'f) t -> ('a, 'b, 'e, 'f) t
  (** [l1 >> l2] is the composition of prisms l1 and l2. *)

  (** {3 Common prisms} *)

  val some : ('a option, 'b option, 'a, 'b) t

  val none : ('a option, unit) mono

  val ok : (('a, 'c) result, ('b, 'c) result, 'a, 'b) t

  val error : (('a, 'b) result, ('a, 'c) result, 'b, 'c) t

  val head : ('a list, 'a) mono

  val nil : ('a list, unit) mono
end

(** Composable getter functions parameterised on applicative input and monadic
    output types. *)
module Getter (In : S.APPLICATIVE) (Out : S.MONAD) : sig
  type ('s, 'a) t

  val ( >> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  val v : ('s In.t -> 'a Out.t) -> ('s, 'a) t

  val ( ^. ) : 's In.t -> ('s, 'a) t -> 'a Out.t
end

(** Provides functions for _optional_ getting and setting, and is a common
    sub-type of lenses and prisms (allowing the two to be composed after an
    explicit coercion).

    A weaker form of traversals that can't focus on more than one element.
    {!Optional} is to [option] as [Traversal] is to [list]. *)
module Optional (F : S.MONAD) : sig
  module Lens : module type of Lens (F)

  module Prism : module type of Prism (F)

  type (-'s, 't, 'a, 'b) t

  val of_lens : ('s, 't, 'a, 'b) Lens.t -> ('s, 't, 'a, 'b) t
  (** Coerce a lens into a traversal. *)

  val of_prism : ('s, 't, 'a, 'b) Prism.t -> ('s, 't, 'a, 'b) t
  (** Coerce a prism into a traversal. *)

  val modify : ('s, 't, 'a, 'b) t -> ('a -> 'b F.t) -> 's -> 't option F.t
  (** Modify the focus of an optional element to the result of a monad action,
      and sequence those actions inside the monad. *)

  val get_opt : ('s, 't, 'a, 'b) t -> 's -> 'a option F.t
  (** Get the first targeted element, if it exists. *)

  val ( >> ) : ('a, 'b, 'c, 'd) t -> ('c, 'd, 'e, 'f) t -> ('a, 'b, 'e, 'f) t
  (** Composition for traversals. *)
end
