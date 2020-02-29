(** Optic subtyping relations

    This module defines the subtyping relations between various optics, using
    so-called 'feature types'. The 'features' of an optic are represented by a
    polymorphic variant, such as:

    [feat] = [`map | `get]

    for an optic that *)

(** {2 Feature types} *)

(** These types define the features that an optic may support. *)

type at_least_one = [ `at_least_one ]
(** The property of an optic with one or more foci. *)

type at_most_one = [ `at_most_one ]
(** The property of an optic with either zero foci or a single focus. *)

type construct = [ `construct ]
(** The property of an optic that can be used to synthesise values of the return
    type. *)

type get = [ `get ]

type map = [ `map ]

type update = [ `update ]

(** Aliases for combinations of the above properties: *)

type exactly_one = [ at_least_one | at_most_one ]

(** {2 Optic features} *)

(** These types define the features supported by each optic. *)

type lens = [ get | map | update | at_most_one | at_least_one ]
(** An optic that focuses on exactly one component of a {i product} type. *)

type prism = [ get | map | update | at_most_one | construct ]
(** An optic that focuses on exactly one component of a {i sum} type. *)

type optional = [ get | map | update | at_most_one ]
(** An optic that focuses on zero or one element in a structure.

    In terms of subtyping, [optional] = [lens] ⊔ [prism]. *)
