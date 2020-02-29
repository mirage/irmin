open Irmin_root
open Brands

(** First class product components extended with monadic effects. *)

type ('s, 't, 'a, 'b, 'm) t = private {
  monad : 'm monad;
  view : 's -> ('a, 'm) app;
  modify : ('a -> ('b, 'm) app) -> 's -> ('t, 'm) app;
}
(** Lenses paremeterised by monadic effects. ['m] is the brand of the
    instantiated monadic effect. *)

type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

type (_, 'm) t_list =
  | ( :: ) :
      ('s, 't, 'a, 'b, 'm) t * ('l, 'm) t_list
      -> (('s, 't, 'a, 'b, 'm) t * 'l, 'm) t_list
  | [] : (unit, 'm) t_list
      (** Convenient syntax for a heterogeneous list of lenses. *)

val v :
  'm monad ->
  ('s -> ('a, 'm) app) ->
  (('a -> ('b, 'm) app) -> 's -> ('t, 'm) app) ->
  ('s, 't, 'a, 'b, 'm) t
(** [v monad f m] is the lens given by the focusing function [f] and the
    modification function [m], each of which runs with effect [monad]. *)

val view : ('s, 't, 'a, 'b, 'm) t -> 's -> ('a, 'm) app
(** [view l] is the focusing function of [l]. *)

val modify :
  ('s, 't, 'a, 'b, 'm) t -> ('a -> ('b, 'm) app) -> 's -> ('t, 'm) app
(** [modify l f s] is the result of applying [f] to the focus of [l] in [s]. *)

val update : ('s, 't, 'a, 'b, 'm) t -> ('b, 'm) app -> 's -> ('t, 'm) app
(** [update l b s] replaces the focus of [l] in [s] with [b]. *)

val ( >> ) :
  ('a, 'b, 'c, 'd, 'm) t -> ('c, 'd, 'e, 'f, 'm) t -> ('a, 'b, 'e, 'f, 'm) t
(** [l1 >> l2] is the composition of lenses [l1] and [l2]. *)
