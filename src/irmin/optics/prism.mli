open Irmin_root
open Brands

(** First-class sum components extended with monadic effects. *)

type ('s, 't, 'a, 'b, 'm) t = private {
  monad : 'm monad;
  review : 'b -> ('t, 'm) app;
  preview : 's -> ('a option, 'm) app;
}
(** Prisms paremeterised by monadic effects. ['m] is the brand of the
    instantiated monadic effect. *)

type ('s, 'a, 'm) mono = ('s, 's, 'a, 'a, 'm) t

type (_, 'm) t_list =
  | ( :: ) :
      ('s, 't, 'a, 'b, 'm) t * ('l, 'm) t_list
      -> (('s, 't, 'a, 'b, 'm) t * 'l, 'm) t_list
  | [] : (unit, 'm) t_list

val v :
  'm monad ->
  ('b -> ('t, 'm) app) ->
  ('s -> ('a option, 'm) app) ->
  ('s, 't, 'a, 'b, 'm) t
(** [v monad review preview] is the prism given by the constructor [review] and
    the focusing function [preview], each of which runs with effect [monad]. *)

val natural_compose :
  ('d -> (('d, 'n) app, 'm) app) ->
  (('c, 'n) app -> ('c, 'm) app) ->
  ('a, 'b, ('c, 'n) app, ('d, 'n) app, 'm) t ->
  ('c, 'd, ('e, 'n) app, ('f, 'n) app, 'm) t ->
  ('a, 'b, ('e, 'n) app, ('f, 'n) app, 'm) t

val review : ('s, 't, 'a, 'b, 'm) t -> 'b -> ('t, 'm) app
(** [review p] is a constructor for the sum case selected by prism [p]. *)

val preview : ('s, 't, 'a, 'b, 'm) t -> 's -> ('a option, 'm) app
(** [preview p] is a partial getter for the sum case selected by prism [p]. *)

val ( >> ) :
  ('a, 'b, 'c, 'd, 'm) t -> ('c, 'd, 'e, 'f, 'm) t -> ('a, 'b, 'e, 'f, 'm) t
(** [l1 >> l2] is the composition of prisms l1 and l2. *)

val id : 'm monad -> ('a, 'a, 'm) mono
(** The identity prism. *)
