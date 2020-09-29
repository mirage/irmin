open Staging

val check_valid_utf8 : string -> unit

val is_valid_utf8 : string -> bool

val fix_staged : ('f -> 'f) -> ((_ -> _) staged as 'f)
(** Fixpoint combinator that unrolls exactly once via lazy, recursively-defined
    values. Useful when unrolling has a non-negligible performance cost, e.g.
    incurs many heap allocations. *)

val fix_staged2 :
  ('f1 -> 'f2 -> 'f1 * 'f2) ->
  ((_ -> _) staged as 'f1) * ((_ -> _) staged as 'f2)
(** Generalises {!fix_staged} to handle mutually recursive definitions. *)
