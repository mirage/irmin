open Staging

val check_valid_utf8 : string -> unit

val is_valid_utf8 : string -> bool

val fix_staged : (('a -> 'b) staged -> ('a -> 'b) staged) -> ('a -> 'b) staged
(** Fixpoint combinator that unrolls exactly once via lazy, recursively-defined
    values. Useful when unrolling has a non-negligible performance cost, e.g.
    incurs many heap allocations. *)
