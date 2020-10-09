(** This module is intended to be globally opened. *)

type +'a staged

val stage : 'a -> 'a staged

val unstage : 'a staged -> 'a
