(** [Reversed_list.t] is constructed the same way as [List.t], but needs to be
    reversed before it can be used as a regular list.

    This is helpful when building up a list in reverse in order to force
    reversal at the end of the accumulation process. *)

type 'a t = [] | ( :: ) of 'a * 'a t [@@deriving irmin]

val rev : 'a t -> 'a list
