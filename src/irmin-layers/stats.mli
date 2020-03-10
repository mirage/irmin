type t = {
  mutable nb_freeze : int;
  mutable copied_contents : int list;
  mutable copied_nodes : int list;
  mutable copied_commits : int list;
  mutable copied_branches : int list;
}
(** The type for stats for a store S.

    - [nb_freeze] is the number of calls to [S.freeze];
    - [copied_contents] is the number of contents copied per freeze;
    - [copied_nodes] is the number of nodes copied per freeze;
    - [copied_commits] is the number of commits copied per freeze;
    - [copied_branches] is the number of branches copied per freeze.

    Note that stats assume that there always is an ongoing freeze. If its not
    the case, you should discard the last 0.*)

val reset_stats : unit -> unit

val get : unit -> t

val copy_contents : unit -> unit
(** Increments t.copied_contents for the current freeze *)

val copy_nodes : unit -> unit
(** Increments t.copied_nodes for the current freeze *)

val copy_commits : unit -> unit
(** Increments t.copied_commits for the current freeze *)

val copy_branches : unit -> unit
(** Increments t.copied_branches for the current freeze *)

val freeze : unit -> unit
(** Signals the start of a new freeze, and increments t.nb_freeze *)
