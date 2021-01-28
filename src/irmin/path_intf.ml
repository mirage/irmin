module type S = sig
  (** {1 Path} *)

  type t
  (** The type for path values. *)

  type step
  (** Type type for path's steps. *)

  val empty : t
  (** The empty path. *)

  val v : step list -> t
  (** Create a path from a list of steps. *)

  val is_empty : t -> bool
  (** Check if the path is empty. *)

  val cons : step -> t -> t
  (** Prepend a step to the path. *)

  val rcons : t -> step -> t
  (** Append a step to the path. *)

  val decons : t -> (step * t) option
  (** Deconstruct the first element of the path. Return [None] if the path is
      empty. *)

  val rdecons : t -> (t * step) option
  (** Deconstruct the last element of the path. Return [None] if the path is
      empty. *)

  val map : t -> (step -> 'a) -> 'a list
  (** [map t f] maps [f] over all steps of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)
end

module type Path = sig
  module type S = S
  (** Signature for path implementations.*)

  (** An implementation of paths as string lists. *)
  module String_list : S with type step = string and type t = string list
end
