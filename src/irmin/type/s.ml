module type FUNCTOR = sig
  type +'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  module Infix : sig
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module type APPLICATIVE = sig
  include FUNCTOR

  val return : 'a -> 'a t

  val lift_bin : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type MONAD = sig
  include APPLICATIVE

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  module Infix : sig
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    (** Infix alias for {!map}. *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** Infix alias for {!bind}. *)

    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    (** Left-to-right Kleisli composition of monads. *)
  end

  module Syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    (** Syntactic sugar for {!map}. *)

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** Syntactic sugar for {!bind}. *)
  end
end
