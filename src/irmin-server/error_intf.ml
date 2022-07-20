type error = [ `Msg of string ]
type t = error
type 'a result = ('a, error) Result.t

exception Error of string

module type Error = sig
  type t = error
  type 'a result = ('a, error) Result.t

  exception Error of string

  val raise_error : string -> 'a

  val unwrap : string -> 'a result -> 'a
  (** Raise an exception if the result is [Error] *)

  val of_string : string -> t
  val to_string : t -> string
end
