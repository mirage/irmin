type 'a typ = 'a Irmin.Type.t

module Id : sig
  type 'a t [@@deriving irmin]
end = struct
  type 'a t = 'a [@@deriving irmin]
end

let __ : type a. a typ -> a Id.t typ = Id.t

module Phantom : sig
  type _ t = int [@@deriving irmin]
end = struct
  type _ t = int [@@deriving irmin]
end

let __ : type a. a typ -> a Phantom.t typ = Phantom.t

module Multiple : sig
  type ('a, 'b, 'c) t = { foo : 'a; bar : 'b list; baz : 'b * 'c }
  [@@deriving irmin]
end = struct
  type ('a, 'b, 'c) t = { foo : 'a; bar : 'b list; baz : 'b * 'c }
  [@@deriving irmin]
end

let __ : type a b c. a typ -> b typ -> c typ -> (a, b, c) Multiple.t typ =
  Multiple.t
