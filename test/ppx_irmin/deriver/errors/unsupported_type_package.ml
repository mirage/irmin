module type S = sig
  val x : unit
end

type t = (module S) [@@deriving irmin]
