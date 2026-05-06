include Repr

module type Defaultable = sig
  include S

  val default : t
end
