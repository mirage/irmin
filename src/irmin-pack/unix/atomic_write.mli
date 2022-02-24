open! Import
include module type of Irmin_pack.Atomic_write

module Make_persistent (K : Irmin.Type.S) (V : Value.S) :
  Persistent with type key = K.t and type value = V.t
