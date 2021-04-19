open! Import

module type S = sig
  module Hash : Irmin.Hash.S

  include
    Irmin.Node.S
      with type contents_key = Hash.t Pack_key.t
       and type node_key = Hash.t Pack_key.t
end
