open! Import

module type S = sig
  module Hash : Irmin.Hash.S

  include
    Irmin.Node.S
      with type contents_key = Pack_key.Make(Hash).t
       and type node_key = Pack_key.Make(Hash).t
end
