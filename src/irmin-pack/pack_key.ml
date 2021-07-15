open! Import

type 'hash t = {
  hash : 'hash;
  index : int63 * int * Pack_value.Kind.t;
      (* TODO: this is equal to Pack_index.value *)
}
[@@deriving irmin]

module Make (Hash : Irmin.Hash.S) : sig
  include Irmin.Key.S with type t = Hash.t t
end = struct
  type nonrec t = Hash.t t [@@deriving irmin]
  type hash = Hash.t

  let hash t = t.hash
end
