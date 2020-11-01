type empty = |

module type S = sig
  val cli : unit -> empty
  (** Run a [Cmdliner] binary containing tools for running offline checks. *)
end

module type Checks = sig
  type nonrec empty = empty

  module type S = S

  module Make (H : Irmin.Hash.S) (Index : Pack_index.S) : S
end
