open! Import

module Make (Errs : Io_errors.S with module Io = Io.Unix) : sig
  val create :
    root:string ->
    generation:int ->
    register_entries:(register_entry:(off:int63 -> len:int -> unit) -> unit) ->
    (unit, [> Errs.t ]) result
  (** [create] creates inside the directory [root] a mapping file. It never
      raises exceptions.

      [register_entries] is a user callback that is responsible for calling
      [register_entry] for each live entry. Duplicates allowed, no specfic order
      expected.

      Returns an error if the platform is not 64bits.

      Works on both little-endian and big-endian platforms.

      Creates temporary files in [root] that are unlinked before the function
      returns. *)

  val iter :
    Io.Unix.t -> (off:int63 -> len:int -> unit) -> (unit, [> Errs.t ]) result
  (** [iter ~path f] Iterate over the entries of the mapping file at [path].

      It is guaranteed for the offsets to be iterated in monotonic order.

      It is guaranteed that entries don't overlap.

      The exceptions raised by [f] are caught and returned (as long as they are
      known by [Errs]. *)
end
