(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module type S = sig
  include Index.S with type value = Int63.t * int * char

  val v :
    ?flush_callback:(unit -> unit) ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?throttle:[ `Block_writes | `Overcommit_memory ] ->
    log_size:int ->
    string ->
    t
  (** Constructor for indices, memoized by [(path, readonly)] pairs. *)

  val find : t -> key -> value option
  val add : ?overcommit:bool -> t -> key -> value -> unit
  val close : t -> unit
  val merge : t -> unit

  module Stats = Index.Stats
end

module type Pack_index = sig
  module type S = S

  module Make (K : Irmin.Hash.S) : S with type key = K.t
end
