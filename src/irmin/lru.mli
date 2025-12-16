(*
   Copyright (c) 2016 David Kaloper Meršinjak

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* Extracted from https://github.com/pqwy/lru *)

(** LRU cache implementation *)
module Make (H : Hashtbl.HashedType) : sig
  type 'a t

  val create : int option -> 'a t
  (** [create n] returns a new LRU cache with the maximum size of [n]. If [n] is
      None, the LRU cache is unbounded and is automatically internally resized.
  *)

  val add : 'a t -> H.t -> ?weight:int -> 'a -> unit
  (** [add t k ~weight v] adds the binding [k -> v] with weight [weight] to the
      cache [t]. If the cache is full, the least recently used element(s) are
      evicted until there is enough space to add the new element. If [k] was
      already bound, its previous binding is replaced by [v] and it is marked as
      most recently used.

      Default value of [weight] is 1. *)

  val find : 'a t -> H.t -> 'a
  (** [find t k] returns the value associated with [k] in the cache [t], and
      marks [k] as most recently used. Raises [Not_found] if [k] is not bound in
      [t]. *)

  val mem : 'a t -> H.t -> bool
  (** [mem t k] checks if [k] is bound in the cache [t], and marks [k] as most
      recently used if it is. *)

  val clear : 'a t -> unit
  (** [clear t] removes all bindings from the cache [t]. *)

  val iter : 'a t -> (H.t -> 'a -> unit) -> unit
  (** [iter t f] calls [f k v] for all bindings in the cache [t]. *)

  val drop : 'a t -> 'a option
  (** [drop t] removes the least recently used binding from the cache [t] and
      returns its value, or [None] if the cache is empty. *)
end
