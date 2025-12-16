(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t
(** The type of mutable sets of fixed-length strings. *)

type elt := string

val create :
  elt_length:int ->
  ?initial_slots:int ->
  ?hash:(elt -> int) ->
  ?hash_substring:(Bigstringaf.t -> off:int -> len:int -> int) ->
  ?null:string ->
  unit ->
  t
(** [create] builds an empty set of fixed-length strings. The parameters are as
    follows:

    - [elt_length]: the length of each element string in bytes;

    - [initial_slots]: the minimum number of slots contained in the initial
      internal buffer (NOTE: the actual number of slots will be the least power
      of two greater than or equal to [initial_buffer]. This is not the same as
      the number of elements that can fit inside the buffer, which also depends
      on the maximum load factor);

    - [hash] / [hash_substring]: functions to use for placing elements inside
      the internal buffer (given that the element is contained in a string or a
      bigstring respectively). The stored elements must have uniformly
      distributed [hash] results for good performance, and the two hash
      functions must be equivalent. Defaults to [Hashtbl.hash] (and an
      equivalent function on substrings).

    - [null]: a string of size [elt_length] that is guaranteed to never be added
      to the hashset. Passing this string to {!add} or {!mem} after creating the
      hashset will result in an exception being raised. *)

include Hashset.S with type t := t and type elt := elt

val invariant : (elt -> unit) -> t -> unit
(** [invariant f t] checks the internal invariants of [t] and calls [f] on every
    element contained within. Exposed for testing. *)

val reachable_words : t -> int
(** [reachable_words t] is the total number of words of data kept alive by [t]
    (on both the C and OCaml heaps). *)
