(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Core library for Irmin datastructures. *)

(** Equalities. *)
type 'a equal = 'a -> 'a -> bool

(** Comparators. *)
type 'a compare = 'a -> 'a -> int

(** Pretty-printing. *)
type 'a to_sexp = 'a -> Sexplib.Sexp.t
val pretty: ('a -> Sexplib.Sexp.t) -> 'a -> string

(** Cstruct readers. *)
type 'a reader = Cstruct.t -> (Cstruct.t * 'a) option

(** Cstruct writers. *)
type 'a writer = 'a -> Cstruct.t -> Cstruct.t

(** JSON converters. *)
type 'a to_json = 'a -> Ezjsonm.t
type 'a of_json = Ezjsonm.t -> 'a

(** Abstract Identifiers. *)
module type I0 = sig
  type t
  val equal: t equal
  val compare: t compare

  (** Used for debugging purposes, might expose internal state. *)
  val to_sexp: t to_sexp

  (** The REST inteface. *)
  val to_json: t to_json
  val of_json: t of_json

  (** The serialization format. *)
  val write: t writer
  val read: t reader
end

(** Build abstract identifiers. *)
module I0 (S: sig type t with sexp, bin_io, compare end):
  I0 with type t = S.t

(** Abstract identifiers with one polymorphic parameter. *)
module type I1 = sig
  type 'a t
  val equal: 'a equal -> 'a t equal
  val compare: 'a compare -> 'a t compare
  val to_sexp: 'a to_sexp -> 'a t to_sexp

  (** The REST interface *)
  val to_json: 'a to_json -> 'a t to_json
  val of_json: 'a of_json -> 'a t of_json

  (** The serialization format *)
  val write: 'a writer -> 'a t writer
  val read: 'a reader -> 'a t reader
end

(** Build abstract identifiers with a polymorphic parameters. *)
module I1 (S: sig type 'a t with sexp, compare, bin_io end):
  I1 with type 'a t = 'a S.t

(** Abstract identifiers with two polymorphic parameters. *)
module type I2 = sig
  type ('a, 'b) t
  val equal: 'a equal -> 'b equal -> ('a, 'b) t equal
  val compare: 'a compare -> 'b compare -> ('a, 'b) t compare
  val to_sexp: 'a to_sexp -> 'b to_sexp -> ('a, 'b) t to_sexp

  (** The REST interface *)
  val to_json: 'a to_json -> 'b to_json -> ('a, 'b) t to_json
  val of_json: 'a of_json -> 'b of_json -> ('a, 'b) t of_json

  (** The serialization format *)
  val write: 'a writer -> 'b writer -> ('a, 'b) t writer
  val read: 'a reader -> 'b reader -> ('a, 'b) t reader
end

(** Build abstract identfiers with two polymorphic parameters. *)
module I2 (S: sig type ('a, 'b) t with sexp, compare, bin_io end):
  I2 with type ('a, 'b) t = ('a, 'b) S.t

(** Dictionnaries with polymorphic values. *)
module type DICT = sig
  include I1
  type key

  (** Constructors *)
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> [`Ok of 'a t | `Duplicate_key of key]
  val of_alist_exn: (key * 'a) list -> 'a t
  val keys: 'a t -> key list

  (** Queries *)
  val is_empty: 'a t -> bool
  val mem: 'a t -> key -> bool
  val find: 'a t -> key -> 'a option

  (** Iterators *)
  val fold: 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val map: 'a t -> f:('a -> 'b) -> 'b t
  val iter: 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val filter: 'a t -> f:(key:key -> data:'a -> bool) -> 'a t
end

(** Hash Tables. *)
module Hashtbl: sig
  module type S = sig
    include DICT
    val create: ?size:int -> unit -> 'a t
    val clear: 'a t -> unit
    val of_alist_add: (key * 'a) list -> 'a t
    val replace: 'a t -> key:key -> data:'a -> unit
    val add: 'a t -> key:key -> data:'a -> [`Ok | `Duplicate]
    val add_exn: 'a t -> key:key -> data:'a -> unit
    val add_multi: 'a list t -> key:key -> data:'a -> unit
    val remove: 'a t -> key -> unit
  end
  val hash: 'a -> int
  module Make (K: I0): S with type key = K.t
end

(** Persistent Maps. *)
module Map: sig
  module type S = sig
    include DICT
    val empty: 'a t
    val add: 'a t -> key:key -> data:'a -> 'a t
    val remove: 'a t -> key -> 'a t
    module Lwt: sig
      val merge: 'v1 t ->'v2 t ->
        f:(key:key -> [ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ] -> 'v3 option Lwt.t) ->
        'v3 t Lwt.t
      val iter2: 'v1 t -> 'v2 t ->
        f:(key:key ->data:[ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ] -> unit Lwt.t) ->
        unit Lwt.t
    end
  end
  module Make (K: I0): S with type key = K.t
end

(** Persistent Sets. *)
module Set: sig
  module type S = sig
    include I0
    type elt
    val of_list: elt list -> t
    val to_list: t -> elt list
  end
  module Make (K: I0): S with type elt = K.t
end

(** Strings. *)
module String: sig
  include I0 with type t = string
  val is_empty: t -> bool
  val split: t -> on:char -> string list
end

(** Characters. *)
module Char: sig
  include I0 with type t = char
  val to_int: char -> int
  val of_int: int -> char option
  val of_int_exn: int -> char
end

(** Polymorphic Containers. *)
module Option: sig
  include I1 with type 'a t = 'a option
end

(** Strings allocated in the C heap. *)
module Bigstring: sig
  open Bigarray
  include I0 with type t = (char, int8_unsigned_elt, c_layout) Array1.t
end

(** Polymorphic Lists. *)
module List: sig
  include I1
  val fold_left: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val dedup: ?compare:'a compare -> 'a t -> 'a t
  val mem: 'a t -> 'a -> bool
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val partition_map : 'a t -> f:('a -> [ `Fst of 'b | `Snd of 'c ]) -> 'b t * 'c t
  module Assoc : sig
    include I2 with type ('a, 'b) t = ('a * 'b) list
    val find: ('a, 'b) t -> ?equal:'a equal -> 'a -> 'b option
    val find_exn: ('a, 'b) t -> ?equal:'a equal -> 'a -> 'b
  end
end

(** Polymorphic Stacks. *)
module Stack: sig
  include I1
  val create: unit -> 'a t
  val to_list: 'a t -> 'a list
  val of_list: 'a list -> 'a t
  val push: 'a t -> 'a -> unit
  val pop: 'a t -> 'a option
  val pop_exn: 'a t -> 'a
end

(** Integers. *)
module Int: sig
  include I0 with type t = int
  val max_value: t
end

(** 64-bits Integers. *)
module Int64: sig
  include I0 with type t = int64
  val (+): t -> t -> t
end

(** Polymorphic mutable queues. *)
module Queue: sig
  include I1
  val enqueue: 'a t -> 'a -> unit
  val dequeue: 'a t -> 'a option
end

module Unit: sig
  include I0 with type t = unit
end

module Out_channel: sig
  type t = out_channel
  val create: string -> t
  val close: t -> unit
end
