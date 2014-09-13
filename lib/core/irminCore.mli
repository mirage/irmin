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

(** Hashing. *)
type 'a hash = 'a -> int

(** Pretty-printing. *)
type 'a to_sexp = 'a -> Sexplib.Sexp.t

(** Cstruct readers. *)
type 'a reader = Cstruct.t -> (Cstruct.t * 'a) option
val read_all: 'a reader -> Cstruct.buffer -> 'a option

(** Pre-compute the size of the written objects. *)
type 'a size_of = 'a -> int

(** Cstruct writers. *)
type 'a writer = 'a -> Cstruct.t -> Cstruct.t
val write_all: 'a size_of -> 'a writer -> 'a -> Cstruct.buffer

(** JSON converters. *)
type 'a to_json = 'a -> Ezjsonm.t
type 'a of_json = Ezjsonm.t -> 'a

module JSON: sig

  val is_valid_utf8: string -> bool
  (** Check whether a string is valid UTF8 encoded. *)

  val encode_string: string -> Ezjsonm.t
  (** Convert a (possibly non-valid UTF8) string to a JSON object.*)

  val decode_string: Ezjsonm.t -> string option
  (** Convert a JSON object to a (possibly non-valid UTF8)
      string. Return [None] if the JSON object is not a valid string. *)

  val decode_string_exn: Ezjsonm.t -> string
  (** Convert a JSON object to a (possibly non-valid UTF8) string. *)

  val to_sexp: Ezjsonm.t -> Sexplib.Type.t
  val of_sexp: Sexplib.Type.t -> Ezjsonm.t

end

(** Abstract Identifiers. *)
module type I0 = sig
  type t
  val equal: t equal
  val compare: t compare
  val hash: t hash

  (** Used for debugging purposes, might expose internal state. *)
  val to_sexp: t to_sexp

  (** The REST inteface. *)
  val to_json: t to_json
  val of_json: t of_json

  (** The serialization format. *)
  val size_of: t size_of
  val write: t writer
  val read: t reader
end

(** type-classes *)
val equal: (module I0 with type t = 'a) -> 'a equal
val compare: (module I0 with type t = 'a) -> 'a compare
val hash: (module I0 with type t = 'a) -> 'a hash
val to_sexp: (module I0 with type t = 'a) -> 'a to_sexp
val to_json: (module I0 with type t = 'a) -> 'a to_json
val size_of: (module I0 with type t = 'a) -> 'a size_of
val write: (module I0 with type t = 'a) -> 'a writer
val read: (module I0 with type t = 'a) -> 'a reader

(** derived type-classes for debugging*)
val force: out_channel -> string Lazy.t -> unit
val pretty: (module I0 with type t = 'a) -> 'a -> string Lazy.t
val prettys: (module I0 with type t = 'a) -> 'a list -> string Lazy.t

(** Build abstract identifiers. *)
module I0 (S: sig type t with sexp, bin_io, compare end):
  I0 with type t = S.t

(** Abstract identifiers with one polymorphic parameter. *)
module type I1 = sig
  type 'a t
  val equal: 'a equal -> 'a t equal
  val compare: 'a compare -> 'a t compare
  val hash: 'a hash -> 'a t hash

  (** Pretty-printing *)
  val to_sexp: 'a to_sexp -> 'a t to_sexp

  (** The REST interface *)
  val to_json: 'a to_json -> 'a t to_json
  val of_json: 'a of_json -> 'a t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'a t size_of
  val write: 'a writer -> 'a t writer
  val read: 'a reader -> 'a t reader
end

(** Build abstract identifiers with a polymorphic parameters. *)
module I1 (S: sig type 'a t with sexp, compare, bin_io end):
  I1 with type 'a t = 'a S.t

(** Monorphize a type with one parameter. *)
module App1 (F: I1)(X: I0): I0 with type t = X.t F.t

(** Abstract identifiers with two polymorphic parameters. *)
module type I2 = sig
  type ('a, 'b) t
  val equal: 'a equal -> 'b equal -> ('a, 'b) t equal
  val compare: 'a compare -> 'b compare -> ('a, 'b) t compare
  val hash: 'a hash -> 'b hash -> ('a, 'b) t hash

  (** Pretty-printing *)
  val to_sexp: 'a to_sexp -> 'b to_sexp -> ('a, 'b) t to_sexp

  (** The REST interface *)
  val to_json: 'a to_json -> 'b to_json -> ('a, 'b) t to_json
  val of_json: 'a of_json -> 'b of_json -> ('a, 'b) t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'b size_of -> ('a, 'b) t size_of
  val write: 'a writer -> 'b writer -> ('a, 'b) t writer
  val read: 'a reader -> 'b reader -> ('a, 'b) t reader
end

(** Build abstract identfiers with two polymorphic parameters. *)
module I2 (S: sig type ('a, 'b) t with sexp, compare, bin_io end):
  I2 with type ('a, 'b) t = ('a, 'b) S.t

(** Monorphize a type with two parameters. *)
module App2(F: I2)(X: I0)(Y: I0): I0 with type t = (X.t, Y.t) F.t


(** Abstract identifiers with two polymorphic parameters. *)
module type I3 = sig
  type ('a, 'b, 'c) t
  val equal: 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
  val compare: 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
  val hash: 'a hash -> 'b hash -> 'c hash -> ('a, 'b, 'c) t hash

  (** Pretty-printing *)
  val to_sexp: 'a to_sexp -> 'b to_sexp -> 'c to_sexp -> ('a, 'b, 'c) t to_sexp

  (** The REST interface *)
  val to_json: 'a to_json -> 'b to_json -> 'c to_json -> ('a, 'b, 'c) t to_json
  val of_json: 'a of_json -> 'b of_json -> 'c of_json -> ('a, 'b, 'c) t of_json

  (** The serialization format *)
  val size_of: 'a size_of -> 'b size_of -> 'c size_of -> ('a, 'b, 'c) t size_of
  val write: 'a writer -> 'b writer -> 'c writer -> ('a, 'b, 'c) t writer
  val read: 'a reader -> 'b reader -> 'c reader -> ('a, 'b, 'c) t reader
end

(** Build abstract identfiers with two polymorphic parameters. *)
module I3 (S: sig type ('a, 'b, 'c) t with sexp, compare, bin_io end):
  I3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) S.t

(** Monorphize a type with three parameters. *)
module App3(F: I3)(X: I0)(Y: I0)(Z: I0): I0 with type t = (X.t, Y.t, Z.t) F.t

(** List-like data-structures. *)
module type ListLike0 = sig
  type t
  module K: I0
  val to_list: t -> K.t list
  val of_list: K.t list -> t
end
module ListLike0 (L: ListLike0): sig
  include I0 with type t = L.t
  include ListLike0 with type t := t
end

(** List-like data-structures with polymorphic elements. *)
module type ListLike1 = sig
  type 'a t
  val to_list: 'a t -> 'a list
  val of_list: 'a list -> 'a t
end
module ListLike1 (L: ListLike1): sig
  include I1 with type 'a t = 'a L.t
  include ListLike1 with type 'a t := 'a t
end

(** Association list-like data-structures, with polymorphic values. *)
module type AListLike1 = sig
  type 'a t
  module K: I0
  val to_alist: 'a t -> (K.t * 'a) list
  val of_alist: (K.t * 'a) list -> [`Ok of 'a t | `Duplicate_key of K.t]
  val of_alist_exn: (K.t * 'a) list -> 'a t
end
module AListLike1 (L: AListLike1): sig
  include I1 with type 'a t = 'a L.t
  include AListLike1 with type 'a t := 'a t
end

(** Dictionaries with polymorphic values. *)
module type Dictionary = sig
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
    include Dictionary
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
    include Dictionary
    val empty: 'a t
    val add: 'a t -> key:key -> data:'a -> 'a t
    val remove: 'a t -> key -> 'a t
    val keys: 'a t -> key list
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
    val empty: t
    val singleton: elt -> t
    val is_empty: t -> bool
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val of_list: elt list -> t
    val to_list: t -> elt list
  end
  module Make (K: I0): S with type elt = K.t
end

(** Strings. *)
module String: sig
  include I0 with type t = string
  val create: int -> t
  val make: int -> char -> t
  val get: t -> int -> char
  val set: t -> int -> char -> unit
  val is_empty: t -> bool
  val length: t -> int
  val sub: t -> pos:int -> len:int -> t
  val split: t -> on:char -> t list
  val blit: t -> int -> t -> int -> int -> unit
  val concat: t list -> sep:t -> t
  val escaped: t -> t

  val replace: pattern:t -> (t -> t) -> t -> t
  (** Replace a pattern in a string. *)

  module Hex: sig

    val encode: string -> string
    (** Encode a binary string to hexa *)

    val decode: string -> string
    (** Decode an hexa string to binary *)

  end

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

(** Pairs *)
module Pair: sig
  include I2 with type ('a, 'b) t = 'a * 'b
end

(** Strings allocated in the C heap. *)
module Bigstring: sig
  open Bigarray
  include I0 with type t = (char, int8_unsigned_elt, c_layout) Array1.t
  val to_string: t -> string
  val of_string: string -> t
end

(** Polymorphic Lists. *)
module List: sig
  include I1 with type 'a t = 'a list
  val fold_left: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val dedup: ?compare:'a compare -> 'a t -> 'a t
  val sort: ?compare:'a compare -> 'a t -> 'a t
  val mem: 'a t -> 'a -> bool
  val length: 'a t -> int
  val rev: 'a t -> 'a t
  val iter: 'a t -> f:('a -> unit) -> unit
  val map: 'a t -> f:('a -> 'b) -> 'b t
  val rev_map: 'a t -> f:('a -> 'b) -> 'b t
  val exists: 'a t -> f:('a -> bool) -> bool
  val filter: 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val partition_map : 'a t -> f:('a -> [ `Fst of 'b | `Snd of 'c ]) -> 'b t * 'c t
  module Assoc : sig
    include I2 with type ('a, 'b) t = ('a * 'b) list
    val find: ('a, 'b) t -> ?equal:'a equal -> 'a -> 'b option
    val find_exn: ('a, 'b) t -> ?equal:'a equal -> 'a -> 'b
  end

  val pretty: ('a -> string) -> 'a t -> string
  (** Pretty-print a list. *)

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
  val to_string: t -> string
end

(** Polymorphic mutable queues. *)
module Queue: sig
  include I1
  val create: unit -> 'a t
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

module Reader: sig
  val to_bin_prot: 'a reader -> 'a Bin_prot.Read.reader
  val of_bin_prot: 'a Bin_prot.Read.reader -> 'a reader
end

module Writer: sig
  val to_bin_prot: 'a writer -> 'a Bin_prot.Write.writer
  val of_bin_prot: 'a Bin_prot.Write.writer -> 'a writer
end

module Lwt_stream: sig

  include (module type of Lwt_stream with type 'a t = 'a Lwt_stream.t)

  val lift: 'a t Lwt.t -> 'a t
  (** Lift a stream out of the monad. *)

end
