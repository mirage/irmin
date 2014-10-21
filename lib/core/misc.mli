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

(** Mstruct reader. *)
type 'a reader = Mstruct.t -> 'a
exception Read_error

(** Pre-compute the size of the written objects. *)
type 'a size_of = 'a -> int

(** Cstruct writer. *)
type 'a writer = 'a -> Cstruct.t -> Cstruct.t

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

(** Derived type-classes for debugging *)

val force: out_channel -> string Lazy.t -> unit
val show: (module I0 with type t = 'a) -> 'a -> string Lazy.t
val shows: (module I0 with type t = 'a) -> 'a list -> string Lazy.t

(** Derived type-classes for reading *)

val read_string: (module I0 with type t = 'a) -> string -> 'a
val read_cstruct: (module I0 with type t = 'a) -> Cstruct.t -> 'a

(** Derived type-classes for writing *)

val write_string: (module I0 with type t = 'a) -> 'a -> string
val write_cstruct: (module I0 with type t = 'a) -> 'a -> Cstruct.t

(** Build abstract identifiers. *)
module I0 (S: sig type t with sexp, bin_io, compare end): I0 with type t = S.t

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

(** Persistent Maps. *)
module type MAP = sig
  include Map.S
  include I1 with type 'a t := 'a t
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val keys: 'a t -> key list
  val add_multi: key -> 'a -> 'a list t -> 'a list t
end
module Map (S: I0): MAP with type key = S.t

(** Persistent Sets. *)
module type SET = sig
  include Set.S
  include I0 with type t := t
  val of_list: elt list -> t
  val to_list: t -> elt list
end
module Set (K: I0): SET with type elt = K.t

val list_partition_map : 'a list -> f:('a -> [ `Fst of 'b | `Snd of 'c ]) -> 'b list * 'c list

module Hex: sig

  val encode: string -> string
  (** Encode a binary string to hexa *)

  val decode: string -> string
  (** Decode an hexa string to binary *)

end

module Lwt_stream: sig

  include (module type of Lwt_stream with type 'a t = 'a Lwt_stream.t)

  val lift: 'a t Lwt.t -> 'a t
  (** Lift a stream out of the monad. *)

end
