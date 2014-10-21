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

(** Misc functions and signatures. *)

module OP: sig
  val (!!): out_channel -> string Lazy.t -> unit
end

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

(** Persistent Maps. *)
module type MAP = sig
  include Map.S
  include Tc.I1 with type 'a t := 'a t
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val keys: 'a t -> key list
  val add_multi: key -> 'a -> 'a list t -> 'a list t
  val iter2:
    (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> unit)
    -> 'a t -> 'b t -> unit
  module Lwt: sig
    val merge:
      (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option Lwt.t)
      -> 'a t ->'b t -> 'c t Lwt.t
    val iter2:
      (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> unit Lwt.t)
      -> 'a t ->'b t -> unit Lwt.t
  end
end
module Map (S: Tc.I0): MAP with type key = S.t

(** Persistent Sets. *)
module type SET = sig
  include Set.S
  include Tc.I0 with type t := t
  val of_list: elt list -> t
  val to_list: t -> elt list
end
module Set (K: Tc.I0): SET with type elt = K.t

val list_partition_map: ('a -> [ `Fst of 'b | `Snd of 'c ]) ->
  'a list -> 'b list * 'c list

val list_pretty: ('a -> string) -> 'a list -> string

val list_filter_map: ('a -> 'b option) -> 'a list -> 'b list

val list_dedup: ?compare:'a Tc.compare -> 'a list -> 'a list

val hashtbl_add_multi: ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit

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

module StringMap: MAP with type key = string
