(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Daniel C. Bünzli
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

(** {1 Configuration converters}

    A configuration converter transforms a string value to an OCaml value and
    vice-versa. There are a few {{!builtin_converters} built-in converters}. *)

module type S = sig
  type 'a key
  type t
  type k

  val key :
    ?docs:string ->
    ?docv:string ->
    ?doc:string ->
    string ->
    'a Type.t ->
    'a ->
    'a key
  (** [key ~docs ~docv ~doc name conv default] is a configuration key named
      [name] that maps to value [default] by default. [conv] is used to convert
      key values provided by end users.

      [docs] is the title of a documentation section under which the key is
      documented. [doc] is a short documentation string for the key, this should
      be a single sentence or paragraph starting with a capital letter and
      ending with a dot. [docv] is a meta-variable for representing the values
      of the key (e.g. ["BOOL"] for a boolean).

      @raise Invalid_argument if the key name is not made of a sequence of ASCII
      lowercase letter, digit, dash or underscore.

      {b Warning.} No two keys should share the same [name] as this may lead to
      difficulties in the UI. *)

  val name : 'a key -> string
  (** The key name. *)

  val ty : 'a key -> 'a Type.t
  (** [tc k] is [k]'s converter. *)

  val default : 'a key -> 'a
  (** [default k] is [k]'s default value. *)

  val doc : 'a key -> string option
  (** [doc k] is [k]'s documentation string (if any). *)

  val docv : 'a key -> string option
  (** [docv k] is [k]'s value documentation meta-variable (if any). *)

  val docs : 'a key -> string option
  (** [docs k] is [k]'s documentation section (if any). *)
  val root : unit -> string key
  (** Default [--root=ROOT] argument. *)

  (** {1:conf Configurations} *)

  val empty : t
  (** [empty] is the empty configuration. *)

  val singleton : 'a key -> 'a -> t
  (** [singleton k v] is the configuration where [k] maps to [v]. *)

  val is_empty : t -> bool
  (** [is_empty c] is [true] iff [c] is empty. *)

  val mem : t -> 'a key -> bool
  (** [mem c k] is [true] iff [k] has a mapping in [c]. *)

  val add : t -> 'a key -> 'a -> t
  (** [add c k v] is [c] with [k] mapping to [v]. *)

  val rem : t -> 'a key -> t
  (** [rem c k] is [c] with [k] unbound. *)

  val union : t -> t -> t
  (** [union r s] is the union of the configurations [r] and [s]. *)

  val find : t -> 'a key -> 'a option
  (** [find c k] is [k]'s mapping in [c], if any. *)

  val get : t -> 'a key -> 'a
  (** [get c k] is [k]'s mapping in [c].

      {b Raises.} [Not_found] if [k] is not bound in [d]. *)

  val keys : t -> k Seq.t
  (** [keys c] returns a [Seq.t] containing keys present in a configuration *)

  val all_keys : unit -> k Seq.t
  (** [all_keys ()] is a [Seq.t] of all registered keys *)

  val find_key : string -> k option
  (** [find_key k] returns the key associated with the name [k] if available *)


  (** {1:builtin_converters Built-in value converters} *)

  val uri : Uri.t Type.t
  (** [uri] converts values with {!Uri.of_string}. *)
end

module type Sigs = sig
  type 'a key
  (** The type for configuration keys whose lookup value is ['a]. *)

  type t
  (** Configuration type *)

  type k = Key : 'a key -> k

  module type S = S with type t = t and type 'a key = 'a key and type k = k

  val name : 'a key -> string
  (** The key name. *)

  val ty : 'a key -> 'a Type.t
  (** [tc k] is [k]'s converter. *)

  val default : 'a key -> 'a
  (** [default k] is [k]'s default value. *)

  val doc : 'a key -> string option
  (** [doc k] is [k]'s documentation string (if any). *)

  val docv : 'a key -> string option
  (** [docv k] is [k]'s value documentation meta-variable (if any). *)

  val docs : 'a key -> string option
  (** [docs k] is [k]'s documentation section (if any). *)

  module Make () : S
  module Extend (S : S) : S with type k = S.k
end
