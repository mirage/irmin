(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
    vice-versa. *)

(** {1:keys Keys} *)

type 'a key
(** The type for configuration keys whose lookup value is ['a]. *)

type k = K : 'a key -> k

module Spec : sig
  type t
  (** A configuration spec is used to group keys by backend *)

  val v : string -> t
  (** [v name] is a new configuration specification named [name] *)

  val name : t -> string
  (** [name spec] is the name associated with a config spec *)

  val find_key : t -> string -> k option
  (** [find_key spec k] is the key associated with the name [k] in [spec] *)

  val keys : t -> k Seq.t
  (** [keys spec] is a sequence of keys available in [spec] *)

  val join : t -> t list -> t
  (** [join a b] is a new [Spec.t] combining [a] and all specs present in [b]

      The name of the resulting spec will be the name of [a] and the names of
      the specs in [b] joined by hyphens. *)

  val copy : t -> t
  (** [copy t] returns a fresh spec with the same keys as [t]. *)
end

module Typ : sig
  type 'a t

  val create : unit -> 'a t
end

val key :
  ?docs:string ->
  ?docv:string ->
  ?doc:string ->
  ?typ:'a Typ.t ->
  spec:Spec.t ->
  string ->
  'a Type.t ->
  'a ->
  'a key
(** [key ~docs ~docv ~doc ~spec name conv default] is a configuration key named
    [name] that maps to value [default] by default. It will be associated with
    the config grouping [spec]. [conv] is used to convert key values provided by
    end users.

    [docs] is the title of a documentation section under which the key is
    documented. [doc] is a short documentation string for the key, this should
    be a single sentence or paragraph starting with a capital letter and ending
    with a dot. [docv] is a meta-variable for representing the values of the key
    (e.g. ["BOOL"] for a boolean).

    @raise Invalid_argument
      if the key name is not made of a sequence of ASCII lowercase letter,
      digit, dash or underscore. *)

val serialized_key :
  ?docs:string ->
  ?docv:string ->
  ?doc:string ->
  ?typ:'a Typ.t ->
  spec:Spec.t ->
  typename:string ->
  to_string:('a -> string) ->
  of_string:(string -> ('a, [ `Msg of string ]) result) ->
  of_json_string:(string -> ('a, [ `Msg of string ]) result) ->
  string ->
  'a ->
  'a key
(** Same as {!key} for types that don't implement [Type.t] but can be serialized
    with [to_string], and deserialized with either [of_string] or
    [of_json_string]. The [typename] is the user-readable description of the
    type, in case of dynamic type errors. *)

val name : 'a key -> string
(** The key name. *)

val typename : 'a key -> string
(** [typename k] is the type name of [k]'s values. *)

val of_string : 'a key -> string -> ('a, [ `Msg of string ]) result
(** [of_string k] is the parser of [k]'s values. *)

val of_json_string : 'a key -> string -> ('a, [ `Msg of string ]) result
(** [of_json_string k] is the json parser of [k]'s values. *)

val default : 'a key -> 'a
(** [default k] is [k]'s default value. *)

val doc : 'a key -> string option
(** [doc k] is [k]'s documentation string (if any). *)

val docv : 'a key -> string option
(** [docv k] is [k]'s value documentation meta-variable (if any). *)

val docs : 'a key -> string option
(** [docs k] is [k]'s documentation section (if any). *)

val root : Spec.t -> string key
(** Default [--root=ROOT] argument. *)

(** {1:conf Configurations} *)

type t
(** The type for configurations. *)

val pp : t Fmt.t
(** [pp] is the pretty printer for configuration values. *)

val equal : t -> t -> bool
(** [equal] is the equality for configuration values. Two values are equal if
    they have the same [pp] representation. *)

val spec : t -> Spec.t
(** [spec c] is the specification associated with [c] *)

val empty : Spec.t -> t
(** [empty spec] is an empty configuration. *)

val singleton : Spec.t -> 'a key -> 'a -> t
(** [singleton spec k v] is the configuration where [k] maps to [v]. *)

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
(** [keys c] is a sequence of all keys present in [c] *)

val with_spec : t -> Spec.t -> t
(** [with_spec t s] is the config [t] with spec [s] *)

val find_key : t -> string -> 'a Typ.t -> 'a
(** [find_key t name typ] returns the value associated with [name] in the config
    [t]. *)

val verify : t -> t
(** [verify t] is an identity function that ensures all keys match the spec

    {b Raises.} [Invalid_argument] if [t] contains invalid keys *)

(** {1:builtin_converters Built-in value converters} *)

val uri : Uri.t Type.t
(** [uri] converts values with {!Uri.of_string}. *)

val find_root : t -> string option
(** [find_root c] is [root]'s mapping in [c], if any. *)
