(*
 * Copyright (c) 2017 Daniel C. Bünzli
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** The type for configuration converter parsers. *)
type 'a parser = string -> ('a, [ `Msg of string ]) result

(** The type for configuration converter printers. *)
type 'a printer = 'a Fmt.t

(** The type for configuration converters. *)
type 'a converter = 'a parser * 'a printer

(** [parser c] is [c]'s parser. *)
val parser : 'a converter -> 'a parser

(** [converter c] is [c]'s printer. *)
val printer : 'a converter -> 'a printer

(** {1:keys Keys} *)

(** The type for configuration keys whose lookup value is ['a]. *)
type 'a key

(** [key ~docs ~docv ~doc name conv default] is a configuration key named [name]
    that maps to value [default] by default. [conv] is used to convert key
    values provided by end users.

    [docs] is the title of a documentation section under which the key is
    documented. [doc] is a short documentation string for the key, this should
    be a single sentence or paragraph starting with a capital letter and ending
    with a dot. [docv] is a meta-variable for representing the values of the key
    (e.g. ["BOOL"] for a boolean).

    @raise Invalid_argument if the key name is not made of a sequence of ASCII
    lowercase letter, digit, dash or underscore.

    {b Warning.} No two keys should share the same [name] as this may lead to
    difficulties in the UI. *)
val key :
  ?docs:string ->
  ?docv:string ->
  ?doc:string ->
  string ->
  'a converter ->
  'a ->
  'a key

(** The key name. *)
val name : 'a key -> string

(** [tc k] is [k]'s converter. *)
val conv : 'a key -> 'a converter

(** [default k] is [k]'s default value. *)
val default : 'a key -> 'a

(** [doc k] is [k]'s documentation string (if any). *)
val doc : 'a key -> string option

(** [docv k] is [k]'s value documentation meta-variable (if any). *)
val docv : 'a key -> string option

(** [docs k] is [k]'s documentation section (if any). *)
val docs : 'a key -> string option

(** Default [--root=ROOT] argument. *)
val root : string option key

(** {1:conf Configurations} *)

(** The type for configurations. *)
type t

(** [empty] is the empty configuration. *)
val empty : t

(** [singleton k v] is the configuration where [k] maps to [v]. *)
val singleton : 'a key -> 'a -> t

(** [is_empty c] is [true] iff [c] is empty. *)
val is_empty : t -> bool

(** [mem c k] is [true] iff [k] has a mapping in [c]. *)
val mem : t -> 'a key -> bool

(** [add c k v] is [c] with [k] mapping to [v]. *)
val add : t -> 'a key -> 'a -> t

(** [rem c k] is [c] with [k] unbound. *)
val rem : t -> 'a key -> t

(** [union r s] is the union of the configurations [r] and [s]. *)
val union : t -> t -> t

(** [find c k] is [k]'s mapping in [c], if any. *)
val find : t -> 'a key -> 'a option

(** [get c k] is [k]'s mapping in [c].

    {b Raises.} [Not_found] if [k] is not bound in [d]. *)
val get : t -> 'a key -> 'a

(** {1:builtin_converters Built-in value converters} *)

(** [bool] converts values with [bool_of_string]. *)
val bool : bool converter

(** [int] converts values with [int_of_string]. *)
val int : int converter

(** [string] converts values with the identity function. *)
val string : string converter

(** [uri] converts values with {!Uri.of_string}. *)
val uri : Uri.t converter

(** [string] converts values with the identity function. *)
val some : 'a converter -> 'a option converter
