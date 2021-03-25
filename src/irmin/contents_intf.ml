(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open S

module type Gen = sig
  (** {1 Signature for store contents} *)

  type t [@@deriving irmin]
  (** The type for user-defined contents. *)

  type ('a, 'io) merge
  (** The type for merge functions. *)

  val merge : unit -> (t option, 'a) merge
  (** Merge function. Evaluates to [`Conflict msg] if the values cannot be
      merged properly. The arguments of the merge function can take [None] to
      mean that the key does not exists for either the least-common ancestor or
      one of the two merging points. The merge function returns [None] when the
      key's value should be deleted. *)
end

module type M = sig
  include Gen with type ('a, 'io) merge := ('a, 'io) Merge.t
  (** @inline *)
end

module type S = sig
  type 'a merge

  include Gen with type ('a, _) merge := 'a merge
  (** @inline *)
end

module type STORE = sig
  type +'a io
  type 'a merge

  include CONTENT_ADDRESSABLE_STORE with type 'a io := 'a io

  val merge : [> read_write ] t -> key option merge
  (** [merge t] lifts the merge functions defined on contents values to contents
      key. The merge function will: {e (i)} read the values associated with the
      given keys, {e (ii)} use the merge function defined on values and
      {e (iii)} write the resulting values into the store to get the resulting
      key. See {!Contents.S.merge}.

      If any of these operations fail, return [`Conflict]. *)

  (** [Key] provides base functions for user-defined contents keys. *)
  module Key : Hash.TYPED with type t = key and type value = value

  module Val : S with type t = value with type 'a merge := 'a merge
  (** [Val] provides base functions for user-defined contents values. *)
end

module type Contents = sig
  module type S = S
  module type M = M

  module String : M with type t = string
  (** Contents of type [string], with the {{!Irmin.Merge.default} default} 3-way
      merge strategy: assume that update operations are idempotent and conflict
      iff values are modified concurrently. *)

  type json =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `O of (string * json) list
    | `A of json list ]

  module Json : M with type t = (string * json) list
  (** [Json] contents are associations from strings to [json] values stored as
      JSON encoded strings. If the same JSON key has been modified concurrently
      with different values then the [merge] function conflicts. *)

  module Json_value : M with type t = json
  (** [Json_value] allows any kind of json value to be stored, not only objects. *)

  module V1 : sig
    module String : M with type t = string
    (** Same as {!String} but use v1 serialisation format. *)
  end

  module type STORE = STORE
  (** Contents store. *)

  module Make (Merge : Merge.S) (M : M) : S with type 'a merge := 'a Merge.t

  (** [Store] creates a contents store. *)
  module Store
      (Merge : Merge.S) (C : sig
        include S.CONTENT_ADDRESSABLE_STORE with type 'a io := 'a Merge.io
        module Key : Hash.S with type t = key
        module Val : S with type t = value and type 'a merge := 'a Merge.t
      end) :
    STORE
      with type 'a t = 'a C.t
       and type key = C.key
       and type value = C.value
       and type 'a io := 'a Merge.io
       and type 'a merge := 'a Merge.t
end
