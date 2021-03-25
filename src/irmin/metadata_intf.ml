(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type Core = sig
  type t [@@deriving irmin]
  (** The type for metadata. *)

  val default : t
  (** The default metadata to attach, for APIs that don't care about metadata. *)
end

module type Gen = sig
  include Core
  (** @inline*)

  type ('a, 'io) merge
  (** The type for merge functions. *)

  val merge : unit -> (t, 'a) merge
  (** [merge] is the merge function for metadata. *)
end

module type S = sig
  type 'a merge

  include Gen with type ('a, _) merge := 'a merge
  (** @include *)
end

module type M = sig
  include Gen with type ('a, 'io) merge := ('a, 'io) Merge.t
  (** @inline *)
end

module type Metadata = sig
  module type Core = Core
  module type S = S
  module type M = M

  module None (Merge : Merge.S) :
    S with type t = unit and type 'a merge := 'a Merge.t

  module Make (Merge : Merge.S) (M : M) :
    S with type t = M.t and type 'a merge := 'a Merge.t
end
