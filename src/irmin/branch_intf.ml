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

module type S = sig
  (** {1 Signature for Branches} *)

  type t [@@deriving irmin]
  (** The type for branches. *)

  val main : t
  (** The name of the main branch. *)

  val is_valid : t -> bool
  (** Check if the branch is valid. *)
end

module Irmin_key = Key

module type Store = sig
  (** {1 Branch Store} *)

  include Atomic_write.S

  module Key : S with type t = key
  (** Base functions on keys. *)

  module Val : Irmin_key.S with type t = value
  (** Base functions on values. *)
end

module type Sigs = sig
  (** {1 Branches} *)

  module type S = S
  (** The signature for branches. Irmin branches are similar to Git branches:
      they are used to associated user-defined names to head commits. Branches
      have a default value: the {{!Branch.S.main} main} branch. *)

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S} S} where branches are
      strings. The [main] branch is ["main"]. Valid branch names contain only
      alpha-numeric characters, [-], [_], [.], and [/]. *)

  module type Store = Store
  (** [Store] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store, where keys
      are branch names created by users and values are keys are head commmits. *)
end
