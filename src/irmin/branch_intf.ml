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

open S

module type S = sig
  (** {1 Signature for Branches} *)

  type t [@@deriving irmin]
  (** The type for branches. *)

  val master : t
  (** The name of the master branch. *)

  val is_valid : t -> bool
  (** Check if the branch is valid. *)
end

module type STORE = sig
  (** {1 Branch Store} *)

  type +'a io

  include ATOMIC_WRITE_STORE with type 'a io := 'a io

  module Key : S with type t = key
  (** Base functions on keys. *)

  module Val : Hash.S with type t = value
  (** Base functions on values. *)
end

module type Branch = sig
  (** {1 Branches} *)

  module type S = S
  (** The signature for branches. Irmin branches are similar to Git branches:
      they are used to associated user-defined names to head commits. Branches
      have a default value: the {{!Branch.S.master} master} branch. *)

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S} S} where branches are
      strings. The [master] branch is ["master"]. Valid branch names contain
      only alpha-numeric characters, [-], [_], [.], and [/]. *)

  module type STORE = STORE
  (** [STORE] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store, where keys
      are branch names created by users and values are keys are head commmits. *)
end
