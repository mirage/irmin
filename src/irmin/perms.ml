(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(** Types representing {i permissions} ['perms] for performing operations on a
    certain type ['perms t].

    They are intended to be used as phantom parameters of the types that they
    control access to. As an example, consider the following type of references
    with permissions:

    {[
      module Ref : sig
        type (+'a, -'perms) t

        val create : 'a -> ('a, read_write) t
        val get : ('a, [> read ]) t -> 'a
        val set : ('a, [> write ]) t -> 'a -> unit
      end
    ]}

    This type allows references to be created with arbitrary read-write access.
    One can then create weaker views onto the reference – with access to fewer
    operations – by upcasting:

    {[
      let read_only t = (t :> (_, read) Ref.t)
      let write_only t = (t :> (_, write) Ref.t)
    ]}

    Note that the ['perms] phantom type parameter should be contravariant: it's
    safe to discard permissions, but not to gain new ones. *)

module Read = struct
  type t = [ `Read ]
end

module Write = struct
  type t = [ `Write ]
end

module Read_write = struct
  type t = [ Read.t | Write.t ]
end

type read = Read.t
(** The type parameter of a handle with [read] permissions. *)

type write = Write.t
(** The type parameter of a handle with [write] permissions. *)

type read_write = Read_write.t
(** The type parameter of a handle with both {!read} and {!write} permissions. *)
