(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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
  include Irmin.Atomic_write.S

  val flush : t -> unit
  val clear_keep_generation : t -> unit Lwt.t
end

module type Persistent = sig
  include S

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t
end

module type Sigs = sig
  module type S = S
  module type Persistent = Persistent

  module Make_persistent
      (_ : Version.S)
      (_ : Irmin.Hash.S)
      (K : Irmin.Type.S)
      (V : Irmin.Hash.S) : Persistent with type key = K.t and type value = V.t

  module Closeable (AW : S) : sig
    include
      S
        with type key = AW.key
         and type value = AW.value
         and type watch = AW.watch

    val make_closeable : AW.t -> t
  end
end
