(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type CONFIG = sig
  val entries : int
end

module Make
    (Conf : CONFIG)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t)
    (P : Pack.MAKER with type key = H.t) : sig
  module Val : Irmin.Type.S

  module Tree : sig
    type t

    val list : t -> (Node.step * Node.value) list

    val v : (Node.step * Node.value) list -> t

    val load : find:(H.t -> Val.t option Lwt.t) -> H.t -> t option Lwt.t

    val save : add:(H.t -> Val.t -> unit Lwt.t) -> t -> hash:H.t -> H.t Lwt.t
  end

  include Pack.S with type key = H.t and type value = Val.t
end
