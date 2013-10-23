(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include IrminBase.S
  val to_string: t -> string
  val of_string: string -> t
end

module Simple = struct
  include IrminBase.PrivateString
  let name = "tag"
end


module type STORE = sig
  type t
  type key
  type tree
  type revision
  exception Unknown of t
  val update: t -> key -> unit Lwt.t
  val remove: t -> unit Lwt.t
  val read: t -> key option Lwt.t
  val read_exn: t -> key Lwt.t
  val list: unit -> t list Lwt.t
  module Watch: sig
    type watch = int
    type path = string list
    val add: t -> path -> (revision -> path -> tree option -> unit Lwt.t) -> watch Lwt.t
    val remove: t -> path -> watch -> unit Lwt.t
    val list: unit -> (t * path * watch) list Lwt.t
  end
end
