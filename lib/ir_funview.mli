(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2016 Grégoire Henry <gregoire.henry@ocamlpro.com>
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
  include Ir_s.RO_STORE
  val update: t -> key -> value -> t Lwt.t
  val remove: t -> key -> t Lwt.t
  val list      : t -> key -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val empty: t
  val equal: t -> t -> bool
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
  module Private : sig
    type key
    val import: db -> key -> t Lwt.t
    val export: db -> t -> [> `Contents of value | `Empty | `Node of key ] Lwt.t
    module Contents: Tc.S0 with type t = value
  end
end

module Make (S: Ir_s.STORE_EXT):
  S with type db = S.t
     and type key = S.key
     and type value = S.value
     and type Private.key = S.Private.Node.key
