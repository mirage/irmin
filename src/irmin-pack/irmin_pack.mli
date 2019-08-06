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

val config :
  ?fresh:bool ->
  ?shared:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  string ->
  Irmin.config

module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index

exception RO_Not_Allowed

module type CONFIG = sig
  val entries : int

  val stable_hash : int
end

module Make_ext
    (Config : CONFIG)
    (Metadata : Irmin.Metadata.S)
    (Contents : Irmin.Contents.S)
    (Path : Irmin.Path.S)
    (Branch : Irmin.Branch.S)
    (Hash : Irmin.Hash.S)
    (N : Irmin.Private.Node.S
         with type metadata = Metadata.t
          and type hash = Hash.t
          and type step = Path.step)
    (CT : Irmin.Private.Commit.S with type hash = Hash.t) :
  Irmin.S
  with type key = Path.t
   and type contents = Contents.t
   and type branch = Branch.t
   and type hash = Hash.t
   and type step = Path.step
   and type metadata = Metadata.t
   and type Key.step = Path.step

module Make (Config : CONFIG) : Irmin.S_MAKER

module KV (Config : CONFIG) : Irmin.KV_MAKER

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) : sig
  include Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : ?fresh:bool -> ?shared:bool -> ?readonly:bool -> string -> t Lwt.t
end
