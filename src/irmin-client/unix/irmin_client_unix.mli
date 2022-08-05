(*
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

module Error = Irmin_client.Error
module IO : Irmin_client.Client.IO

module Info (I : Irmin.Info.S) : sig
  include Irmin.Info.S with type t = I.t

  val init : ?author:string -> ?message:string -> int64 -> t
  val v : ?author:string -> ('b, Format.formatter, unit, f) format4 -> 'b
end

module Make (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

module Make_codec
    (Codec : Irmin_server.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

module Make_json (Store : Irmin.Generic_key.S) :
  Irmin_client.S
    with module Schema = Store.Schema
     and type Backend.Remote.endpoint = unit
     and type commit_key = Store.commit_key
     and type contents_key = Store.contents_key
     and type node_key = Store.node_key

val config : ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config
