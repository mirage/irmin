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

open Irmin_server

module type S = sig
  type t

  module Store : Irmin.Generic_key.S
  module Command : Command.S with module Store = Store

  val readonly : Irmin.config -> Irmin.config

  val v :
    sw:Eio.Switch.t ->
    ?tls_config:[ `Cert_file of string ] * [ `Key_file of string ] ->
    ?dashboard:Conduit_lwt_unix.server ->
    uri:Uri.t ->
    Irmin.config ->
    t Lwt.t
  (** Create an instance of the server *)

  val serve : ?stop:unit Lwt.t -> t -> unit Lwt.t
  (** Run the server *)

  val commands : (string, Command.t) Hashtbl.t
  (** A table mapping commands to command names *)
end

module type Server = sig
  module type S = S

  module Make (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) :
    S with module Store = Store and module Command.Conn.IO = IO
end
