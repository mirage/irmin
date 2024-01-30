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
module Error = Error
module IO = IO

module Info (I : Irmin.Info.S) = struct
  include I

  let init = v

  let v ?author fmt =
    Fmt.kstr
      (fun message () ->
        let date = Int64.of_float (Unix.gettimeofday ()) in
        init ?author ~message date)
      fmt
end

let normalize_uri ?hostname uri =
  let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
  let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
  let scheme_lower = String.lowercase_ascii scheme in
  let uri =
    if scheme_lower = "tcp" || scheme_lower = "ws" || scheme_lower = "wss" then
      let ip = Unix.gethostbyname addr in
      let port = Uri.port uri |> Option.value ~default:9181 in
      let ip = ip.h_addr_list.(0) |> Unix.string_of_inet_addr in
      Uri.make ~scheme:scheme_lower ~port ~host:ip ()
    else uri
  in
  (uri, Option.value ~default:addr hostname)

let config ?tls ?hostname uri =
  let uri, addr = normalize_uri ?hostname uri in
  Irmin_client.config ?tls ~hostname:(Option.value ~default:addr hostname) uri

module Make_codec (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  include Irmin_client.Make_codec (IO) (Codec) (Store)

  let connect ~sw ?tls ?hostname uri =
    let uri, hostname = normalize_uri ?hostname uri in
    connect ~sw ?tls ~hostname uri
end

module Make (Store : Irmin.Generic_key.S) = Make_codec (Conn.Codec.Bin) (Store)

module Make_json (Store : Irmin.Generic_key.S) =
  Make_codec (Conn.Codec.Json) (Store)
