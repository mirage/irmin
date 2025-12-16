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

open Cmdliner

let default_uri = Uri.of_string "tcp://127.0.0.1:9181"

let uri : Uri.t option Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Term.(
    const (Option.map Uri.of_string)
    $ Arg.(value & opt (some string) None & doc))

let config_path : string option Cmdliner.Term.t =
  let doc = Arg.info ~docv:"PATH" ~doc:"Config path" [ "config" ] in
  Arg.(value & opt (some string) None & doc)

let codec =
  let doc =
    Arg.info ~docv:"CODEC" ~doc:"Encoding to use for messages" [ "codec" ]
  in
  let t = Arg.enum [ ("bin", `Bin); ("json", `Json) ] in
  Arg.(value & opt t `Bin doc)

module Conf = struct
  let spec = Irmin.Backend.Conf.Spec.v "server"

  module Key = struct
    let uri =
      Irmin.Backend.Conf.key ~spec "uri" Irmin.Backend.Conf.uri default_uri
  end

  let add_uri config u = Irmin.Backend.Conf.add config Key.uri u

  let v config uri =
    let spec' = Irmin.Backend.Conf.spec config in
    let spec = Irmin.Backend.Conf.Spec.join spec' [ spec ] in
    let config = Irmin.Backend.Conf.with_spec config spec in
    match uri with Some uri -> add_uri config uri | None -> config
end
